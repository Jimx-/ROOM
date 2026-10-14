"""Host launch controller for the troom FPGA wrapper.

``TroomController`` follows the GROOM CSR pattern (``groom/ctrl.py``): a
``roomsoc.peripheral.Peripheral`` whose register bank is exposed through
a Wishbone bridge, attached to the host's AXI-Lite master through
``TroomAXILiteBridge``::

    ctrl = TroomController(params)
    axil = AXILiteInterface(data_width=32,
                            addr_width=32)
    m.submodules.ctrl = ctrl
    m.submodules.adapt = TroomAXILiteBridge(axil, ctrl)

The register map, status/error encodings, and the format-0 descriptor
serialization are frozen in ``troom.abi``; this module only implements
behavior:

* configuration writes affect staging registers only. ``START``
  (write-one to ``CONTROL``) validates the staged launch and, on
  success, atomically captures it into the active launch registers,
  clears terminal status, updates ``ACCEPTED_ID``, and pulses
  ``start`` with the captured configuration held on the ``active_*``
  outputs;
* a start while a launch is active or while the wrapper is not
  quiescent is rejected: ``STATUS.REJECTED`` latches, and neither the
  active launch's identity nor its results are modified;
* a configuration fault (unsupported format, zero count, misalignment,
  range or capacity violations, nonzero high address words, buffer
  overlap) rejects the launch and terminates it immediately with the
  reason in ``ERROR_CODE`` and ``FINISHED_ID`` set to the rejected
  ``SUBMIT_ID``;
* the first error event of an active run (command fetch failure,
  completion-write failure, failed command, or watchdog expiry) is
  latched sticky with its origin and index; a run terminates only once
  the wrapper reports quiescence, so ``DONE`` means the core's ``END``
  retired, every completion write was acknowledged, and traffic has
  drained;
* the engine observation counters on ``counters_i`` are frozen into
  the counter-window registers once per launch: at termination, or at
  the first fatal error cycle.

The controller is generic over the rest of the wrapper: launch progress
(``busy_i``), successful batch completion (``done_i``), and bus/core
quiescence (``quiescent_i``) are inputs, and error events arrive as
levels from the command fetcher and completion writer plus the internal
watchdog. Wiring them to ``troom.Core``, ``troom.fetch``, and
``troom.completion`` is the wrapper's job.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.utils import log2_int

from troom.abi import (
    CAPS_ENGINE_COUNTERS,
    CAPS_FMT0,
    CAPS_MM_EPI_COUNTERS,
    CAPS_WATCHDOG,
    CMD_DESC_BYTES,
    CMD_FORMAT_0,
    COMP_RECORD_BYTES,
    CONTROL_CLEAR,
    CONTROL_START,
    ERROR_INDEX_NONE,
    REG_ABI_VERSION,
    REG_ACCEPTED_ID,
    REG_ARRAY_STARVE,
    REG_BEAT_BYTES,
    REG_CAPS,
    REG_CMD_ALIGN,
    REG_CMD_BASE_HI,
    REG_CMD_BASE_LO,
    REG_CMD_COUNT,
    REG_CMD_FORMAT,
    REG_CMDS_DONE,
    REG_COMP_ALIGN,
    REG_COMP_BASE_HI,
    REG_COMP_BASE_LO,
    REG_COMP_CAPACITY,
    REG_COMPLETED_COUNT,
    REG_CONTROL,
    REG_CYCLES,
    REG_DMA_STALLS,
    REG_DRAM_BYTES,
    REG_ERROR_CODE,
    REG_ERROR_INDEX,
    REG_FINISHED_ID,
    REG_ID,
    REG_MAX_CYCLES,
    REG_MEM_BASE,
    REG_MEM_SIZE,
    REG_MM_CREDIT_STALLS,
    REG_MM_DRAIN_CYCLES,
    REG_MM_RUN_CYCLES,
    REG_EPI_ACTIVE_CYCLES,
    REG_EPI_ADD_OPS,
    REG_EPI_CAST_OPS,
    REG_EPI_MUL_OPS,
    REG_EPI_RECORDS,
    REG_EPI_STALLS,
    REG_RUN_CYCLES,
    REG_STATUS,
    REG_SUBMIT_ID,
    REG_TMEM_BYTES,
    REG_TMEM_CONFLICTS,
    REG_USEFUL_MACS,
    STATUS_BUSY,
    STATUS_DONE,
    STATUS_ERROR,
    STATUS_QUIESCENT,
    STATUS_REJECTED,
    TROOM_ABI_VERSION,
    TROOM_ID,
    WrapperError,
)
from troom.types import HasTRoomParams

from roomsoc.peripheral import Peripheral


class TroomController(HasTRoomParams, Peripheral, Elaboratable):
    """CSR launch controller implementing the frozen register ABI.

    Parameters
    ----------
    params : dict
        Validated troom parameters; the memory beat size, TMEM size,
        and address width feed the capability registers and the
        alignment/range checks.
    mem_base, mem_size : int
        Device memory window the command and completion buffers must
        fall inside. Defaults admit the whole 32-bit space.
    """

    def __init__(self, params, *, mem_base=0, mem_size=None, name=None):
        HasTRoomParams.__init__(self, params)
        Peripheral.__init__(self, name=name)

        if mem_size is None:
            mem_size = 1 << self.addr_width
        if mem_base < 0 or mem_size <= 0:
            raise ValueError('memory window must be non-empty')
        if mem_base + mem_size > 1 << self.addr_width:
            raise ValueError(
                f'memory window [{mem_base:#x}, {mem_base + mem_size:#x}) '
                f'exceeds the {self.addr_width}-bit address space')
        self.mem_base = mem_base
        self.mem_size = mem_size

        # Required buffer alignments, advertised through the capability
        # registers and enforced at start.
        self.cmd_align = self.beat_bytes
        self.comp_align = max(self.beat_bytes, COMP_RECORD_BYTES)

        #
        # Wrapper-facing launch interface.
        #
        self.start = Signal()
        self.abort = Signal()

        self.active_submit_id = Signal(32)
        self.active_cmd_base = Signal(self.addr_width)
        self.active_cmd_count = Signal(32)
        self.active_comp_base = Signal(self.addr_width)
        self.active_max_cycles = Signal(64)

        # Run progress from the wrapper.
        self.busy_i = Signal()
        self.done_i = Signal()
        self.quiescent_i = Signal()

        # Sticky error sources; levels with stable payloads so that
        # simultaneous failures cannot lose an event.
        self.fetch_err = Signal()
        self.fetch_err_code = Signal(WrapperError)
        self.fetch_err_index = Signal(32)
        self.comp_err = Signal()
        self.comp_err_code = Signal(WrapperError)
        self.comp_err_index = Signal(32)

        # Completion records committed through AXI B responses.
        self.completed_i = Signal(32)

        # Live engine observation counters from the compute core; the
        # controller freezes them into the snapshot registers when the
        # launch terminates. Leave unwired to read all-zero counters.
        self.counters_i = Record([
            ('cmds_done', 32, Direction.FANOUT),
            ('run_cycles', 48, Direction.FANOUT),
            ('dram_bytes', 48, Direction.FANOUT),
            ('dma_stalls', 48, Direction.FANOUT),
            ('tmem_conflicts', 48, Direction.FANOUT),
            ('useful_macs', 48, Direction.FANOUT),
            ('array_starve', 48, Direction.FANOUT),
            ('mm_credit_stalls', 48, Direction.FANOUT),
            ('mm_run_cycles', 48, Direction.FANOUT),
            ('mm_drain_cycles', 48, Direction.FANOUT),
            ('epi_records', 48, Direction.FANOUT),
            ('epi_active_cycles', 48, Direction.FANOUT),
            ('epi_stalls', 48, Direction.FANOUT),
            ('epi_cast_ops', 48, Direction.FANOUT),
            ('epi_mul_ops', 48, Direction.FANOUT),
            ('epi_add_ops', 48, Direction.FANOUT),
        ])

        bank = self.csr_bank()
        self._id = bank.csr(32, 'r', addr=REG_ID, name='id')
        self._abi_version = bank.csr(32,
                                     'r',
                                     addr=REG_ABI_VERSION,
                                     name='abi_version')
        self._caps = bank.csr(32, 'r', addr=REG_CAPS, name='caps')
        self._status = bank.csr(32, 'r', addr=REG_STATUS, name='status')
        self._control = bank.csr(32, 'w', addr=REG_CONTROL, name='control')
        self._submit_id = bank.csr(32,
                                   'rw',
                                   addr=REG_SUBMIT_ID,
                                   name='submit_id')

        self._cmd_base_lo = bank.csr(32,
                                     'rw',
                                     addr=REG_CMD_BASE_LO,
                                     name='cmd_base_lo')
        self._cmd_base_hi = bank.csr(32,
                                     'rw',
                                     addr=REG_CMD_BASE_HI,
                                     name='cmd_base_hi')
        self._cmd_count = bank.csr(32,
                                   'rw',
                                   addr=REG_CMD_COUNT,
                                   name='cmd_count')
        self._cmd_format = bank.csr(32,
                                    'rw',
                                    addr=REG_CMD_FORMAT,
                                    name='cmd_format')

        self._comp_base_lo = bank.csr(32,
                                      'rw',
                                      addr=REG_COMP_BASE_LO,
                                      name='comp_base_lo')
        self._comp_base_hi = bank.csr(32,
                                      'rw',
                                      addr=REG_COMP_BASE_HI,
                                      name='comp_base_hi')
        self._comp_capacity = bank.csr(32,
                                       'rw',
                                       addr=REG_COMP_CAPACITY,
                                       name='comp_capacity')

        self._completed = bank.csr(32,
                                   'r',
                                   addr=REG_COMPLETED_COUNT,
                                   name='completed_count')
        self._finished_id = bank.csr(32,
                                     'r',
                                     addr=REG_FINISHED_ID,
                                     name='finished_id')
        self._error_code = bank.csr(32,
                                    'r',
                                    addr=REG_ERROR_CODE,
                                    name='error_code')
        self._error_index = bank.csr(32,
                                     'r',
                                     addr=REG_ERROR_INDEX,
                                     name='error_index')
        self._accepted_id = bank.csr(32,
                                     'r',
                                     addr=REG_ACCEPTED_ID,
                                     name='accepted_id')

        self._max_cycles = bank.csr(64,
                                    'rw',
                                    addr=REG_MAX_CYCLES,
                                    name='max_cycles')

        self._mem_base = bank.csr(64, 'r', addr=REG_MEM_BASE, name='mem_base')
        self._mem_size = bank.csr(64, 'r', addr=REG_MEM_SIZE, name='mem_size')
        self._beat_bytes = bank.csr(32,
                                    'r',
                                    addr=REG_BEAT_BYTES,
                                    name='beat_bytes')
        self._tmem_bytes = bank.csr(32,
                                    'r',
                                    addr=REG_TMEM_BYTES,
                                    name='tmem_bytes')
        self._cmd_align = bank.csr(32,
                                   'r',
                                   addr=REG_CMD_ALIGN,
                                   name='cmd_align')
        self._comp_align = bank.csr(32,
                                    'r',
                                    addr=REG_COMP_ALIGN,
                                    name='comp_align')
        self._cycles = bank.csr(64, 'r', addr=REG_CYCLES, name='cycles')

        self._snap_cmds_done = bank.csr(64,
                                        'r',
                                        addr=REG_CMDS_DONE,
                                        name='cmds_done')
        self._snap_run_cycles = bank.csr(64,
                                         'r',
                                         addr=REG_RUN_CYCLES,
                                         name='run_cycles')
        self._snap_dram_bytes = bank.csr(64,
                                         'r',
                                         addr=REG_DRAM_BYTES,
                                         name='dram_bytes')
        self._snap_dma_stalls = bank.csr(64,
                                         'r',
                                         addr=REG_DMA_STALLS,
                                         name='dma_stalls')
        self._snap_tmem_conflicts = bank.csr(64,
                                             'r',
                                             addr=REG_TMEM_CONFLICTS,
                                             name='tmem_conflicts')
        self._snap_useful_macs = bank.csr(64,
                                          'r',
                                          addr=REG_USEFUL_MACS,
                                          name='useful_macs')
        self._snap_array_starve = bank.csr(64,
                                           'r',
                                           addr=REG_ARRAY_STARVE,
                                           name='array_starve')

        # M3b cycle-breakdown snapshot window (see troom/abi.py for the
        # per-counter units).
        self._snap_mm_credit = bank.csr(64,
                                        'r',
                                        addr=REG_MM_CREDIT_STALLS,
                                        name='mm_credit_stalls')
        self._snap_mm_run = bank.csr(64,
                                     'r',
                                     addr=REG_MM_RUN_CYCLES,
                                     name='mm_run_cycles')
        self._snap_mm_drain = bank.csr(64,
                                       'r',
                                       addr=REG_MM_DRAIN_CYCLES,
                                       name='mm_drain_cycles')
        self._snap_epi_records = bank.csr(64,
                                          'r',
                                          addr=REG_EPI_RECORDS,
                                          name='epi_records')
        self._snap_epi_active = bank.csr(64,
                                         'r',
                                         addr=REG_EPI_ACTIVE_CYCLES,
                                         name='epi_active_cycles')
        self._snap_epi_stalls = bank.csr(64,
                                         'r',
                                         addr=REG_EPI_STALLS,
                                         name='epi_stalls')
        self._snap_epi_cast = bank.csr(64,
                                       'r',
                                       addr=REG_EPI_CAST_OPS,
                                       name='epi_cast_ops')
        self._snap_epi_mul = bank.csr(64,
                                      'r',
                                      addr=REG_EPI_MUL_OPS,
                                      name='epi_mul_ops')
        self._snap_epi_add = bank.csr(64,
                                      'r',
                                      addr=REG_EPI_ADD_OPS,
                                      name='epi_add_ops')

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        #
        # Identity, capabilities, and parameters.
        #
        m.d.comb += [
            self._id.r_data.eq(TROOM_ID),
            self._abi_version.r_data.eq(TROOM_ABI_VERSION),
            self._caps.r_data.eq((1 << CAPS_FMT0) | (1 << CAPS_WATCHDOG)
                                 | (1 << CAPS_ENGINE_COUNTERS)
                                 | (1 << CAPS_MM_EPI_COUNTERS)),
            self._mem_base.r_data.eq(self.mem_base),
            self._mem_size.r_data.eq(self.mem_size),
            self._beat_bytes.r_data.eq(self.beat_bytes),
            self._tmem_bytes.r_data.eq(self.tmem_bytes),
            self._cmd_align.r_data.eq(self.cmd_align),
            self._comp_align.r_data.eq(self.comp_align),
        ]

        cycle_cnt = Signal(64)
        m.d.sync += cycle_cnt.eq(cycle_cnt + 1)
        m.d.comb += self._cycles.r_data.eq(cycle_cnt)

        #
        # Staging registers. Writes land in the shadow and commit when
        # the CSR machinery completes the register; reads have no side
        # effects.
        #
        staging = [
            self._submit_id,
            self._cmd_base_lo,
            self._cmd_base_hi,
            self._cmd_count,
            self._cmd_format,
            self._comp_base_lo,
            self._comp_base_hi,
            self._comp_capacity,
            self._max_cycles,
        ]
        for elem in staging:
            with m.If(elem.w_stb):
                m.d.sync += elem.r_data.eq(elem.w_data)

        #
        # Launch state.
        #
        run_active = Signal()
        done_sticky = Signal()
        error_sticky = Signal()
        rejected_sticky = Signal()
        err_code = Signal(32, reset=WrapperError.NONE)
        err_index = Signal(32, reset=ERROR_INDEX_NONE)
        accepted_id = Signal(32)
        finished_id = Signal(32)
        wd_count = Signal(64)
        fatal_sticky = Signal()
        snapped = Signal()

        m.d.comb += [
            self._status.r_data.eq(
                Cat(
                    run_active,
                    done_sticky,
                    error_sticky,
                    self.quiescent_i,
                    rejected_sticky,
                )),
            self._completed.r_data.eq(self.completed_i),
            self._error_code.r_data.eq(err_code),
            self._error_index.r_data.eq(err_index),
            self._accepted_id.r_data.eq(accepted_id),
            self._finished_id.r_data.eq(finished_id),
        ]

        #
        # Staged-configuration validation. Checked only at START, in a
        # fixed priority order so the reported reason is deterministic.
        #
        count = self._cmd_count.r_data
        capacity = self._comp_capacity.r_data
        cmd_base = Cat(self._cmd_base_lo.r_data, self._cmd_base_hi.r_data)
        comp_base = Cat(self._comp_base_lo.r_data, self._comp_base_hi.r_data)

        lg_cmd = log2_int(self.cmd_align)
        lg_comp = log2_int(self.comp_align)

        # Guarded arithmetic: the byte lengths carry enough extra bits
        # that no intermediate can wrap.
        cmd_bytes = Signal(40)
        comp_bytes = Signal(36)
        m.d.comb += [
            cmd_bytes.eq(((count * CMD_DESC_BYTES + self.beat_bytes -
                           1) >> lg_cmd) << lg_cmd),
            comp_bytes.eq(((capacity * COMP_RECORD_BYTES + self.beat_bytes -
                            1) >> lg_cmd) << lg_cmd),
        ]
        cmd_end = Signal(65)
        comp_end = Signal(65)
        m.d.comb += [
            cmd_end.eq(cmd_base + cmd_bytes),
            comp_end.eq(comp_base + comp_bytes),
        ]

        mem_end = self.mem_base + self.mem_size

        checks = [
            (self._cmd_format.r_data != CMD_FORMAT_0, WrapperError.BAD_FORMAT),
            (count == 0, WrapperError.BAD_COUNT),
            ((self._cmd_base_hi.r_data != 0)
             | (self._comp_base_hi.r_data != 0), WrapperError.ADDR_OVERFLOW),
            (cmd_base[:lg_cmd].any() | comp_base[:lg_comp].any(),
             WrapperError.BAD_ALIGN),
            ((cmd_base < self.mem_base) | (cmd_end > mem_end)
             | (comp_base < self.mem_base) | (comp_end > mem_end),
             WrapperError.BAD_RANGE),
            (capacity < count, WrapperError.BAD_CAPACITY),
            (~((cmd_end <= comp_base) | (comp_end <= cmd_base)),
             WrapperError.BUF_OVERLAP),
        ]

        # First match wins: the earliest check in priority order forms
        # the outermost Mux.
        rej_any = Const(0)
        rej_code = Const(WrapperError.NONE)
        for bad, code in reversed(checks):
            rej_code = Mux(bad, code, rej_code)
            rej_any = rej_any | bad

        #
        # Error events. The fetcher and writer hold sticky levels with
        # stable payloads; fetch faults win simultaneous contention.
        # The watchdog is internal and only armed by a nonzero
        # MAX_CYCLES.
        #
        wd_en = run_active & (self.active_max_cycles != 0) & ~fatal_sticky
        wd_expired = wd_en & (wd_count == self.active_max_cycles - 1)
        with m.If(wd_en & ~wd_expired):
            m.d.sync += wd_count.eq(wd_count + 1)

        err_hit = self.fetch_err | self.comp_err | wd_expired
        err_code_sel = Mux(
            self.fetch_err, self.fetch_err_code,
            Mux(self.comp_err, self.comp_err_code, WrapperError.WATCHDOG))
        err_index_sel = Mux(
            self.fetch_err, self.fetch_err_index,
            Mux(self.comp_err, self.comp_err_index, ERROR_INDEX_NONE))

        fatal_hit = self.fetch_err | wd_expired | (
            self.comp_err & (self.comp_err_code != WrapperError.CMD_FAILED))
        with m.If(run_active & fatal_hit):
            m.d.sync += fatal_sticky.eq(1)

        with m.If(run_active):
            with m.If(~error_sticky & err_hit):
                m.d.sync += [
                    error_sticky.eq(1),
                    err_code.eq(err_code_sel),
                    err_index.eq(err_index_sel),
                ]

        #
        # CONTROL actions. The CSR machinery completes a register on
        # the write of its highest byte lane, and the action bits live
        # in that byte: a committed write always carries freshly
        # written action bits, and a partial write excluding the last
        # lane commits nothing at all.
        #
        m.d.sync += self.start.eq(0)
        with m.If(self._control.w_stb):
            with m.If(self._control.w_data[CONTROL_START]):
                with m.If(run_active | ~self.quiescent_i):
                    # Busy rejection: the active run keeps its identity
                    # and results untouched.
                    m.d.sync += rejected_sticky.eq(1)
                with m.Elif(rej_any):
                    # Configuration fault: the launch terminates
                    # immediately with the reason readable through the
                    # terminal-status protocol.
                    m.d.sync += [
                        rejected_sticky.eq(1),
                        done_sticky.eq(0),
                        error_sticky.eq(1),
                        err_code.eq(rej_code),
                        err_index.eq(ERROR_INDEX_NONE),
                        finished_id.eq(self._submit_id.r_data),
                    ]
                with m.Else():
                    # Accepted: capture the staged configuration
                    # atomically and start.
                    m.d.sync += [
                        run_active.eq(1),
                        fatal_sticky.eq(0),
                        rejected_sticky.eq(0),
                        error_sticky.eq(0),
                        done_sticky.eq(0),
                        err_code.eq(WrapperError.NONE),
                        err_index.eq(ERROR_INDEX_NONE),
                        snapped.eq(0),
                        accepted_id.eq(self._submit_id.r_data),
                        self.active_submit_id.eq(self._submit_id.r_data),
                        self.active_cmd_base.eq(self._cmd_base_lo.r_data),
                        self.active_cmd_count.eq(count),
                        self.active_comp_base.eq(self._comp_base_lo.r_data),
                        self.active_max_cycles.eq(self._max_cycles.r_data),
                        wd_count.eq(0),
                        self.start.eq(1),
                    ]
            with m.Elif(self._control.w_data[CONTROL_CLEAR]):
                with m.If(~run_active):
                    m.d.sync += [
                        done_sticky.eq(0),
                        error_sticky.eq(0),
                        rejected_sticky.eq(0),
                        err_code.eq(WrapperError.NONE),
                        err_index.eq(ERROR_INDEX_NONE),
                    ]

        #
        # Terminal transition. A run finishes only once the wrapper is
        # quiescent; DONE is exclusive with a latched error.
        #
        finish = run_active & self.quiescent_i \
            & (self.done_i | fatal_sticky)
        with m.If(finish):
            m.d.sync += [
                run_active.eq(0),
                finished_id.eq(self.active_submit_id),
                done_sticky.eq(self.done_i & ~error_sticky & ~err_hit),
            ]

        #
        # Engine-counter snapshot. The core resets its counters at the
        # next launch's re-arm, so the terminal values are frozen here
        # once per launch: at termination, or at the first fatal error
        # cycle while the counters still hold the work done so far.
        # Reads between launches are therefore stable.
        #
        with m.If(run_active & ~snapped & (finish | fatal_hit)):
            m.d.sync += [
                snapped.eq(1),
                self._snap_cmds_done.r_data.eq(self.counters_i.cmds_done),
                self._snap_run_cycles.r_data.eq(self.counters_i.run_cycles),
                self._snap_dram_bytes.r_data.eq(self.counters_i.dram_bytes),
                self._snap_dma_stalls.r_data.eq(self.counters_i.dma_stalls),
                self._snap_tmem_conflicts.r_data.eq(
                    self.counters_i.tmem_conflicts),
                self._snap_useful_macs.r_data.eq(self.counters_i.useful_macs),
                self._snap_array_starve.r_data.eq(
                    self.counters_i.array_starve),
                self._snap_mm_credit.r_data.eq(
                    self.counters_i.mm_credit_stalls),
                self._snap_mm_run.r_data.eq(self.counters_i.mm_run_cycles),
                self._snap_mm_drain.r_data.eq(self.counters_i.mm_drain_cycles),
                self._snap_epi_records.r_data.eq(self.counters_i.epi_records),
                self._snap_epi_active.r_data.eq(
                    self.counters_i.epi_active_cycles),
                self._snap_epi_stalls.r_data.eq(self.counters_i.epi_stalls),
                self._snap_epi_cast.r_data.eq(self.counters_i.epi_cast_ops),
                self._snap_epi_mul.r_data.eq(self.counters_i.epi_mul_ops),
                self._snap_epi_add.r_data.eq(self.counters_i.epi_add_ops),
            ]

        m.d.comb += self.abort.eq(run_active & fatal_sticky)

        return m


class TroomAXILiteBridge(Elaboratable):
    """Checked AXI-Lite adapter for the TROOM register window.

    AW and W are buffered independently. Unaligned, unmapped, and
    access-direction violations return SLVERR without reaching the CSR
    bridge. Full host addresses are checked before narrowing to Wishbone.
    """

    def __init__(self, axil, ctrl, *, base_addr=0):
        if (base_addr < 0 or base_addr % 4
                or base_addr + REG_EPI_ADD_OPS + 8 > 2**32):
            raise ValueError('MMIO window must be aligned and fit in 32 bits')
        self.axil = axil
        self.ctrl = ctrl
        self.base_addr = base_addr

    def elaborate(self, platform):
        m = Module()
        axil, wb = self.axil, self.ctrl.bus
        aw_pending, w_pending, ar_pending = Signal(), Signal(), Signal()
        aw_addr, ar_addr, w_data = Signal(32), Signal(32), Signal(32)
        w_strb = Signal(4)
        b_valid, r_valid = Signal(), Signal()
        b_resp, r_resp = Signal(2), Signal(2)
        r_data = Signal(32)
        m.d.comb += [
            axil.aw.ready.eq(~aw_pending & ~b_valid),
            axil.w.ready.eq(~w_pending & ~b_valid),
            axil.ar.ready.eq(~ar_pending & ~r_valid),
            axil.b.valid.eq(b_valid),
            axil.b.resp.eq(b_resp),
            axil.r.valid.eq(r_valid),
            axil.r.resp.eq(r_resp),
            axil.r.data.eq(r_data),
        ]
        with m.If(axil.aw.valid & axil.aw.ready):
            m.d.sync += [aw_pending.eq(1), aw_addr.eq(axil.aw.addr)]
        with m.If(axil.w.valid & axil.w.ready):
            m.d.sync += [
                w_pending.eq(1),
                w_data.eq(axil.w.data),
                w_strb.eq(axil.w.strb)
            ]
        with m.If(axil.ar.valid & axil.ar.ready):
            m.d.sync += [ar_pending.eq(1), ar_addr.eq(axil.ar.addr)]
        with m.If(axil.b.valid & axil.b.ready):
            m.d.sync += b_valid.eq(0)
        with m.If(axil.r.valid & axil.r.ready):
            m.d.sync += r_valid.eq(0)

        writable = (REG_CONTROL, REG_SUBMIT_ID, REG_CMD_BASE_LO,
                    REG_CMD_BASE_HI, REG_CMD_COUNT, REG_CMD_FORMAT,
                    REG_COMP_BASE_LO, REG_COMP_BASE_HI, REG_COMP_CAPACITY,
                    REG_MAX_CYCLES, REG_MAX_CYCLES + 4)
        readable = tuple(
            o for o in range(REG_MAX_CYCLES + 8)
            if o % 4 == 0 and o != REG_CONTROL) + tuple(
                range(REG_MEM_BASE, REG_CYCLES + 8, 4)) + tuple(
                    range(REG_CMDS_DONE, REG_ARRAY_STARVE + 8, 4)) + tuple(
                        range(REG_MM_CREDIT_STALLS, REG_EPI_ADD_OPS + 8, 4))

        def mapped(addr, offsets):
            result = Const(0)
            for offset in offsets:
                result = result | (addr == self.base_addr + offset)
            return result

        # Merge byte strobes per host word before traversing the shared
        # byte-wide CSR shadow. Otherwise partial writes can pick up bytes
        # belonging to an unrelated register's previous transaction.
        shadows = {
            offset: Signal(32, name=f"staging_{offset:02x}")
            for offset in writable if offset != REG_CONTROL
        }
        merged = Signal(32)
        m.d.comb += merged.eq(w_data)
        for offset, shadow in shadows.items():
            with m.If(aw_addr == self.base_addr + offset):
                for lane in range(4):
                    m.d.comb += merged.word_select(lane, 8).eq(
                        Mux(w_strb[lane], w_data.word_select(lane, 8),
                            shadow.word_select(lane, 8)))

        write_ok = mapped(aw_addr, writable)
        read_ok = mapped(ar_addr, readable)
        with m.FSM():
            with m.State('IDLE'):
                with m.If(aw_pending & w_pending):
                    with m.If(write_ok):
                        with m.If(w_strb == 0):
                            m.d.sync += [
                                aw_pending.eq(0),
                                w_pending.eq(0),
                                b_valid.eq(1),
                                b_resp.eq(0)
                            ]
                        with m.Elif(aw_addr == (self.base_addr +
                                                REG_MAX_CYCLES + 4)):
                            m.next = 'WRITE_LOW'
                        with m.Else():
                            m.next = 'WRITE'
                    with m.Else():
                        m.d.sync += [
                            aw_pending.eq(0),
                            w_pending.eq(0),
                            b_valid.eq(1),
                            b_resp.eq(2)
                        ]
                with m.Elif(ar_pending):
                    with m.If(read_ok):
                        m.next = 'READ'
                    with m.Else():
                        m.d.sync += [
                            ar_pending.eq(0),
                            r_valid.eq(1),
                            r_resp.eq(2),
                            r_data.eq(0)
                        ]
            with m.State('WRITE_LOW'):
                # Restore the low half before the committing high half;
                # unrelated CSR writes may have reused its shadow bytes.
                m.d.comb += [
                    wb.cyc.eq(1),
                    wb.stb.eq(1),
                    wb.we.eq(1),
                    wb.adr.eq(REG_MAX_CYCLES >> 2),
                    wb.dat_w.eq(shadows[REG_MAX_CYCLES]),
                    wb.sel.eq(0xf)
                ]
                with m.If(wb.ack):
                    m.next = 'WRITE_GAP'
            with m.State('WRITE_GAP'):
                m.next = 'WRITE'
            with m.State('WRITE'):
                m.d.comb += [
                    wb.cyc.eq(1),
                    wb.stb.eq(1),
                    wb.we.eq(1),
                    wb.adr.eq((aw_addr - self.base_addr) >> 2),
                    wb.dat_w.eq(merged),
                    wb.sel.eq(
                        Mux(aw_addr == self.base_addr + REG_CONTROL, w_strb,
                            0xf))
                ]
                with m.If(wb.ack):
                    for offset, shadow in shadows.items():
                        with m.If(aw_addr == self.base_addr + offset):
                            m.d.sync += shadow.eq(merged)
                    m.d.sync += [
                        aw_pending.eq(0),
                        w_pending.eq(0),
                        b_valid.eq(1),
                        b_resp.eq(0)
                    ]
                    m.next = 'IDLE'
            with m.State('READ'):
                m.d.comb += [
                    wb.cyc.eq(1),
                    wb.stb.eq(1),
                    wb.adr.eq((ar_addr - self.base_addr) >> 2),
                    wb.sel.eq(0xf)
                ]
                with m.If(wb.ack):
                    m.d.sync += [
                        ar_pending.eq(0),
                        r_valid.eq(1),
                        r_resp.eq(0),
                        r_data.eq(wb.dat_r)
                    ]
                    m.next = 'IDLE'
        return m
