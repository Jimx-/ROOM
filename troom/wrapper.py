"""FPGA host-interface wrapper integrating the troom command interface.

``TroomWrapper`` wires the launch controller, command fetcher, tensor
core, and completion writer behind the public interfaces the FPGA
board uses (``plans/2026-10-09-troom-fpga-host-interface.md``):

* ``axil`` -- an AXI-Lite slave carrying the register ABI. The RDMA
  stack's AXI-Lite master drives it directly; ``mmio_base`` selects
  where the window sits in that master's address space (the RDMA
  router places MMIO at ``0x40000000``).
* ``bus`` -- a single arbitrated AXI memory master. An
  ``AXIArbiter`` merges the fetcher, the core's tensor DMA, and the
  completion writer; the arbiter routes responses by grant and locks
  each direction until its outstanding bursts drain, so the engines
  share one ID space.

Launch, drain, terminal status, and per-run re-arm run through one
FSM:

* ``IDLE`` asserts quiescence. A controller ``start`` pulse leads to
  ``REARM``, which resets the compute core while preserving DRAM and
  the CSR block. ``LAUNCH`` clears the drained completion buffers and pulses the
  fetcher and writer starts with the controller's atomically captured
  configuration.
* ``RUN`` feeds fetched descriptors to the core and completions to
  the writer. Batch success is the core's ``END`` retirement with
  every completion committed (``fetcher.done & core.done &
  writer.drained``); the controller then observes ``done_i`` and
  quiescence together and latches ``DONE`` with ``FINISHED_ID``.
* Any latched run error (fetch failure, decode fault, completion
  write failure, watchdog expiry) raises
  ``ctrl.abort``: submission stops immediately and the FSM enters
  recovery (``DRAIN`` .. ``SETTLE``). Recovery never depends on the
  core finishing -- an unresolved ``WAIT`` is discarded by a local
  core reset -- but the reset is applied only once the wrapper's
  memory master is quiet and all queued tensor DMA has drained, so no AXI transaction is ever discarded mid-flight.
  Ordinary command errors continue through END.
  After the reset, the completion writer is flushed (no further
  records can arrive), drained, and quiescence is reasserted so the
  controller can terminate the failed launch.

Stale completion and error levels from a finished run are gated by
the FSM state: between accept and the engine starts the controller
sees neither ``done_i`` nor error sources, so a new launch cannot be
terminated by the previous run's residue.
"""

from amaranth import *
from amaranth.hdl.xfrm import ResetInserter

from troom.abi import WrapperError
from troom.completion import CompletionWriter
from troom.core import Core
from troom.ctrl import TroomAXILiteBridge, TroomController
from troom.fetch import CommandFetcher
from troom.types import HasTRoomParams

from roomsoc.interconnect.axi import (
    AXIInterface,
    AXILiteInterface,
)
from roomsoc.interconnect.axi.axi_full import AXIArbiter


class TroomWrapper(HasTRoomParams, Elaboratable):
    """Command-interface wrapper: CSRs, fetch, core, completions."""

    def __init__(self,
                 params,
                 *,
                 mem_base=0,
                 mem_size=None,
                 mmio_base=0,
                 reset_cycles=8,
                 name=None):
        HasTRoomParams.__init__(self, params)

        if mem_size is None:
            mem_size = 1 << self.addr_width
        self.mmio_base = mmio_base
        if reset_cycles < 1:
            raise ValueError('reset_cycles must be positive')
        self.reset_cycles = reset_cycles

        # Public interfaces.
        self.axil = AXILiteInterface(data_width=32, addr_width=32)
        self.bus = AXIInterface(data_width=self.data_width,
                                addr_width=self.addr_width,
                                id_width=self.id_width)

        # Command interface components. The memory window bounds both
        # the controller's buffer checks and every tensor DMA issue.
        self.ctrl = TroomController(params,
                                    mem_base=mem_base,
                                    mem_size=mem_size)
        self.fetcher = CommandFetcher(params)
        self.core = Core(params, mem_base=mem_base, mem_size=mem_size)
        self.writer = CompletionWriter(params)

        # Observation: the compute-core re-arm reset.
        self.core_reset = Signal()

    def elaborate(self, platform):
        m = Module()

        ctrl = m.submodules.ctrl = self.ctrl
        fetcher = m.submodules.fetcher = self.fetcher
        core_rst = Signal()
        m.submodules.core = ResetInserter(core_rst)(self.core)
        writer = m.submodules.writer = self.writer

        m.submodules.axil_adapt = TroomAXILiteBridge(self.axil,
                                                     ctrl,
                                                     base_addr=self.mmio_base)

        # One memory master: fetch reads, tensor DMA, completion
        # writes arbitrated onto the public port.
        arb = m.submodules.arb = AXIArbiter(addr_width=self.addr_width,
                                            data_width=self.data_width,
                                            id_width=self.id_width)
        arb.add(fetcher.bus)
        arb.add(self.core.bus)
        arb.add(writer.bus)
        m.d.comb += arb.bus.connect(self.bus)

        # Engine datapaths.
        m.d.comb += [
            fetcher.cmd.connect(self.core.cmd),
            self.core.resp.connect(writer.sink),
        ]

        # Launch configuration from the controller's atomic capture.
        m.d.comb += [
            fetcher.cmd_base.eq(ctrl.active_cmd_base),
            fetcher.cmd_count.eq(ctrl.active_cmd_count),
            writer.comp_base.eq(ctrl.active_comp_base),
            writer.comp_count.eq(ctrl.active_cmd_count),
        ]

        #
        # Outstanding-burst tracking on the public master. The
        # recovery reset is applied only once both counters are
        # empty, so a transaction is never discarded mid-flight.
        #
        rd_out = Signal(range(2 * self.dma_max_outstanding + 1))
        wr_out = Signal(range(2 * self.dma_max_outstanding + 1))
        ar_fire = self.bus.ar.valid & self.bus.ar.ready
        r_last = self.bus.r.valid & self.bus.r.ready & self.bus.r.bits.last
        aw_fire = self.bus.aw.valid & self.bus.aw.ready
        b_fire = self.bus.b.valid & self.bus.b.ready
        with m.If(ar_fire & ~r_last):
            m.d.sync += rd_out.eq(rd_out + 1)
        with m.Elif(~ar_fire & r_last):
            m.d.sync += rd_out.eq(rd_out - 1)
        with m.If(aw_fire & ~b_fire):
            m.d.sync += wr_out.eq(wr_out + 1)
        with m.Elif(~aw_fire & b_fire):
            m.d.sync += wr_out.eq(wr_out - 1)
        bus_quiet = ((rd_out == 0) & (wr_out == 0)
                     & ~self.bus.ar.valid & ~self.bus.aw.valid
                     & ~self.bus.w.valid)

        #
        # Launch, drain, and recovery sequencing.
        #
        rst_cnt = Signal(range(self.reset_cycles))
        success = fetcher.done & self.core.done & writer.drained

        with m.FSM() as fsm:
            with m.State('IDLE'):
                with m.If(ctrl.start):
                    m.d.sync += rst_cnt.eq(0)
                    m.next = 'REARM'

            with m.State('REARM'):
                # Compute-core reset only, after quiescence: DRAM,
                # the CSR block, and the fetch/write helpers are
                # preserved; the engine starts below re-initialize
                # their run state.
                m.d.comb += core_rst.eq(1)
                m.d.sync += rst_cnt.eq(rst_cnt + 1)
                with m.If(rst_cnt + 1 == self.reset_cycles):
                    m.next = 'LAUNCH'

            with m.State('LAUNCH'):
                m.d.comb += [fetcher.start.eq(1), writer.start.eq(1)]
                m.next = 'RUN'

            with m.State('RUN'):
                m.d.comb += fetcher.abort.eq(ctrl.abort)
                with m.If(ctrl.abort):
                    m.next = 'DRAIN'
                with m.Elif(success):
                    m.next = 'IDLE'

            with m.State('DRAIN'):
                # Stop engine issue and drain queued DMA as well as public
                # bus transactions. A gap between bursts is not idle;
                # an unresolved WAIT does not prevent memory drain.
                m.d.comb += fetcher.abort.eq(1)
                with m.If(bus_quiet & ~fetcher.active & self.core.memory_idle):
                    m.d.sync += rst_cnt.eq(0)
                    m.next = 'RECOVER'

            with m.State('RECOVER'):
                # Local core recovery: discard the stalled sequencer
                # (and any residual command prefetch). Safe because
                # the bus is quiet.
                m.d.comb += core_rst.eq(1)
                m.d.sync += rst_cnt.eq(rst_cnt + 1)
                with m.If(rst_cnt + 1 == self.reset_cycles):
                    m.next = 'FLUSH'

            with m.State('FLUSH'):
                # No further records can arrive; flush and drain the
                # completion log.
                m.d.comb += writer.flush.eq(1)
                with m.If(writer.idle):
                    m.next = 'SETTLE'

            with m.State('SETTLE'):
                with m.If(bus_quiet):
                    m.next = 'IDLE'

        # Quiescence is tracked separately from logical completion:
        # it holds in IDLE and in the single cycle where a successful
        # batch presents done_i. Stale component levels are gated by
        # the FSM so a fresh launch cannot be terminated by residue
        # of the previous one.
        engines_live = ~(fsm.ongoing('IDLE') | fsm.ongoing('REARM')
                         | fsm.ongoing('LAUNCH'))
        m.d.comb += [
            self.core.halt.eq(ctrl.abort),
            ctrl.busy_i.eq(~fsm.ongoing('IDLE')),
            ctrl.done_i.eq(fsm.ongoing('RUN') & success),
            ctrl.quiescent_i.eq(
                fsm.ongoing('IDLE')
                | (fsm.ongoing('RUN') & success)),
            ctrl.fetch_err.eq(engines_live & fetcher.err),
            ctrl.fetch_err_code.eq(fetcher.err_code),
            ctrl.fetch_err_index.eq(fetcher.err_index),
            ctrl.comp_err.eq(engines_live & (writer.err | writer.fatal)),
            ctrl.comp_err_code.eq(
                Mux(writer.fatal, WrapperError.COMP_ERROR, writer.err_code)),
            ctrl.comp_err_index.eq(writer.err_index),
            ctrl.completed_i.eq(writer.completed),
            ctrl.counters_i.cmds_done.eq(self.core.cmds_done),
            ctrl.counters_i.run_cycles.eq(self.core.cycle_count),
            ctrl.counters_i.dram_bytes.eq(self.core.dram_bytes),
            ctrl.counters_i.dma_stalls.eq(self.core.dma_stalls),
            ctrl.counters_i.tmem_conflicts.eq(self.core.tmem_conflicts),
            ctrl.counters_i.useful_macs.eq(self.core.useful_macs),
            ctrl.counters_i.array_starve.eq(self.core.array_starve),
            ctrl.counters_i.mm_credit_stalls.eq(self.core.mm_credit_stalls),
            ctrl.counters_i.mm_run_cycles.eq(self.core.mm_run_cycles),
            ctrl.counters_i.mm_drain_cycles.eq(self.core.mm_drain_cycles),
            ctrl.counters_i.epi_records.eq(self.core.epi_records),
            ctrl.counters_i.epi_active_cycles.eq(self.core.epi_active_cycles),
            ctrl.counters_i.epi_stalls.eq(self.core.epi_stalls),
            ctrl.counters_i.epi_cast_ops.eq(self.core.epi_cast_ops),
            ctrl.counters_i.epi_mul_ops.eq(self.core.epi_mul_ops),
            ctrl.counters_i.epi_add_ops.eq(self.core.epi_add_ops),
            self.core_reset.eq(core_rst),
        ]

        return m
