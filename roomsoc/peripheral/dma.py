from amaranth import *
from amaranth.lib.fifo import SyncFIFO
from amaranth.utils import log2_int
from amaranth.hdl.rec import Direction

from .peripheral import Peripheral
from roomsoc.interconnect.axi import AXIInterface
from roomsoc.interconnect.axi.common import AXIBurst
from roomsoc.interconnect.stream import Decoupled, Queue


def _validate_axi_dma_config(version, burst_type, max_burst_beats,
                             max_outstanding):
    if version not in ('axi3', 'axi4'):
        raise ValueError("version must be 'axi3' or 'axi4'")
    if burst_type not in ('FIXED', 'INCR', 'WRAP'):
        raise ValueError("invalid burst_type {}".format(burst_type))
    beat_max = 256 if (version == 'axi4' and burst_type == 'INCR') else 16
    if not (1 <= max_burst_beats <= beat_max):
        raise ValueError(
            "max_burst_beats {} out of range for {} {} (1..{})".format(
                max_burst_beats, version, burst_type, beat_max))
    if max_outstanding < 1:
        raise ValueError("max_outstanding must be >= 1")
    if burst_type == 'WRAP' and max_burst_beats not in (2, 4, 8, 16):
        raise ValueError("WRAP max_burst_beats must be 2, 4, 8, or 16")


class WishboneDMAReader(Peripheral, Elaboratable):

    def __init__(self,
                 bus,
                 *,
                 name=None,
                 fifo_depth=16,
                 with_csr=False,
                 default_base=0,
                 default_length=0,
                 default_enable=0,
                 default_loop=0):
        super().__init__(name=name)

        self.wb_bus = bus
        self.fifo_depth = fifo_depth
        self.with_csr = with_csr

        if with_csr:
            self.default_base = default_base
            self.default_length = default_length
            self.default_enable = default_enable
            self.default_loop = default_loop

            bank = self.csr_bank()
            self._base = bank.csr(64, 'rw')
            self._length = bank.csr(32, 'rw')
            self._enable = bank.csr(1, 'rw')
            self._loop = bank.csr(1, 'rw')
            self._done = bank.csr(1, 'r')
            self._offset = bank.csr(32, 'r')

            self._bridge = self.bridge(data_width=32,
                                       granularity=8,
                                       alignment=2)
            self.bus = self._bridge.bus

        self.sink = Decoupled(Record, [("address", bus.addr_width),
                                       ("last", 1)])
        self.source = Decoupled(Record,
                                [("data", bus.data_width, Direction.FANOUT),
                                 ("last", 1, Direction.FANOUT)])

    def elaborate(self, platform):
        m = Module()

        if self.with_csr:
            m.submodules.bridge = self._bridge

        fifo = m.submodules.fifo = SyncFIFO(depth=self.fifo_depth,
                                            width=self.wb_bus.data_width + 1)

        m.d.comb += [
            self.wb_bus.stb.eq(self.sink.valid & fifo.w_rdy),
            self.wb_bus.cyc.eq(self.sink.valid & fifo.w_rdy),
            self.wb_bus.we.eq(0),
            self.wb_bus.sel.eq(2**(self.wb_bus.data_width // 8) - 1),
            self.wb_bus.adr.eq(self.sink.bits.address),
            fifo.w_data.eq(Cat(self.wb_bus.dat_r, self.sink.bits.last)),
        ]

        with m.If(self.wb_bus.stb & self.wb_bus.ack):
            m.d.comb += [
                self.sink.ready.eq(1),
                fifo.w_en.eq(1),
            ]

        m.d.comb += [
            self.source.bits.eq(fifo.r_data),
            self.source.valid.eq(fifo.r_rdy),
            fifo.r_en.eq(self.source.ready),
        ]

        if self.with_csr:
            base = Signal(self.wb_bus.addr_width)
            length = Signal(self.wb_bus.addr_width)
            offset = Signal(32)

            self._base.r_data.reset = self.default_base
            self._length.r_data.reset = self.default_length
            self._enable.r_data.reset = self.default_enable
            self._loop.r_data.reset = self.default_loop

            with m.If(self._base.w_stb):
                m.d.sync += self._base.r_data.eq(self._base.w_data)
            with m.If(self._length.w_stb):
                m.d.sync += self._length.r_data.eq(self._length.w_data)
            with m.If(self._enable.w_stb):
                m.d.sync += self._enable.r_data.eq(self._enable.w_data)
            with m.If(self._loop.w_stb):
                m.d.sync += self._loop.r_data.eq(self._loop.w_data)

            shift = log2_int(self.wb_bus.data_width // 8)
            m.d.comb += [
                base.eq(self._base.r_data[shift:]),
                length.eq(self._length.r_data[shift:]),
                self._offset.r_data.eq(Cat(Const(0, shift), offset)),
            ]

            with m.FSM():
                with m.State('IDLE'):
                    m.d.sync += offset.eq(0)
                    with m.If(self._enable.r_data):
                        m.next = 'RUN'

                with m.State('RUN'):
                    with m.If(~self._enable.r_data):
                        m.next = 'IDLE'
                    with m.Else():
                        m.d.comb += [
                            self.sink.valid.eq(1),
                            self.sink.bits.last.eq(offset == (length - 1)),
                            self.sink.bits.address.eq(base + offset),
                        ]

                        with m.If(self.sink.ready):
                            m.d.sync += offset.eq(offset + 1)
                            with m.If(self.sink.bits.last):
                                with m.If(self._loop.r_data):
                                    m.d.sync += offset.eq(0)
                                with m.Else():
                                    m.next = 'IDLE'

        return m


class WishboneDMAWriter(Peripheral, Elaboratable):

    def __init__(self,
                 bus,
                 *,
                 name=None,
                 with_csr=False,
                 default_base=0,
                 default_length=0,
                 default_enable=0,
                 default_loop=0):
        super().__init__(name=name)

        self.wb_bus = bus
        self.with_csr = with_csr

        self._sink = Decoupled(Record, [("address", bus.addr_width),
                                        ("data", bus.data_width), ("last", 1)])
        self.sink = self._sink

        if with_csr:
            self.default_base = default_base
            self.default_length = default_length
            self.default_enable = default_enable
            self.default_loop = default_loop

            bank = self.csr_bank()
            self._base = bank.csr(64, 'rw')
            self._length = bank.csr(32, 'rw')
            self._enable = bank.csr(1, 'rw')
            self._loop = bank.csr(1, 'rw')
            self._done = bank.csr(1, 'r')
            self._offset = bank.csr(32, 'r')

            self._bridge = self.bridge(data_width=32,
                                       granularity=8,
                                       alignment=2)
            self.bus = self._bridge.bus

            self.sink = Decoupled(Record, [("data", bus.data_width),
                                           ("last", 1)])

    def elaborate(self, platform):
        m = Module()

        if self.with_csr:
            m.submodules.bridge = self._bridge

        m.d.comb += [
            self.wb_bus.stb.eq(self._sink.valid),
            self.wb_bus.cyc.eq(self._sink.valid),
            self.wb_bus.we.eq(1),
            self.wb_bus.sel.eq(2**(self.wb_bus.data_width // 8) - 1),
            self.wb_bus.adr.eq(self._sink.bits.address),
            self.wb_bus.dat_w.eq(self._sink.bits.data),
            self._sink.ready.eq(self.wb_bus.ack),
        ]

        if self.with_csr:
            base = Signal(self.wb_bus.addr_width)
            length = Signal(self.wb_bus.addr_width)
            offset = Signal(32)

            self._base.r_data.reset = self.default_base
            self._length.r_data.reset = self.default_length
            self._enable.r_data.reset = self.default_enable
            self._loop.r_data.reset = self.default_loop

            with m.If(self._base.w_stb):
                m.d.sync += self._base.r_data.eq(self._base.w_data)
            with m.If(self._length.w_stb):
                m.d.sync += self._length.r_data.eq(self._length.w_data)
            with m.If(self._enable.w_stb):
                m.d.sync += self._enable.r_data.eq(self._enable.w_data)
            with m.If(self._loop.w_stb):
                m.d.sync += self._loop.r_data.eq(self._loop.w_data)

            shift = log2_int(self.wb_bus.data_width // 8)
            m.d.comb += [
                base.eq(self._base.r_data[shift:]),
                length.eq(self._length.r_data[shift:]),
                self._offset.r_data.eq(Cat(Const(0, shift), offset)),
            ]

            with m.FSM():
                with m.State('IDLE'):
                    m.d.sync += offset.eq(0)
                    with m.If(self._enable.r_data):
                        m.next = 'RUN'

                with m.State('RUN'):
                    with m.If(~self._enable.r_data):
                        m.next = 'IDLE'
                    with m.Else():
                        m.d.comb += [
                            self._sink.valid.eq(self.sink.valid),
                            self._sink.bits.last.eq(self.sink.bits.last
                                                    | (offset == (length -
                                                                  1))),
                            self._sink.bits.address.eq(base + offset),
                            self._sink.bits.data.eq(self.sink.bits.data),
                            self.sink.ready.eq(self._sink.ready),
                        ]

                        with m.If(self.sink.fire):
                            m.d.sync += offset.eq(offset + 1)
                            with m.If(self.sink.bits.last):
                                with m.If(self._loop.r_data):
                                    m.d.sync += offset.eq(0)
                                with m.Else():
                                    m.next = 'IDLE'

        return m


class AXIDMAReader(Elaboratable):
    """AXI read DMA master with multiple outstanding bursts.

    Interface contract:

    * ``sink`` is a Decoupled descriptor stream. ``sink.addr`` is a byte
      address and ``sink.len`` is a non-zero byte count. A full-width command
      requires both fields to be aligned to ``data_width / 8``. The only
      supported narrow command is exactly one half-width beat, aligned to
      ``data_width / 16``. A descriptor transfers only when ``sink.valid`` and
      ``sink.ready`` are both asserted.
    * ``source`` is a Decoupled payload stream with one entry per AXI R beat.
      ``source.data`` preserves AXI byte-lane placement; it is the unshifted
      RDATA value. For example, a 32-bit transfer at address ``...4`` on a
      64-bit bus occupies ``source.data[63:32]``. Unrequested lanes are not
      meaningful. ``source.last`` marks the final beat of the descriptor, not
      each AXI burst. Holding ``source.ready`` low applies backpressure to R.
    * An invalid descriptor is consumed without issuing AXI and pulses both
      ``done`` and ``error`` for one cycle. For a valid descriptor, ``done``
      pulses when the final ``source`` beat is transferred. A non-OKAY RRESP
      pulses ``error`` when that R beat is transferred.

    Valid commands are split into legal AXI bursts, clipped to
    ``max_burst_beats`` and, for INCR, to 4 KiB boundaries. Up to
    ``max_outstanding`` bursts may be in flight. All requests use one AXI ID,
    so responses must be returned in order.
    """

    def __init__(self,
                 bus=None,
                 *,
                 addr_width=32,
                 data_width=32,
                 id_width=1,
                 version='axi4',
                 max_burst_beats=16,
                 max_outstanding=8,
                 cmd_fifo_depth=16,
                 data_fifo_depth=32,
                 burst_type='INCR',
                 prot=0,
                 cache=0b0011,
                 qos=0):
        _validate_axi_dma_config(version, burst_type, max_burst_beats,
                                 max_outstanding)
        if bus is None:
            bus = AXIInterface(addr_width=addr_width,
                               data_width=data_width,
                               id_width=id_width,
                               version=version)
        self.bus = bus
        self.addr_width = bus.addr_width
        self.data_width = bus.data_width
        self.id_width = bus.id_width
        self.version = version
        self.max_burst_beats = max_burst_beats
        self.max_outstanding = max_outstanding
        self.cmd_fifo_depth = cmd_fifo_depth
        self.data_fifo_depth = data_fifo_depth
        self.burst_type = burst_type
        self.prot = prot
        self.cache = cache
        self.qos = qos

        if hasattr(bus, 'version') and bus.version != version:
            raise ValueError("bus version does not match DMA version")

        self.beat_bytes = self.data_width // 8
        self.lg_bytes = log2_int(self.beat_bytes)

        # Public descriptor and payload streams; see the class contract above.
        self.sink = Decoupled(Record,
                              [("addr", self.addr_width, Direction.FANOUT),
                               ("len", 32, Direction.FANOUT)])
        self.source = Decoupled(Record,
                                [("data", self.data_width, Direction.FANOUT),
                                 ("last", 1, Direction.FANOUT)])
        self.done = Signal()
        self.error = Signal()

    def elaborate(self, platform):
        m = Module()
        bus = self.bus
        lg = self.lg_bytes

        burst_enc = {
            'FIXED': AXIBurst.FIXED,
            'INCR': AXIBurst.INCR,
            'WRAP': AXIBurst.WRAP,
        }[self.burst_type]

        cmd_q = m.submodules.cmd_q = Queue(
            self.cmd_fifo_depth, Record,
            [("addr", self.addr_width, Direction.FANOUT),
             ("len", 32, Direction.FANOUT)])
        m.d.comb += self.sink.connect(cmd_q.enq)

        data_q = m.submodules.data_q = Queue(
            self.data_fifo_depth, Record,
            [("data", self.data_width, Direction.FANOUT),
             ("last", 1, Direction.FANOUT)])
        m.d.comb += data_q.deq.connect(self.source)

        # One entry per in-flight burst recording whether it completes a
        # command; popped on the burst's last R beat to reconstruct per-command
        # ``last`` on the source.
        meta_q = m.submodules.meta_q = Queue(self.max_outstanding, Signal, 1)

        outstanding = Signal(range(self.max_outstanding + 1))
        has_room = outstanding < self.max_outstanding

        cur_addr = Signal(self.addr_width)
        remaining = Signal(32)
        transfer_lg = Signal(range(lg + 1), reset=lg)
        in_burst = Signal()
        invalid_cmd = Signal()
        m.d.sync += invalid_cmd.eq(0)

        b1 = Mux(remaining < self.max_burst_beats, remaining,
                 self.max_burst_beats)
        if self.burst_type == 'WRAP':
            burst_beats = Mux(b1 >= 16, 16, Mux(b1 >= 8, 8, Mux(b1 >= 4, 4,
                                                                2)))
        else:
            burst_beats = b1
        if self.burst_type == 'INCR':
            to_4kb = (0x1000 - (cur_addr & 0xfff)) >> transfer_lg
            burst_beats = Mux(b1 < to_4kb, b1, to_4kb)

        ar_valid = in_burst & has_room & meta_q.enq.ready

        m.d.comb += [
            bus.ar.valid.eq(ar_valid),
            bus.ar.bits.addr.eq(cur_addr),
            bus.ar.bits.len.eq(burst_beats - 1),
            bus.ar.bits.size.eq(transfer_lg),
            bus.ar.bits.burst.eq(burst_enc),
            bus.ar.bits.lock.eq(0),
            bus.ar.bits.prot.eq(self.prot),
            bus.ar.bits.cache.eq(self.cache),
            bus.ar.bits.qos.eq(self.qos),
            bus.ar.bits.region.eq(0),
            bus.ar.bits.id.eq(0),
        ]

        ar_fire = ar_valid & bus.ar.ready

        with m.FSM():
            with m.State('IDLE'):
                with m.If(cmd_q.deq.valid):
                    narrow = cmd_q.deq.bits.len == self.beat_bytes // 2
                    narrow_valid = narrow
                    if lg > 1:
                        narrow_valid = narrow_valid & (
                            cmd_q.deq.bits.addr[:lg - 1] == 0)
                    beats = Mux(narrow_valid, 1, cmd_q.deq.bits.len[lg:])
                    invalid = cmd_q.deq.bits.len == 0
                    if lg:
                        full_invalid = ((cmd_q.deq.bits.addr[:lg] != 0)
                                        | (cmd_q.deq.bits.len[:lg] != 0))
                        invalid = invalid | (full_invalid & ~narrow_valid)
                    if self.burst_type == 'WRAP':
                        invalid = invalid | beats[0]
                    m.d.comb += cmd_q.deq.ready.eq(1)
                    m.d.sync += [
                        cur_addr.eq(cmd_q.deq.bits.addr),
                        remaining.eq(beats),
                        transfer_lg.eq(Mux(narrow_valid, lg - 1, lg)),
                    ]
                    with m.If(~invalid):
                        m.d.sync += in_burst.eq(1)
                        m.next = 'BURST'
                    with m.Else():
                        m.d.sync += invalid_cmd.eq(1)

            with m.State('BURST'):
                with m.If(ar_fire):
                    m.d.comb += [
                        meta_q.enq.valid.eq(1),
                        meta_q.enq.bits.eq(remaining == burst_beats),
                    ]
                    m.d.sync += [
                        cur_addr.eq(cur_addr + (burst_beats << transfer_lg)),
                        remaining.eq(remaining - burst_beats),
                    ]
                    with m.If(remaining == burst_beats):
                        m.d.sync += in_burst.eq(0)
                        m.next = 'IDLE'

        r_last_fire = bus.r.valid & bus.r.ready & bus.r.bits.last

        with m.If(ar_fire & ~r_last_fire):
            m.d.sync += outstanding.eq(outstanding + 1)
        with m.Elif(~ar_fire & r_last_fire):
            m.d.sync += outstanding.eq(outstanding - 1)

        m.d.comb += [
            bus.r.ready.eq(data_q.enq.ready
                           & (~bus.r.bits.last | meta_q.deq.valid)),
            data_q.enq.valid.eq(bus.r.valid & bus.r.ready),
            data_q.enq.bits.data.eq(bus.r.bits.data),
            data_q.enq.bits.last.eq(bus.r.bits.last & meta_q.deq.bits),
            meta_q.deq.ready.eq(bus.r.valid & bus.r.ready & bus.r.bits.last),
            self.done.eq(invalid_cmd
                         | (self.source.fire & self.source.bits.last)),
            self.error.eq(invalid_cmd
                          | (bus.r.valid & bus.r.ready
                             & (bus.r.bits.resp != 0))),
        ]

        return m


class AXIDMAWriter(Elaboratable):
    """AXI write DMA master with multiple outstanding bursts.

    Interface contract:

    * ``sink`` follows the same Decoupled descriptor rules as
      :class:`AXIDMAReader`: byte address and non-zero byte length, either
      naturally aligned full-width beats or exactly one naturally aligned
      half-width beat.
    * ``data`` is a Decoupled payload stream ordered to match accepted
      descriptors. It supplies exactly ``sink.len / (data_width / 8)`` beats
      for a full-width descriptor and one beat for a half-width descriptor.
      ``data.data`` and ``data.strb`` must already use AXI byte-lane placement;
      they are forwarded unchanged to WDATA and WSTRB. Thus a 32-bit transfer
      at address ``...4`` on a 64-bit bus places its value in ``data[63:32]``
      with ``strb=0xf0``; a full-width beat normally uses ``strb=0xff``.
      There is no input ``last`` signal because the descriptor defines the
      transfer length; the DMA generates WLAST at every AXI burst boundary.
    * An invalid descriptor is consumed without consuming ``data`` or issuing
      AXI, and pulses both ``done`` and ``error``. For a valid descriptor,
      ``done`` pulses when its final B response is transferred. A non-OKAY
      BRESP pulses ``error`` when that response is transferred.

    Valid commands are split into legal AXI bursts, clipped to
    ``max_burst_beats`` and, for INCR, to 4 KiB boundaries. Up to
    ``max_outstanding`` bursts may be in flight. AW, W, and B ordering follows
    accepted descriptor order and all requests use one AXI ID.
    """

    def __init__(self,
                 bus=None,
                 *,
                 addr_width=32,
                 data_width=32,
                 id_width=1,
                 version='axi4',
                 max_burst_beats=16,
                 max_outstanding=8,
                 cmd_fifo_depth=16,
                 burst_type='INCR',
                 prot=0,
                 cache=0b0011,
                 qos=0):
        _validate_axi_dma_config(version, burst_type, max_burst_beats,
                                 max_outstanding)
        if bus is None:
            bus = AXIInterface(addr_width=addr_width,
                               data_width=data_width,
                               id_width=id_width,
                               version=version)
        self.bus = bus
        self.addr_width = bus.addr_width
        self.data_width = bus.data_width
        self.id_width = bus.id_width
        self.version = version
        self.max_burst_beats = max_burst_beats
        self.max_outstanding = max_outstanding
        self.cmd_fifo_depth = cmd_fifo_depth
        self.burst_type = burst_type
        self.prot = prot
        self.cache = cache
        self.qos = qos

        if hasattr(bus, 'version') and bus.version != version:
            raise ValueError("bus version does not match DMA version")

        self.beat_bytes = self.data_width // 8
        self.lg_bytes = log2_int(self.beat_bytes)

        # Width needed to hold any per-burst beat count (1..max_burst_beats).
        self.beats_width = max(1, self.max_burst_beats.bit_length())

        # Public descriptor and payload streams; see the class contract above.
        self.sink = Decoupled(Record,
                              [("addr", self.addr_width, Direction.FANOUT),
                               ("len", 32, Direction.FANOUT)])
        self.data = Decoupled(
            Record, [("data", self.data_width, Direction.FANOUT),
                     ("strb", self.data_width // 8, Direction.FANOUT)])
        self.done = Signal()
        self.error = Signal()

    def elaborate(self, platform):
        m = Module()
        bus = self.bus
        lg = self.lg_bytes

        burst_enc = {
            'FIXED': AXIBurst.FIXED,
            'INCR': AXIBurst.INCR,
            'WRAP': AXIBurst.WRAP,
        }[self.burst_type]

        cmd_q = m.submodules.cmd_q = Queue(
            self.cmd_fifo_depth, Record,
            [("addr", self.addr_width, Direction.FANOUT),
             ("len", 32, Direction.FANOUT)])
        m.d.comb += self.sink.connect(cmd_q.enq)

        # One entry per in-flight burst. ``wmeta_q`` carries the number of W
        # beats to stream for that burst (popped on its last W beat so we know
        # where to assert W.last); ``bmeta_q`` carries whether the burst
        # completes a command (popped on its B response to reconstruct
        # per-command completion on ``done``). Both are pushed together when AW
        # fires, so their occupancy never exceeds ``outstanding``.
        wmeta_q = m.submodules.wmeta_q = Queue(self.max_outstanding, Signal,
                                               self.beats_width)
        bmeta_q = m.submodules.bmeta_q = Queue(self.max_outstanding, Signal, 1)

        outstanding = Signal(range(self.max_outstanding + 1))
        has_room = outstanding < self.max_outstanding

        cur_addr = Signal(self.addr_width)
        remaining = Signal(32)
        transfer_lg = Signal(range(lg + 1), reset=lg)
        in_burst = Signal()
        invalid_cmd = Signal()
        m.d.sync += invalid_cmd.eq(0)

        b1 = Mux(remaining < self.max_burst_beats, remaining,
                 self.max_burst_beats)
        if self.burst_type == 'WRAP':
            burst_beats = Mux(b1 >= 16, 16, Mux(b1 >= 8, 8, Mux(b1 >= 4, 4,
                                                                2)))
        else:
            burst_beats = b1
        if self.burst_type == 'INCR':
            to_4kb = (0x1000 - (cur_addr & 0xfff)) >> transfer_lg
            burst_beats = Mux(b1 < to_4kb, b1, to_4kb)

        aw_valid = in_burst & has_room & wmeta_q.enq.ready & bmeta_q.enq.ready

        m.d.comb += [
            bus.aw.valid.eq(aw_valid),
            bus.aw.bits.addr.eq(cur_addr),
            bus.aw.bits.len.eq(burst_beats - 1),
            bus.aw.bits.size.eq(transfer_lg),
            bus.aw.bits.burst.eq(burst_enc),
            bus.aw.bits.lock.eq(0),
            bus.aw.bits.prot.eq(self.prot),
            bus.aw.bits.cache.eq(self.cache),
            bus.aw.bits.qos.eq(self.qos),
            bus.aw.bits.region.eq(0),
            bus.aw.bits.id.eq(0),
        ]
        if self.version == 'axi3':
            m.d.comb += bus.w.bits.id.eq(0)

        aw_fire = aw_valid & bus.aw.ready

        with m.FSM():
            with m.State('IDLE'):
                with m.If(cmd_q.deq.valid):
                    narrow = cmd_q.deq.bits.len == self.beat_bytes // 2
                    narrow_valid = narrow
                    if lg > 1:
                        narrow_valid = narrow_valid & (
                            cmd_q.deq.bits.addr[:lg - 1] == 0)
                    beats = Mux(narrow_valid, 1, cmd_q.deq.bits.len[lg:])
                    invalid = cmd_q.deq.bits.len == 0
                    if lg:
                        full_invalid = ((cmd_q.deq.bits.addr[:lg] != 0)
                                        | (cmd_q.deq.bits.len[:lg] != 0))
                        invalid = invalid | (full_invalid & ~narrow_valid)
                    if self.burst_type == 'WRAP':
                        invalid = invalid | beats[0]
                    m.d.comb += cmd_q.deq.ready.eq(1)
                    m.d.sync += [
                        cur_addr.eq(cmd_q.deq.bits.addr),
                        remaining.eq(beats),
                        transfer_lg.eq(Mux(narrow_valid, lg - 1, lg)),
                    ]
                    with m.If(~invalid):
                        m.d.sync += in_burst.eq(1)
                        m.next = 'BURST'
                    with m.Else():
                        m.d.sync += invalid_cmd.eq(1)

            with m.State('BURST'):
                with m.If(aw_fire):
                    m.d.comb += [
                        wmeta_q.enq.valid.eq(1),
                        wmeta_q.enq.bits.eq(burst_beats),
                        bmeta_q.enq.valid.eq(1),
                        bmeta_q.enq.bits.eq(remaining == burst_beats),
                    ]
                    m.d.sync += [
                        cur_addr.eq(cur_addr + (burst_beats << transfer_lg)),
                        remaining.eq(remaining - burst_beats),
                    ]
                    with m.If(remaining == burst_beats):
                        m.d.sync += in_burst.eq(0)
                        m.next = 'IDLE'

        b_fire = bus.b.valid & bus.b.ready

        with m.If(aw_fire & ~b_fire):
            m.d.sync += outstanding.eq(outstanding + 1)
        with m.Elif(~aw_fire & b_fire):
            m.d.sync += outstanding.eq(outstanding - 1)

        # Write data path: for each burst recorded in ``wmeta_q`` stream exactly
        # that many beats from ``data`` to the W channel, asserting W.last on
        # the final beat of every burst.
        wbeats = Signal(self.beats_width)
        m.d.comb += bus.w.bits.strb.eq(self.data.bits.strb)

        with m.FSM():
            with m.State('WIDLE'):
                with m.If(wmeta_q.deq.valid):
                    m.d.comb += wmeta_q.deq.ready.eq(1)
                    m.d.sync += wbeats.eq(wmeta_q.deq.bits)
                    m.next = 'WSTREAM'

            with m.State('WSTREAM'):
                w_fire = self.data.valid & bus.w.ready
                m.d.comb += [
                    bus.w.valid.eq(self.data.valid),
                    bus.w.bits.data.eq(self.data.bits.data),
                    bus.w.bits.last.eq(wbeats == 1),
                    self.data.ready.eq(bus.w.ready),
                ]
                with m.If(w_fire):
                    m.d.sync += wbeats.eq(wbeats - 1)
                    with m.If(wbeats == 1):
                        m.next = 'WIDLE'

        # Write response path: accept B in order and pulse ``done`` on the
        # final response of each command.
        m.d.comb += [
            bus.b.ready.eq(bmeta_q.deq.valid),
            bmeta_q.deq.ready.eq(b_fire),
            self.done.eq(invalid_cmd | (b_fire & bmeta_q.deq.bits)),
            self.error.eq(invalid_cmd | (b_fire & (bus.b.bits.resp != 0))),
        ]

        return m
