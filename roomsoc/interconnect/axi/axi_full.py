from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT
from amaranth.lib.fifo import SyncFIFO
from amaranth.utils import log2_int
from amaranth_soc.memory import MemoryMap

from .axi_lite import AXILiteInterface, Wishbone2AXILite, AXILite2Wishbone
from .common import *
from roomsoc.interconnect.stream import SkidBuffer, Decoupled, Queue


def make_ax_layout(addr_width=32, id_width=1, version='axi4', user_width=0):
    len_width = {'axi3': 4, 'axi4': 8}[version]
    size_width = {'axi3': 4, 'axi4': 3}[version]
    lock_width = {'axi3': 2, 'axi4': 1}[version]

    layout = [
        ('addr', addr_width, DIR_FANOUT),
        ('burst', AXIBurst, DIR_FANOUT),
        ('len', len_width, DIR_FANOUT),
        ('size', size_width, DIR_FANOUT),
        ('lock', lock_width, DIR_FANOUT),
        ('prot', 3, DIR_FANOUT),
        ('cache', 4, DIR_FANOUT),
        ('qos', 4, DIR_FANOUT),
        ('region', 4, DIR_FANOUT),
        ('id', id_width, DIR_FANOUT),
        ('user', user_width, DIR_FANOUT),
    ]

    return layout


def make_axi_layout(data_width=32,
                    addr_width=32,
                    id_width=1,
                    version='axi4',
                    aw_user_width=0,
                    w_user_width=0,
                    b_user_width=0,
                    ar_user_width=0,
                    r_user_width=0):
    wstrb_width = data_width // 8

    aw_layout = make_ax_layout(addr_width, id_width, version, aw_user_width)
    ar_layout = make_ax_layout(addr_width, id_width, version, ar_user_width)

    w_layout = [
        ("data", data_width, DIR_FANOUT),  # write data
        ("strb", wstrb_width, DIR_FANOUT),  # write strobes
        ("user", w_user_width, DIR_FANOUT),
        ("last", 1, DIR_FANOUT),
    ]
    if version == 'axi3':
        w_layout.append(('id', id_width, DIR_FANOUT))

    b_layout = [
        ("resp", AXIResp, DIR_FANIN),  # write response
        ("id", id_width, DIR_FANIN),
        ("user", b_user_width, DIR_FANIN),
    ]

    r_layout = [
        ("data", data_width, DIR_FANIN),  # read data
        ("id", id_width, DIR_FANIN),
        ("user", r_user_width, DIR_FANIN),
        ("last", 1, DIR_FANIN),
        ("resp", AXIResp, DIR_FANIN),  # read response
    ]

    return [
        # write address channel signals
        ("aw", [
            ("bits", aw_layout),
            ("valid", 1, DIR_FANOUT),
            ("ready", 1, DIR_FANIN),
        ]),
        # write data channel signals
        (
            "w",
            [
                ("bits", w_layout),
                ("valid", 1, DIR_FANOUT),  # write valid
                ("ready", 1, DIR_FANIN),  # write ready
            ]),
        # write response channel signals
        (
            "b",
            [
                ("bits", b_layout),
                ("valid", 1, DIR_FANIN),  # write response valid
                ("ready", 1, DIR_FANOUT),  # response ready
            ]),
        # read address channel signals
        ("ar", [
            ("bits", ar_layout),
            ("valid", 1, DIR_FANOUT),
            ("ready", 1, DIR_FANIN),
        ]),
        # read data channel signals
        (
            "r",
            [
                ("bits", r_layout),
                ("valid", 1, DIR_FANIN),  # read valid
                ("ready", 1, DIR_FANOUT),  # read ready
            ]),
    ]


class AXIInterface(Record):

    def __init__(self,
                 data_width=32,
                 addr_width=32,
                 id_width=1,
                 name=None,
                 src_loc_at=1):
        self.addr_width = addr_width
        self.data_width = data_width
        self.id_width = id_width
        self._map = None

        super().__init__(make_axi_layout(data_width=data_width,
                                         addr_width=addr_width,
                                         id_width=id_width),
                         name=name,
                         src_loc_at=src_loc_at)

    @property
    def memory_map(self):
        if self._map is None:
            raise NotImplementedError(
                "Bus interface {!r} does not have a memory map".format(self))
        return self._map

    @memory_map.setter
    def memory_map(self, memory_map):
        if not isinstance(memory_map, MemoryMap):
            raise TypeError(
                "Memory map must be an instance of MemoryMap, not {!r}".format(
                    memory_map))
        if memory_map.addr_width != max(1, self.addr_width):
            raise ValueError(
                "Memory map has address width {}, which is not the same as bus "
                "interface address width {}".format(memory_map.addr_width,
                                                    self.addr_width))
        memory_map.freeze()
        self._map = memory_map


class AXILite2AXI(Elaboratable):

    def __init__(self,
                 axi_lite,
                 axi,
                 write_id=0,
                 read_id=0,
                 prot=0,
                 burst_type='INCR'):

        self.write_id = write_id
        self.read_id = read_id
        self.prot = prot
        self.burst_type = burst_type

        self.axi_lite = axi_lite
        self.axi = axi

    def elaborate(self, platform):
        m = Module()

        axi_lite = self.axi_lite
        axi = self.axi

        burst_size = log2_int(axi.data_width // 8)
        burst_type = {
            'FIXED': 0b00,
            'INCR': 0b01,
            'WRAP': 0b10
        }[self.burst_type]

        m.d.comb += [
            # aw
            axi.aw.valid.eq(axi_lite.aw.valid),
            axi_lite.aw.ready.eq(axi.aw.ready),
            axi.aw.bits.addr.eq(axi_lite.aw.addr),
            axi.aw.bits.burst.eq(burst_type),
            axi.aw.bits.len.eq(0),
            axi.aw.bits.size.eq(burst_size),
            axi.aw.bits.lock.eq(0),
            axi.aw.bits.prot.eq(self.prot),
            axi.aw.bits.cache.eq(0b0011),
            axi.aw.bits.qos.eq(0),
            axi.aw.bits.id.eq(self.write_id),

            # w
            axi.w.valid.eq(axi_lite.w.valid),
            axi_lite.w.ready.eq(axi.w.ready),
            axi.w.bits.data.eq(axi_lite.w.data),
            axi.w.bits.strb.eq(axi_lite.w.strb),
            axi.w.bits.last.eq(1),

            # b
            axi_lite.b.valid.eq(axi.b.valid),
            axi_lite.b.resp.eq(axi.b.bits.resp),
            axi.b.ready.eq(axi_lite.b.ready),

            # ar
            axi.ar.valid.eq(axi_lite.ar.valid),
            axi_lite.ar.ready.eq(axi.ar.ready),
            axi.ar.bits.addr.eq(axi_lite.ar.addr),
            axi.ar.bits.burst.eq(burst_type),
            axi.ar.bits.len.eq(0),
            axi.ar.bits.size.eq(burst_size),
            axi.ar.bits.lock.eq(0),
            axi.ar.bits.prot.eq(self.prot),
            axi.ar.bits.cache.eq(0b0011),
            axi.ar.bits.qos.eq(0),
            axi.ar.bits.id.eq(self.read_id),

            # r
            axi_lite.r.valid.eq(axi.r.valid),
            axi_lite.r.resp.eq(axi.r.bits.resp),
            axi_lite.r.data.eq(axi.r.bits.data),
            axi.r.ready.eq(axi_lite.r.ready),
        ]

        return m


class Wishbone2AXI(Elaboratable):

    def __init__(self, wishbone, axi, base_addr=0x00000000):
        self.base_addr = base_addr

        self.wishbone = wishbone
        self.axi = axi

    def elaborate(self, platform):
        m = Module()

        wb = self.wishbone
        axi = self.axi

        axil = AXILiteInterface(data_width=axi.data_width,
                                addr_width=axi.addr_width)

        m.submodules.wb2axil = Wishbone2AXILite(wb,
                                                axil,
                                                base_addr=self.base_addr)
        m.submodules.axil2axi = AXILite2AXI(axil, axi)

        return m


class _AXFragmenter(Elaboratable):

    def __init__(self, ax, data_width, max_size1=0):
        self.ax = ax
        self.data_width = data_width
        self.max_size1 = max_size1

        self.out = Decoupled(Record, ax.bits.layout)
        self.beats = Signal(len(self.ax.bits.len) + 1)
        self.last = Signal()

    def elaborate(self, platform):
        m = Module()

        beat_bytes = self.data_width // 8
        lg_bytes = log2_int(beat_bytes)

        busy = Signal()

        addr_reg = Signal.like(self.ax.bits.addr)
        len_reg = Signal.like(self.ax.bits.len)

        addr = Mux(busy, addr_reg, self.ax.bits.addr)
        length = Mux(busy, len_reg, self.ax.bits.len)
        alignment = (addr >> lg_bytes)[:len(self.ax.bits.len)]

        remain1 = Signal.like(self.ax.bits.len)
        for i in range(len(self.ax.bits.len) + 1):
            with m.If((length + 1) & (1 << i)):
                m.d.comb += remain1.eq((1 << i) - 1)

        align1 = Signal.like(self.ax.bits.len)
        m.d.comb += align1.eq(~0)
        for i in reversed(range(len(self.ax.bits.len))):
            with m.If(alignment & (1 << i)):
                m.d.comb += align1.eq((1 << i) - 1)

        max_size1 = self.max_size1 & remain1 & align1

        fixed = self.ax.bits.burst == AXIBurst.FIXED
        narrow = self.ax.bits.size != lg_bytes
        bad = fixed | narrow

        beats1 = Signal(len(self.ax.bits.len))
        beats = Signal(len(self.ax.bits.len) + 1)
        m.d.comb += [
            beats1.eq(Mux(bad, 0, max_size1)),
            beats.eq(beats1 + 1),
        ]

        def bytesm1(bits):
            max_shift = 1 << len(bits.size)
            tail = Const((1 << max_shift) - 1, max_shift)
            return (Cat(tail, bits.len) << bits.size) >> max_shift

        burst_addr = addr + (beats << self.ax.bits.size)
        wrap_mask = Signal.like(self.ax.bits.addr)
        mux_addr = Signal.like(self.ax.bits.addr)
        addr_mask = Signal.like(self.ax.bits.addr)
        m.d.comb += mux_addr.eq(burst_addr)
        with m.If(self.ax.bits.burst == AXIBurst.WRAP):
            m.d.comb += mux_addr.eq((burst_addr & wrap_mask)
                                    | (self.ax.bits.addr & ~wrap_mask))
        with m.If(self.ax.bits.burst == AXIBurst.FIXED):
            m.d.comb += mux_addr.eq(self.ax.bits.addr)
        m.d.comb += [
            wrap_mask.eq(bytesm1(self.ax.bits)),
            addr_mask.eq(self.ax.bits.size),
        ]

        last = length == beats1
        m.d.comb += [
            self.ax.ready.eq(self.out.ready & last),
            self.out.valid.eq(self.ax.valid),
            self.ax.bits.connect(self.out.bits),
            self.out.bits.len.eq(beats1),
            self.out.bits.addr.eq(addr & ~addr_mask),
            self.beats.eq(beats),
            self.last.eq(last),
        ]

        with m.If(self.out.fire):
            m.d.sync += [
                busy.eq(~last),
                addr_reg.eq(mux_addr),
                len_reg.eq(length - beats),
            ]

        return m


class AXIFragmenter(Elaboratable):

    def __init__(self, in_bus, max_size=8, max_flights=1):
        self.max_size = max_size
        self.max_flights = max_flights
        self.in_bus = in_bus

        self.out_bus = AXIInterface(addr_width=in_bus.addr_width,
                                    data_width=in_bus.data_width,
                                    id_width=in_bus.id_width)

        if max_size * 8 < in_bus.data_width:
            raise ValueError(
                "Max transfer size {} should not be smaller than bus data width {}"
                .format(max_size * 8, in_bus.data_width))

    def elaborate(self, platform):
        m = Module()

        in_bus = self.in_bus
        out_bus = self.out_bus
        beat_bytes = out_bus.data_width // 8
        max_size1 = self.max_size // beat_bytes - 1

        ar_queue = m.submodules.ar_queue = Queue(1,
                                                 Record,
                                                 self.in_bus.ar.bits.layout,
                                                 flow=True)
        aw_queue = m.submodules.aw_queue = Queue(1,
                                                 Record,
                                                 self.in_bus.aw.bits.layout,
                                                 flow=True)
        m.d.comb += [
            in_bus.ar.bits.connect(ar_queue.enq.bits),
            ar_queue.enq.valid.eq(in_bus.ar.valid),
            in_bus.ar.ready.eq(ar_queue.enq.ready),
            in_bus.aw.bits.connect(aw_queue.enq.bits),
            aw_queue.enq.valid.eq(in_bus.aw.valid),
            in_bus.aw.ready.eq(aw_queue.enq.ready),
        ]
        ar_frag = m.submodules.ar_frag = _AXFragmenter(
            ar_queue.deq, data_width=in_bus.data_width, max_size1=max_size1)
        aw_frag = m.submodules.aw_frag = _AXFragmenter(
            aw_queue.deq, data_width=in_bus.data_width, max_size1=max_size1)

        w_queue = m.submodules.w_queue = Queue(1,
                                               Record,
                                               self.in_bus.w.bits.layout,
                                               flow=True)
        m.d.comb += [
            in_bus.w.bits.connect(w_queue.enq.bits),
            w_queue.enq.valid.eq(in_bus.w.valid),
            in_bus.w.ready.eq(w_queue.enq.ready),
        ]

        rqueues = [
            SyncFIFO(depth=self.max_flights, width=1)
            for _ in range(2**len(in_bus.ar.bits.id))
        ]
        m.submodules += rqueues
        ar_ready = Signal()

        with m.Switch(out_bus.ar.bits.id):
            for i, q in enumerate(rqueues):
                with m.Case(i):
                    m.d.comb += [
                        ar_ready.eq(q.w_rdy),
                        q.w_en.eq(out_bus.ar.valid & out_bus.ar.ready),
                        q.w_data.eq(ar_frag.last),
                    ]

        m.d.comb += [
            ar_frag.out.bits.connect(out_bus.ar.bits),
            out_bus.ar.valid.eq(ar_frag.out.valid & ar_ready),
            ar_frag.out.ready.eq(out_bus.ar.ready & ar_ready),
        ]

        wqueues = [
            SyncFIFO(depth=self.max_flights, width=1)
            for _ in range(2**len(in_bus.aw.bits.id))
        ]
        m.submodules += wqueues
        aw_ready = Signal()

        with m.Switch(out_bus.aw.bits.id):
            for i, q in enumerate(wqueues):
                with m.Case(i):
                    m.d.comb += [
                        aw_ready.eq(q.w_rdy),
                        q.w_en.eq(out_bus.aw.valid & out_bus.aw.ready),
                        q.w_data.eq(aw_frag.last),
                    ]

        wbeats_valid = Signal()
        wbeats_ready = Signal()
        wbeats_latched = Signal()
        with m.If(wbeats_valid & wbeats_ready):
            m.d.sync += wbeats_latched.eq(1)
        with m.If(out_bus.aw.valid & out_bus.aw.ready):
            m.d.sync += wbeats_latched.eq(0)

        m.d.comb += [
            aw_frag.out.bits.connect(out_bus.aw.bits),
            out_bus.aw.valid.eq(aw_frag.out.valid
                                & (wbeats_ready | wbeats_latched) & aw_ready),
            aw_frag.out.ready.eq(out_bus.aw.ready
                                 & (wbeats_ready | wbeats_latched) & aw_ready),
            wbeats_valid.eq(aw_frag.out.valid & aw_ready & ~wbeats_latched),
        ]

        w_counter = Signal(len(in_bus.aw.bits.len) + 1)
        w_idle = ~w_counter.any()
        w_rem = Mux(w_idle, Mux(wbeats_valid, aw_frag.beats, 0), w_counter)
        w_last = w_rem == 1
        m.d.sync += w_counter.eq(w_rem - (out_bus.w.valid & out_bus.w.ready))

        m.d.comb += [
            wbeats_ready.eq(w_idle),
            w_queue.deq.bits.connect(out_bus.w.bits),
            out_bus.w.bits.last.eq(w_last),
            out_bus.w.valid.eq(w_queue.deq.valid
                               & (~wbeats_ready | wbeats_valid)),
            w_queue.deq.ready.eq(out_bus.w.ready
                                 & (~wbeats_ready | wbeats_valid)),
        ]

        r_last = Signal()
        with m.Switch(out_bus.r.bits.id):
            for i, q in enumerate(rqueues):
                with m.Case(i):
                    m.d.comb += [
                        r_last.eq(q.r_data),
                        q.r_en.eq(out_bus.r.valid & out_bus.r.ready
                                  & out_bus.r.bits.last),
                    ]

        m.d.comb += [
            in_bus.r.connect(out_bus.r),
            in_bus.r.bits.last.eq(out_bus.r.bits.last & r_last),
        ]

        b_last = Signal()
        with m.Switch(out_bus.b.bits.id):
            for i, q in enumerate(wqueues):
                with m.Case(i):
                    m.d.comb += [
                        b_last.eq(q.r_data),
                        q.r_en.eq(out_bus.b.valid & out_bus.b.ready),
                    ]

        m.d.comb += [
            in_bus.b.bits.connect(out_bus.b.bits),
            in_bus.b.valid.eq(out_bus.b.valid & b_last),
            out_bus.b.ready.eq(in_bus.b.ready | ~b_last),
        ]

        return m


class AXI2AXILite(Elaboratable):

    def __init__(self, axi, axi_lite):
        self.axi = axi
        self.axi_lite = axi_lite

    def elaborate(self, platform):
        m = Module()

        axi = self.axi
        axil = self.axi_lite

        ax_layout = make_ax_layout(addr_width=axi.addr_width,
                                   id_width=axi.id_width)
        ax_buffer = m.submodules.ax_buffer = SkidBuffer(Record, ax_layout)
        fragmenter = m.submodules.fragmenter = _AXFragmenter(
            ax_buffer.deq, data_width=axil.data_width, max_size1=0)

        cmd_done = Signal()
        last_ar = Signal()

        m.d.comb += axil.b.ready.eq(1)

        with m.FSM():
            with m.State('IDLE'):
                m.d.sync += cmd_done.eq(0)

                with m.If(axi.ar.valid & axi.aw.valid):
                    with m.If(last_ar):
                        m.d.comb += [
                            axi.aw.bits.connect(ax_buffer.enq.bits),
                            ax_buffer.enq.valid.eq(axi.aw.valid),
                            axi.aw.ready.eq(ax_buffer.enq.ready),
                        ]
                        m.d.sync += last_ar.eq(0)
                        m.next = 'WRITE'

                    with m.Else():
                        m.d.comb += [
                            axi.ar.bits.connect(ax_buffer.enq.bits),
                            ax_buffer.enq.valid.eq(axi.ar.valid),
                            axi.ar.ready.eq(ax_buffer.enq.ready),
                        ]
                        m.d.sync += last_ar.eq(1)
                        m.next = 'READ'
                with m.Elif(axi.ar.valid):
                    m.d.comb += [
                        axi.ar.bits.connect(ax_buffer.enq.bits),
                        ax_buffer.enq.valid.eq(axi.ar.valid),
                        axi.ar.ready.eq(ax_buffer.enq.ready),
                    ]
                    m.d.sync += last_ar.eq(1)
                    m.next = 'READ'
                with m.Elif(axi.aw.valid):
                    m.d.comb += [
                        axi.aw.bits.connect(ax_buffer.enq.bits),
                        ax_buffer.enq.valid.eq(axi.aw.valid),
                        axi.aw.ready.eq(ax_buffer.enq.ready),
                    ]
                    m.d.sync += last_ar.eq(0)
                    m.next = 'WRITE'

            with m.State('READ'):
                m.d.comb += [
                    axil.ar.valid.eq(fragmenter.out.valid & ~cmd_done),
                    axil.ar.addr.eq(fragmenter.out.bits.addr),
                    fragmenter.out.ready.eq(axil.ar.ready & ~cmd_done),
                ]
                with m.If(fragmenter.out.valid & fragmenter.last):
                    with m.If(axil.ar.ready):
                        m.d.comb += fragmenter.out.ready.eq(0)
                        m.d.sync += cmd_done.eq(1)

                m.d.comb += [
                    axi.r.valid.eq(axil.r.valid),
                    axi.r.bits.last.eq(cmd_done),
                    axi.r.bits.resp.eq(0),
                    axi.r.bits.id.eq(fragmenter.out.bits.id),
                    axi.r.bits.data.eq(axil.r.data),
                    axil.r.ready.eq(axi.r.ready),
                ]

                with m.If(axi.r.valid & axi.r.ready & axi.r.bits.last):
                    m.d.comb += fragmenter.out.ready.eq(1)
                    m.next = 'IDLE'

            with m.State('WRITE'):
                m.d.comb += [
                    axil.aw.valid.eq(fragmenter.out.valid & ~cmd_done),
                    axil.aw.addr.eq(fragmenter.out.bits.addr),
                    fragmenter.out.ready.eq(axil.aw.ready & ~cmd_done),
                ]
                with m.If(fragmenter.out.valid & fragmenter.last):
                    with m.If(axil.aw.ready):
                        m.d.comb += fragmenter.out.ready.eq(0)
                        m.d.sync += cmd_done.eq(1)

                m.d.comb += [
                    axil.w.valid.eq(axi.w.valid),
                    axil.w.data.eq(axi.w.bits.data),
                    axil.w.strb.eq(axi.w.bits.strb),
                    axi.w.ready.eq(axil.w.ready),
                ]

                with m.If(axi.w.valid & axi.w.ready & axi.w.bits.last):
                    m.next = 'RESP'

            with m.State('RESP'):
                m.d.comb += [
                    axi.b.valid.eq(1),
                    axi.b.bits.resp.eq(0),
                    axi.b.bits.id.eq(fragmenter.out.bits.id),
                ]
                with m.If(axi.b.ready):
                    m.d.comb += fragmenter.out.ready.eq(1)
                    m.next = 'IDLE'

        return m


class AXI2Wishbone(Elaboratable):

    def __init__(self, axi, wishbone, base_addr=0x00000000):
        self.base_addr = base_addr

        self.axi = axi
        self.wishbone = wishbone

    def elaborate(self, platform):
        m = Module()

        wb = self.wishbone
        axi = self.axi

        axil = AXILiteInterface(data_width=axi.data_width,
                                addr_width=axi.addr_width)

        m.submodules.axi2axil = AXI2AXILite(axi, axil)
        m.submodules.axil2wishbone = AXILite2Wishbone(axil,
                                                      wb,
                                                      base_addr=self.base_addr)

        return m


class _RequestCounter(Elaboratable):

    def __init__(self, max_requests=256):
        self.max_requests = max_requests

        self.req = Signal()
        self.resp = Signal()
        self.ready = Signal()
        self.stall = Signal()

    def elaborate(self, platform):
        m = Module()

        counter = Signal(range(self.max_requests))
        full = counter == self.max_requests - 1
        empty = counter == 0
        m.d.comb += [
            self.ready.eq(empty),
            self.stall.eq(self.req & full),
        ]

        with m.If(self.req & self.resp):
            m.d.sync += counter.eq(counter)
        with m.Elif(self.req & ~full):
            m.d.sync += counter.eq(counter + 1)
        with m.Elif(self.resp & ~empty):
            m.d.sync += counter.eq(counter - 1)

        return m


class AXIArbiter(Elaboratable):

    def __init__(self, *, addr_width, data_width, id_width):
        self.bus = AXIInterface(addr_width=addr_width,
                                data_width=data_width,
                                id_width=id_width)
        self._intrs = []

    def add(self, intr_bus):
        if not isinstance(intr_bus, AXIInterface):
            raise TypeError(
                "Initiator bus must be an instance of AXIInterface, not {!r}".
                format(intr_bus))
        if intr_bus.addr_width != self.bus.addr_width:
            raise ValueError(
                "Initiator bus has address width {}, which is not the same as "
                "arbiter address width {}".format(intr_bus.addr_width,
                                                  self.bus.addr_width))
        if intr_bus.data_width != self.bus.data_width:
            raise ValueError(
                "Initiator bus has data width {}, which is not the same as "
                "arbiter data width {}".format(intr_bus.data_width,
                                               self.bus.data_width))

        self._intrs.append(intr_bus)

    def elaborate(self, platform):
        m = Module()

        rd_requests = Signal(len(self._intrs))
        rd_grant = Signal(range(len(self._intrs)))
        rd_early_grant = Signal.like(rd_grant)
        m.d.comb += [
            rd_requests.eq(Cat(intr_bus.ar.valid for intr_bus in self._intrs)),
            rd_early_grant.eq(rd_grant),
        ]
        rd_lock = m.submodules.rd_lock = _RequestCounter()
        m.d.comb += [
            rd_lock.req.eq(self.bus.ar.valid & self.bus.ar.ready),
            rd_lock.resp.eq(self.bus.r.valid & self.bus.r.ready
                            & self.bus.r.bits.last),
        ]

        with m.If(rd_lock.ready):
            with m.Switch(rd_grant):
                for i in range(len(rd_requests)):
                    with m.Case(i):
                        with m.If(rd_requests[i]):
                            m.d.comb += rd_early_grant.eq(i)

                        for pred in reversed(range(i)):
                            with m.If(rd_requests[pred]):
                                m.d.comb += rd_early_grant.eq(pred)

                                with m.If(rd_lock.req):
                                    m.d.sync += rd_grant.eq(pred)

                        for succ in reversed(range(i + 1, len(rd_requests))):
                            with m.If(rd_requests[succ]):
                                m.d.comb += rd_early_grant.eq(succ)

                                with m.If(rd_lock.req):
                                    m.d.sync += rd_grant.eq(succ)

        wr_requests = Signal(len(self._intrs))
        wr_grant = Signal(range(len(self._intrs)))
        wr_early_grant = Signal.like(wr_grant)
        m.d.comb += [
            wr_requests.eq(Cat(intr_bus.aw.valid for intr_bus in self._intrs)),
            wr_early_grant.eq(wr_grant),
        ]
        wr_lock = m.submodules.wr_lock = _RequestCounter()
        m.d.comb += [
            wr_lock.req.eq(self.bus.aw.valid & self.bus.aw.ready),
            wr_lock.resp.eq(self.bus.b.valid & self.bus.b.ready),
        ]

        with m.If(wr_lock.ready):
            with m.Switch(wr_grant):
                for i in range(len(wr_requests)):
                    with m.Case(i):
                        with m.If(wr_requests[i]):
                            m.d.comb += wr_early_grant.eq(i)

                        for pred in reversed(range(i)):
                            with m.If(wr_requests[pred]):
                                m.d.comb += wr_early_grant.eq(pred)

                                with m.If(wr_lock.req):
                                    m.d.sync += wr_grant.eq(pred)

                        for succ in reversed(range(i + 1, len(wr_requests))):
                            with m.If(wr_requests[succ]):
                                m.d.comb += wr_early_grant.eq(succ)

                                with m.If(wr_lock.req):
                                    m.d.sync += wr_grant.eq(succ)

        with m.Switch(rd_early_grant):
            for i, intr_bus in enumerate(self._intrs):
                with m.Case(i):
                    m.d.comb += [
                        intr_bus.ar.connect(self.bus.ar),
                        intr_bus.r.connect(self.bus.r),
                    ]

        with m.Switch(wr_early_grant):
            for i, intr_bus in enumerate(self._intrs):
                with m.Case(i):
                    m.d.comb += [
                        intr_bus.aw.connect(self.bus.aw),
                        intr_bus.w.connect(self.bus.w),
                        intr_bus.b.connect(self.bus.b),
                    ]

        return m


class AXIDecoder(Elaboratable):

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 id_width,
                 alignment=0,
                 name=None):
        self.data_width = data_width
        self.id_width = id_width
        self.alignment = alignment

        self._map = MemoryMap(addr_width=max(1, addr_width),
                              data_width=8,
                              alignment=alignment,
                              name=name)
        self._subs = dict()
        self._bus = None

    @property
    def bus(self):
        if self._bus is None:
            self._map.freeze()
            self._bus = AXIInterface(addr_width=self._map.addr_width,
                                     data_width=self.data_width,
                                     id_width=self.id_width)
            self._bus.memory_map = self._map
        return self._bus

    def align_to(self, alignment):
        return self._map.align_to(alignment)

    def add(self, sub_bus, *, addr=None, sparse=False, extend=False):
        if not isinstance(sub_bus, AXIInterface):
            raise TypeError(
                "Subordinate bus must be an instance of AXIInterface, not {!r}"
                .format(sub_bus))
        if not sparse:
            if sub_bus.data_width != self.data_width:
                raise ValueError(
                    "Subordinate bus has data width {}, which is not the same as "
                    "decoder data width {} (required for dense address translation)"
                    .format(sub_bus.data_width, self.data_width))

        self._subs[sub_bus.memory_map] = sub_bus
        return self._map.add_window(sub_bus.memory_map,
                                    addr=addr,
                                    sparse=sparse,
                                    extend=extend)

    def elaborate(self, platform):
        m = Module()

        rd_sel_dec = Signal(len(self._subs))
        rd_sel_reg = Signal(len(self._subs))
        rd_sel = Signal(len(self._subs))

        wr_sel_dec = Signal(len(self._subs))
        wr_sel_reg = Signal(len(self._subs))
        wr_sel = Signal(len(self._subs))

        with m.Switch(self.bus.ar.bits.addr):
            for i, (_, (sub_pat, _)) in enumerate(self._map.window_patterns()):
                with m.Case(sub_pat):
                    m.d.comb += rd_sel_dec[i].eq(1)

        with m.Switch(self.bus.aw.bits.addr):
            for i, (_, (sub_pat, _)) in enumerate(self._map.window_patterns()):
                with m.Case(sub_pat):
                    m.d.comb += wr_sel_dec[i].eq(1)

        rd_lock = m.submodules.rd_lock = _RequestCounter()
        m.d.comb += [
            rd_lock.req.eq(self.bus.ar.valid & self.bus.ar.ready),
            rd_lock.resp.eq(self.bus.r.valid & self.bus.r.ready
                            & self.bus.r.bits.last),
        ]
        wr_lock = m.submodules.wr_lock = _RequestCounter()
        m.d.comb += [
            wr_lock.req.eq(self.bus.aw.valid & self.bus.aw.ready),
            wr_lock.resp.eq(self.bus.b.valid & self.bus.b.ready),
        ]

        with m.If(rd_lock.ready):
            m.d.sync += rd_sel_reg.eq(rd_sel_dec)
        with m.If(wr_lock.ready):
            m.d.sync += wr_sel_reg.eq(wr_sel_dec)

        m.d.comb += [
            rd_sel.eq(Mux(rd_lock.ready, rd_sel_dec, rd_sel_reg)),
            wr_sel.eq(Mux(wr_lock.ready, wr_sel_dec, wr_sel_reg)),
        ]

        for i, (sub_map, (_, _)) in enumerate(self._map.window_patterns()):
            sub_bus = self._subs[sub_map]

            with m.If(rd_sel[i]):
                m.d.comb += [
                    self.bus.ar.connect(sub_bus.ar),
                    sub_bus.ar.valid.eq(self.bus.ar.valid & rd_sel_dec[i]),
                    self.bus.ar.ready.eq(sub_bus.ar.ready & rd_sel_dec[i]),
                    self.bus.r.connect(sub_bus.r),
                ]

            with m.If(wr_sel[i]):
                m.d.comb += [
                    self.bus.aw.connect(sub_bus.aw),
                    sub_bus.aw.valid.eq(self.bus.aw.valid & wr_sel_dec[i]),
                    self.bus.aw.ready.eq(sub_bus.aw.ready & wr_sel_dec[i]),
                    self.bus.w.connect(sub_bus.w),
                    self.bus.b.connect(sub_bus.b),
                ]

        return m


class AXIInterconnectP2P(Elaboratable):

    def __init__(self, master, slave):
        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.master.connect(self.slave)

        return m


def _check_parameter(intrs, param_fn):
    param = param_fn(intrs[0])
    if len(intrs) > 1:
        for intr in intrs[1:]:
            param = max(param, param_fn(intr))

    return param


class AXIIDWidthConverter(Elaboratable):

    def __init__(self, in_bus, out_bus):
        self.in_bus = in_bus
        self.out_bus = out_bus

    def elaborate(self, platform):
        m = Module()

        in_id_width = self.in_bus.id_width
        out_id_width = self.out_bus.id_width

        m.d.comb += self.in_bus.connect(self.out_bus)

        if in_id_width > out_id_width:
            rd_lock = m.submodules.rd_lock = _RequestCounter()
            m.d.comb += [
                rd_lock.req.eq(self.in_bus.ar.valid & self.in_bus.ar.ready),
                rd_lock.resp.eq(self.in_bus.r.valid & self.in_bus.r.ready
                                & self.in_bus.r.bits.last),
            ]
            wr_lock = m.submodules.wr_lock = _RequestCounter()
            m.d.comb += [
                wr_lock.req.eq(self.in_bus.aw.valid & self.in_bus.aw.ready),
                wr_lock.resp.eq(self.in_bus.b.valid & self.in_bus.b.ready),
            ]

            rid = Signal(in_id_width)
            wid = Signal(in_id_width)

            with m.If(self.in_bus.ar.valid & self.in_bus.ar.ready):
                m.d.sync += rid.eq(self.in_bus.ar.bits.id)
            with m.If(self.in_bus.aw.valid & self.in_bus.aw.ready):
                m.d.sync += wid.eq(self.in_bus.aw.bits.id)

            m.d.comb += [
                self.in_bus.ar.ready.eq(self.out_bus.ar.ready & rd_lock.ready),
                self.in_bus.r.bits.id.eq(rid),
                self.in_bus.aw.ready.eq(self.out_bus.aw.ready & wr_lock.ready),
                self.in_bus.b.bits.id.eq(wid),
            ]

        return m


class AXIInterconnectShared(Elaboratable):

    def __init__(self,
                 addr_width,
                 data_width,
                 masters,
                 slaves,
                 timeout_cycles=None):
        self.addr_width = addr_width
        self.data_width = data_width
        self.id_width = _check_parameter(intrs=masters,
                                         param_fn=lambda intr: intr.id_width)
        self.masters = masters
        self.slaves = slaves

    def elaborate(self, platform):
        m = Module()

        arbiter = m.submodules.arbiter = AXIArbiter(data_width=self.data_width,
                                                    addr_width=self.addr_width,
                                                    id_width=self.id_width)
        for master in self.masters:
            arbiter.add(master)
        shared = arbiter.bus

        decoder = m.submodules.decoder = AXIDecoder(data_width=self.data_width,
                                                    addr_width=self.addr_width,
                                                    id_width=self.id_width)
        for region, slave in self.slaves:
            if slave.id_width != self.id_width:
                adapted_bus = AXIInterface(addr_width=slave.addr_width,
                                           data_width=slave.data_width,
                                           id_width=self.id_width)
                adapted_bus.memory_map = slave.memory_map

                iw_converter = AXIIDWidthConverter(adapted_bus, slave)
                m.submodules += iw_converter
                slave = adapted_bus

            decoder.add(slave, addr=region.origin)
        m.d.comb += shared.connect(decoder.bus)

        return m
