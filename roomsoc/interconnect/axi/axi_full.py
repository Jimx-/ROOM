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
                 version='axi4',
                 name=None,
                 src_loc_at=1):
        if version not in ('axi3', 'axi4'):
            raise ValueError("version must be 'axi3' or 'axi4'")

        self.addr_width = addr_width
        self.data_width = data_width
        self.id_width = id_width
        self.version = version
        self._map = None

        super().__init__(make_axi_layout(data_width=data_width,
                                         addr_width=addr_width,
                                         id_width=id_width,
                                         version=version),
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
                 prot=None,
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
        write_prot = axi_lite.aw.prot if self.prot is None else self.prot
        read_prot = axi_lite.ar.prot if self.prot is None else self.prot

        m.d.comb += [
            # aw
            axi.aw.valid.eq(axi_lite.aw.valid),
            axi_lite.aw.ready.eq(axi.aw.ready),
            axi.aw.bits.addr.eq(axi_lite.aw.addr),
            axi.aw.bits.burst.eq(burst_type),
            axi.aw.bits.len.eq(0),
            axi.aw.bits.size.eq(burst_size),
            axi.aw.bits.lock.eq(0),
            axi.aw.bits.prot.eq(write_prot),
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
            axi.ar.bits.prot.eq(read_prot),
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
                                    id_width=in_bus.id_width,
                                    version=in_bus.version)

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
        if axi.addr_width != axi_lite.addr_width:
            raise ValueError("AXI and AXI-Lite address widths must match")
        if axi.data_width != axi_lite.data_width:
            raise ValueError("AXI and AXI-Lite data widths must match")

        self.axi = axi
        self.axi_lite = axi_lite

    def elaborate(self, platform):
        m = Module()

        axi = self.axi
        axil = self.axi_lite
        bus_addr_mask = axi.data_width // 8 - 1

        ax_layout = make_ax_layout(addr_width=axi.addr_width,
                                   id_width=axi.id_width)
        ax_buffer = m.submodules.ax_buffer = SkidBuffer(Record, ax_layout)
        fragmenter = m.submodules.fragmenter = _AXFragmenter(
            ax_buffer.deq, data_width=axil.data_width, max_size1=0)

        cmd_done = Signal()
        last_ar = Signal()
        write_resp = Signal(2)

        with m.FSM():
            with m.State('IDLE'):
                m.d.sync += [
                    cmd_done.eq(0),
                    write_resp.eq(0),
                ]

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
                    # AXI-Lite has no transfer size, so represent a narrow AXI
                    # access as a full-width, bus-aligned AXI-Lite access. The
                    # AXI byte lanes retain the requested transfer placement.
                    axil.ar.addr.eq(fragmenter.out.bits.addr
                                    & ~bus_addr_mask),
                    axil.ar.prot.eq(fragmenter.out.bits.prot),
                    fragmenter.out.ready.eq(axil.ar.ready & ~cmd_done),
                ]
                with m.If(fragmenter.out.valid & fragmenter.last):
                    with m.If(axil.ar.ready):
                        m.d.comb += fragmenter.out.ready.eq(0)
                        m.d.sync += cmd_done.eq(1)

                m.d.comb += [
                    axi.r.valid.eq(axil.r.valid),
                    axi.r.bits.last.eq(cmd_done),
                    axi.r.bits.resp.eq(axil.r.resp),
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
                    axil.aw.addr.eq(fragmenter.out.bits.addr
                                    & ~bus_addr_mask),
                    axil.aw.prot.eq(fragmenter.out.bits.prot),
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
                    axil.b.ready.eq(1),
                ]

                with m.If(axil.b.valid & (axil.b.resp != 0)):
                    m.d.sync += write_resp.eq(axil.b.resp)

                with m.If(axi.w.valid & axi.w.ready & axi.w.bits.last):
                    m.next = 'WRITE_RESP'

            with m.State('WRITE_RESP'):
                m.d.comb += axil.b.ready.eq(1)
                with m.If(axil.b.valid):
                    with m.If(axil.b.resp != 0):
                        m.d.sync += write_resp.eq(axil.b.resp)
                    m.next = 'RESP'

            with m.State('RESP'):
                m.d.comb += [
                    axi.b.valid.eq(1),
                    axi.b.bits.resp.eq(write_resp),
                    axi.b.bits.id.eq(fragmenter.out.bits.id),
                ]
                with m.If(axi.b.valid & axi.b.ready):
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

        # Address forwarding truncates the initiator address to the subordinate
        # bus width. This is equivalent to subtracting the window base only
        # when the base is aligned to the subordinate aperture size.
        if isinstance(addr, int) and addr >= 0:
            aperture_size = 1 << sub_bus.memory_map.addr_width
            if addr % aperture_size != 0:
                raise ValueError(
                    "Window address {:#x} must be aligned to its {:#x}-byte "
                    "aperture".format(addr, aperture_size))

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
                self.out_bus.ar.valid.eq(self.in_bus.ar.valid & rd_lock.ready),
                self.in_bus.ar.ready.eq(self.out_bus.ar.ready & rd_lock.ready),
                self.in_bus.r.bits.id.eq(rid),
                self.out_bus.aw.valid.eq(self.in_bus.aw.valid & wr_lock.ready),
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


# AXI Bursts to Beats ------------------------------------------------------------------------------

class AXIBurst2Beat(Elaboratable):
    """Expand an AXI address-burst channel into one beat per transfer.

    The input ``ax_burst`` is an AXI AW/AR channel (a ``Record`` carrying ``.bits``,
    ``.valid`` and ``.ready``). The output :pyattr:`out` is a :class:`Decoupled` stream of
    single beats carrying ``addr``, ``id``, ``first`` and ``last``. FIXED bursts hold the
    address, INCR/WRAP bursts advance it by ``2**size`` per beat (WRAP wraps within the
    burst window). Mirrors LiteX ``AXIBurst2Beat``.
    """

    def __init__(self, ax_burst):
        self.ax_burst = ax_burst
        addr_w = len(ax_burst.bits.addr)
        id_w = len(ax_burst.bits.id)
        self.out = Decoupled(Record, [
            ('addr',  addr_w),
            ('id',    id_w),
            ('first', 1),
            ('last',  1),
        ])

    def elaborate(self, platform):
        m = Module()

        ax_burst = self.ax_burst
        ax_beat = self.out

        addr_w = len(ax_burst.bits.addr)
        beat_count  = Signal(8)
        beat_size   = Signal(addr_w + 1)
        beat_offset = Signal(signed(addr_w + 1))
        beat_wrap   = Signal(addr_w)

        m.d.comb += [
            beat_size.eq(1 << ax_burst.bits.size),
            beat_wrap.eq((ax_burst.bits.len << ax_burst.bits.size)[:addr_w]),

            ax_beat.valid.eq(ax_burst.valid | ~ax_beat.bits.first),
            ax_beat.bits.first.eq(beat_count == 0),
            ax_beat.bits.last.eq(beat_count == ax_burst.bits.len),
            ax_beat.bits.addr.eq(
                (ax_burst.bits.addr + beat_offset)[:addr_w]),
            ax_beat.bits.id.eq(ax_burst.bits.id),
        ]
        with m.If(ax_beat.ready):
            with m.If(ax_beat.bits.last):
                m.d.comb += ax_burst.ready.eq(1)

        with m.If(ax_beat.valid & ax_beat.ready):
            with m.If(ax_beat.bits.last):
                m.d.sync += [
                    beat_count.eq(0),
                    beat_offset.eq(0),
                ]
            with m.Else():
                m.d.sync += beat_count.eq(beat_count + 1)
                with m.If((ax_burst.bits.burst == AXIBurst.INCR) |
                          (ax_burst.bits.burst == AXIBurst.WRAP)):
                    m.d.sync += beat_offset.eq(beat_offset + beat_size)
            with m.If(ax_burst.bits.burst == AXIBurst.WRAP):
                with m.If((ax_beat.bits.addr & beat_wrap) == beat_wrap):
                    # ``beat_wrap`` is the byte offset of the final beat in
                    # the wrap window.  Move from that beat to the window's
                    # first beat, rather than merely undoing the normal
                    # one-beat increment above.
                    m.d.sync += beat_offset.eq(beat_offset - beat_wrap)

        return m


# AXI Stream Width Converters (used internally by the data-width converter) ------------------------

class _StrideDown(Elaboratable):
    """Split each wide (data + strb) beat into ``ratio`` narrow beats."""

    def __init__(self, dw_wide, dw_narrow):
        self.dw_wide = dw_wide
        self.dw_narrow = dw_narrow
        self.ratio = dw_wide // dw_narrow
        self.sink = Decoupled(Record, [
            ('data', dw_wide),
            ('strb', dw_wide // 8),
            ('last', 1),
        ])
        self.source = Decoupled(Record, [
            ('data', dw_narrow),
            ('strb', dw_narrow // 8),
            ('last', 1),
        ])

    def elaborate(self, platform):
        m = Module()

        ratio = self.ratio
        nb = self.dw_narrow // 8
        sel = Signal(range(ratio))

        m.d.comb += [
            self.source.valid.eq(self.sink.valid),
            self.source.bits.last.eq(self.sink.bits.last & (sel == ratio - 1)),
        ]
        with m.Switch(sel):
            for i in range(ratio):
                with m.Case(i):
                    m.d.comb += [
                        self.source.bits.data.eq(
                            self.sink.bits.data[i * self.dw_narrow:(i + 1) * self.dw_narrow]),
                        self.source.bits.strb.eq(
                            self.sink.bits.strb[i * nb:(i + 1) * nb]),
                    ]

        with m.If(self.source.fire):
            m.d.sync += sel.eq(sel + 1)
            with m.If(sel == ratio - 1):
                m.d.comb += self.sink.ready.eq(1)
                m.d.sync += sel.eq(0)

        return m


class _StrideUp(Elaboratable):
    """Merge ``ratio`` narrow data beats into one wide beat."""

    def __init__(self, dw_narrow, dw_wide):
        self.dw_narrow = dw_narrow
        self.dw_wide = dw_wide
        self.ratio = dw_wide // dw_narrow
        self.sink = Decoupled(Record, [
            ('data', dw_narrow),
            ('last', 1),
        ])
        self.source = Decoupled(Record, [
            ('data', dw_wide),
            ('last', 1),
        ])

    def elaborate(self, platform):
        m = Module()

        ratio = self.ratio
        sel = Signal(range(ratio))

        m.d.comb += self.sink.ready.eq(~self.source.valid | self.source.ready)

        with m.If(self.source.ready):
            m.d.sync += self.source.valid.eq(0)

        with m.If(self.sink.fire):
            with m.If(sel == 0):
                m.d.sync += self.source.bits.data.eq(0)
            with m.Switch(sel):
                for i in range(ratio):
                    with m.Case(i):
                        m.d.sync += self.source.bits.data[
                            i * self.dw_narrow:(i + 1) * self.dw_narrow].eq(
                                self.sink.bits.data)
            with m.If((sel == ratio - 1) | self.sink.bits.last):
                m.d.sync += [
                    self.source.valid.eq(1),
                    self.source.bits.last.eq(self.sink.bits.last),
                    sel.eq(0),
                ]
            with m.Else():
                m.d.sync += sel.eq(sel + 1)

        return m


# AXI Data-Width Up Converter ---------------------------------------------------------------------

class AXIUpConverter(Elaboratable):
    """AXI4 data-width up-converter (``dw_from < dw_to``, integer ratio).

    Each narrow beat is turned into a single-beat wide transfer, placing the narrow
    data/strobe into the address-selected wide lane. Bursts of any type are supported:
    an :class:`AXIBurst2Beat` expands the address burst into per-beat commands so every
    narrow beat becomes its own wide single-beat access. Mirrors LiteX
    ``AXIUpConverter``.
    """

    def __init__(self, axi_from, axi_to):
        self.axi_from = axi_from
        self.axi_to = axi_to
        dw_from = axi_from.data_width
        dw_to = axi_to.data_width
        ratio = dw_to // dw_from
        if dw_from * ratio != dw_to:
            raise ValueError("AXI up-converter ratio must be an integer")
        self.ratio = ratio
        self.lane_bits = log2_int(ratio)
        self.lane_lsb = log2_int(dw_from // 8)

    def elaborate(self, platform):
        m = Module()

        axi_from = self.axi_from
        axi_to = self.axi_to
        ratio = self.ratio
        lane_bits = self.lane_bits
        lane_lsb = self.lane_lsb
        dw_from = axi_from.data_width

        has_user = len(axi_from.aw.bits.user) > 0
        is_axi3 = axi_from.version == 'axi3'
        lane_field = max(1, lane_bits)

        def ax_cmd_layout():
            layout = [
                ('addr',   len(axi_to.aw.bits.addr)),
                ('size',   len(axi_to.aw.bits.size)),
                ('lock',   len(axi_to.aw.bits.lock)),
                ('prot',   len(axi_to.aw.bits.prot)),
                ('cache',  len(axi_to.aw.bits.cache)),
                ('qos',    len(axi_to.aw.bits.qos)),
                ('region', len(axi_to.aw.bits.region)),
                ('id',     len(axi_to.aw.bits.id)),
                ('lane',   lane_field),
                ('last',   1),
            ]
            if has_user:
                layout.append(('user', len(axi_to.aw.bits.user)))
            return layout

        # -- Write address path ---------------------------------------------------------------
        aw_b2b = m.submodules.aw_b2b = AXIBurst2Beat(axi_from.aw)
        aw = aw_b2b.out

        aw_cmd = m.submodules.aw_cmd = Queue(16, Record, ax_cmd_layout(), flow=False)
        aw_data = m.submodules.aw_data = Queue(16, Record, [('lane', lane_field)], flow=False)
        b_info = m.submodules.b_info = Queue(
            16, Record, [('id', len(axi_to.b.bits.id)), ('last', 1)], flow=False)

        aw_fifos_ready = Signal()
        m.d.comb += [
            aw_fifos_ready.eq(aw_cmd.enq.ready & aw_data.enq.ready),
            aw.ready.eq(aw_fifos_ready),

            aw_cmd.enq.valid.eq(aw.valid & aw_data.enq.ready),
            aw_cmd.enq.bits.addr.eq(aw.bits.addr),
            aw_cmd.enq.bits.size.eq(axi_from.aw.bits.size),
            aw_cmd.enq.bits.lock.eq(axi_from.aw.bits.lock),
            aw_cmd.enq.bits.prot.eq(axi_from.aw.bits.prot),
            aw_cmd.enq.bits.cache.eq(axi_from.aw.bits.cache),
            aw_cmd.enq.bits.qos.eq(axi_from.aw.bits.qos),
            aw_cmd.enq.bits.region.eq(axi_from.aw.bits.region),
            aw_cmd.enq.bits.id.eq(aw.bits.id),
            aw_cmd.enq.bits.lane.eq(aw.bits.addr[lane_lsb:lane_lsb + lane_bits]),
            aw_cmd.enq.bits.last.eq(aw.bits.last),

            aw_data.enq.valid.eq(aw.valid & aw_cmd.enq.ready),
            aw_data.enq.bits.lane.eq(aw.bits.addr[lane_lsb:lane_lsb + lane_bits]),

            axi_to.aw.valid.eq(aw_cmd.deq.valid & b_info.enq.ready),
            axi_to.aw.bits.addr.eq(aw_cmd.deq.bits.addr),
            axi_to.aw.bits.burst.eq(AXIBurst.INCR),
            axi_to.aw.bits.len.eq(0),
            axi_to.aw.bits.size.eq(aw_cmd.deq.bits.size),
            axi_to.aw.bits.lock.eq(aw_cmd.deq.bits.lock),
            axi_to.aw.bits.prot.eq(aw_cmd.deq.bits.prot),
            axi_to.aw.bits.cache.eq(aw_cmd.deq.bits.cache),
            axi_to.aw.bits.qos.eq(aw_cmd.deq.bits.qos),
            axi_to.aw.bits.region.eq(aw_cmd.deq.bits.region),
            axi_to.aw.bits.id.eq(aw_cmd.deq.bits.id),
            aw_cmd.deq.ready.eq(axi_to.aw.ready & b_info.enq.ready),

            b_info.enq.valid.eq(aw_cmd.deq.valid & axi_to.aw.ready),
            b_info.enq.bits.id.eq(aw_cmd.deq.bits.id),
            b_info.enq.bits.last.eq(aw_cmd.deq.bits.last),
        ]
        if has_user:
            m.d.comb += [
                aw_cmd.enq.bits.user.eq(axi_from.aw.bits.user),
                axi_to.aw.bits.user.eq(aw_cmd.deq.bits.user),
            ]

        # -- Write data path ------------------------------------------------------------------
        m.d.comb += [
            axi_to.w.valid.eq(axi_from.w.valid & aw_data.deq.valid),
            axi_from.w.ready.eq(axi_to.w.ready & aw_data.deq.valid),
            aw_data.deq.ready.eq(axi_from.w.valid & axi_to.w.ready),

            axi_to.w.bits.data.eq(0),
            axi_to.w.bits.strb.eq(0),
            axi_to.w.bits.last.eq(1),
        ]
        if is_axi3:
            m.d.comb += axi_to.w.bits.id.eq(axi_from.w.bits.id)
        if has_user:
            m.d.comb += axi_to.w.bits.user.eq(axi_from.w.bits.user)
        for i in range(ratio):
            with m.If(aw_data.deq.bits.lane == i):
                m.d.comb += [
                    axi_to.w.bits.data[i * dw_from:(i + 1) * dw_from].eq(axi_from.w.bits.data),
                    axi_to.w.bits.strb[i * (dw_from // 8):(i + 1) * (dw_from // 8)].eq(
                        axi_from.w.bits.strb),
                ]

        # -- Write response path --------------------------------------------------------------
        b_resp = Signal(2, reset=AXIResp.OKAY)
        m.d.comb += [
            axi_from.b.valid.eq(axi_to.b.valid & b_info.deq.valid & b_info.deq.bits.last),
            axi_from.b.bits.resp.eq(
                Mux(axi_to.b.bits.resp != AXIResp.OKAY, axi_to.b.bits.resp, b_resp)),
            axi_from.b.bits.id.eq(axi_to.b.bits.id),
            axi_to.b.ready.eq(b_info.deq.valid &
                              (~b_info.deq.bits.last | axi_from.b.ready)),
            b_info.deq.ready.eq(axi_to.b.valid & axi_to.b.ready),
        ]
        if has_user:
            m.d.comb += axi_from.b.bits.user.eq(axi_to.b.bits.user)
        with m.If(axi_to.b.valid & axi_to.b.ready):
            with m.If(b_info.deq.bits.last):
                m.d.sync += b_resp.eq(AXIResp.OKAY)
            with m.Elif(axi_to.b.bits.resp != AXIResp.OKAY):
                m.d.sync += b_resp.eq(axi_to.b.bits.resp)

        # -- Read address path ----------------------------------------------------------------
        ar_b2b = m.submodules.ar_b2b = AXIBurst2Beat(axi_from.ar)
        ar = ar_b2b.out

        ar_cmd = m.submodules.ar_cmd = Queue(16, Record, ax_cmd_layout(), flow=False)
        r_info = m.submodules.r_info = Queue(
            16, Record, [('lane', lane_field), ('last', 1)], flow=False)
        r_fifo = m.submodules.r_fifo = Queue(
            16, Record, [
                ('data', dw_from),
                ('resp', len(axi_from.r.bits.resp)),
                ('id', len(axi_from.r.bits.id)),
                ('last', 1),
            ], flow=False)

        m.d.comb += [
            ar_cmd.enq.valid.eq(ar.valid),
            ar.ready.eq(ar_cmd.enq.ready),
            ar_cmd.enq.bits.addr.eq(ar.bits.addr),
            ar_cmd.enq.bits.size.eq(axi_from.ar.bits.size),
            ar_cmd.enq.bits.lock.eq(axi_from.ar.bits.lock),
            ar_cmd.enq.bits.prot.eq(axi_from.ar.bits.prot),
            ar_cmd.enq.bits.cache.eq(axi_from.ar.bits.cache),
            ar_cmd.enq.bits.qos.eq(axi_from.ar.bits.qos),
            ar_cmd.enq.bits.region.eq(axi_from.ar.bits.region),
            ar_cmd.enq.bits.id.eq(ar.bits.id),
            ar_cmd.enq.bits.lane.eq(ar.bits.addr[lane_lsb:lane_lsb + lane_bits]),
            ar_cmd.enq.bits.last.eq(ar.bits.last),

            axi_to.ar.valid.eq(ar_cmd.deq.valid & r_info.enq.ready),
            axi_to.ar.bits.addr.eq(ar_cmd.deq.bits.addr),
            axi_to.ar.bits.burst.eq(AXIBurst.INCR),
            axi_to.ar.bits.len.eq(0),
            axi_to.ar.bits.size.eq(ar_cmd.deq.bits.size),
            axi_to.ar.bits.lock.eq(ar_cmd.deq.bits.lock),
            axi_to.ar.bits.prot.eq(ar_cmd.deq.bits.prot),
            axi_to.ar.bits.cache.eq(ar_cmd.deq.bits.cache),
            axi_to.ar.bits.qos.eq(ar_cmd.deq.bits.qos),
            axi_to.ar.bits.region.eq(ar_cmd.deq.bits.region),
            axi_to.ar.bits.id.eq(ar_cmd.deq.bits.id),
            ar_cmd.deq.ready.eq(axi_to.ar.ready & r_info.enq.ready),

            r_info.enq.valid.eq(ar_cmd.deq.valid & axi_to.ar.ready),
            r_info.enq.bits.lane.eq(ar_cmd.deq.bits.lane),
            r_info.enq.bits.last.eq(ar_cmd.deq.bits.last),
        ]
        if has_user:
            m.d.comb += [
                ar_cmd.enq.bits.user.eq(axi_from.ar.bits.user),
                axi_to.ar.bits.user.eq(ar_cmd.deq.bits.user),
            ]

        # -- Read data path -------------------------------------------------------------------
        r_data = Signal(dw_from)
        for i in range(ratio):
            with m.If(r_info.deq.bits.lane == i):
                m.d.comb += r_data.eq(axi_to.r.bits.data[i * dw_from:(i + 1) * dw_from])

        m.d.comb += [
            r_fifo.enq.valid.eq(axi_to.r.valid & r_info.deq.valid),
            r_fifo.enq.bits.data.eq(r_data),
            r_fifo.enq.bits.resp.eq(axi_to.r.bits.resp),
            r_fifo.enq.bits.id.eq(axi_to.r.bits.id),
            r_fifo.enq.bits.last.eq(r_info.deq.bits.last),
            axi_to.r.ready.eq(r_info.deq.valid & r_fifo.enq.ready),
            r_info.deq.ready.eq(axi_to.r.valid & axi_to.r.ready),

            axi_from.r.valid.eq(r_fifo.deq.valid),
            axi_from.r.bits.data.eq(r_fifo.deq.bits.data),
            axi_from.r.bits.resp.eq(r_fifo.deq.bits.resp),
            axi_from.r.bits.last.eq(r_fifo.deq.bits.last),
            axi_from.r.bits.id.eq(r_fifo.deq.bits.id),
            r_fifo.deq.ready.eq(axi_from.r.ready),
        ]
        if has_user:
            m.d.comb += axi_from.r.bits.user.eq(axi_to.r.bits.user)

        return m


# AXI Data-Width Down Converter -------------------------------------------------------------------

class AXIDownConverter(Elaboratable):
    """AXI4 data-width down-converter (``dw_from > dw_to``, integer ratio).

    INCR/WRAP/FIXED-1 bursts take a combinational fast path: one wide AW/AR becomes one
    narrow AW/AR with a ratio-multiplied length, and the W/R streams are split/merged by
    ratio. FIXED bursts longer than one beat (and bursts too long to fit a narrow burst)
    cannot be expressed as a single narrow burst and take a slower FSM path that issues
    ``ratio``-beat narrow INCR bursts back-to-back, coalescing the B responses and
    re-timing the W/R ``last`` signals. Mirrors LiteX ``AXIDownConverter``.
    """

    def __init__(self, axi_from, axi_to):
        self.axi_from = axi_from
        self.axi_to = axi_to
        dw_from = axi_from.data_width
        dw_to = axi_to.data_width
        ratio = dw_from // dw_to
        if dw_from != dw_to * ratio:
            raise ValueError("AXI down-converter ratio must be an integer")
        self.ratio = ratio
        self.wide_size_log2 = log2_int(dw_from // 8)
        self.narrow_size_log2 = log2_int(dw_to // 8)

    def elaborate(self, platform):
        m = Module()

        axi_from = self.axi_from
        axi_to = self.axi_to
        ratio = self.ratio
        wide_size_log2 = self.wide_size_log2
        narrow_size_log2 = self.narrow_size_log2
        dw_from = axi_from.data_width
        dw_to = axi_to.data_width
        ratio_log2 = log2_int(ratio)

        has_user = len(axi_from.aw.bits.user) > 0
        is_axi3 = axi_from.version == 'axi3'
        wide_mask = C((1 << wide_size_log2) - 1, len(axi_to.aw.bits.addr))
        max_fast_len = (256 >> ratio_log2) - 1

        def burst_remap():
            with m.Switch(axi_from.aw.bits.burst):
                with m.Case(AXIBurst.FIXED):
                    m.d.comb += axi_to.aw.bits.burst.eq(AXIBurst.INCR)
                with m.Case(AXIBurst.INCR):
                    m.d.comb += axi_to.aw.bits.burst.eq(AXIBurst.INCR)
                with m.Case(AXIBurst.WRAP):
                    m.d.comb += axi_to.aw.bits.burst.eq(AXIBurst.WRAP)
                with m.Case(AXIBurst.RESERVED):
                    m.d.comb += axi_to.aw.bits.burst.eq(AXIBurst.RESERVED)

        # ============================ Write path: AW / W / B ============================

        cap_aw_addr = Signal.like(axi_from.aw.bits.addr)
        cap_aw_len = Signal.like(axi_from.aw.bits.len)
        cap_aw_size = Signal.like(axi_from.aw.bits.size)
        cap_aw_id = Signal.like(axi_from.aw.bits.id)
        cap_aw_lock = Signal.like(axi_from.aw.bits.lock)
        cap_aw_prot = Signal.like(axi_from.aw.bits.prot)
        cap_aw_cache = Signal.like(axi_from.aw.bits.cache)
        cap_aw_qos = Signal.like(axi_from.aw.bits.qos)
        cap_aw_region = Signal.like(axi_from.aw.bits.region)
        cap_aw_lane = Signal(range(max(ratio, 2)))

        aw_emit_count = Signal.like(axi_from.aw.bits.len)
        b_collected_resp = Signal(2)
        w_subbeat_count = Signal(range(max(ratio, 2)))

        cap_aw_incr = Signal()
        fixed_aw_active = Signal()
        narrow_aw_active = Signal()
        aw_narrow_pending = Signal()

        is_aw_narrow = (axi_from.aw.bits.len == 0) & (axi_from.aw.bits.size <= narrow_size_log2)
        is_aw_slow = (
            (axi_from.aw.bits.burst == AXIBurst.FIXED) & (axi_from.aw.bits.len != 0)
        ) | (axi_from.aw.bits.len > max_fast_len)

        with m.FSM():
            with m.State('IDLE'):
                with m.If(narrow_aw_active):
                    m.d.comb += axi_from.aw.ready.eq(0)
                with m.Elif(is_aw_narrow):
                    m.d.comb += [
                        aw_narrow_pending.eq(axi_from.aw.valid),
                        axi_to.aw.valid.eq(axi_from.aw.valid),
                        axi_to.aw.bits.addr.eq(axi_from.aw.bits.addr),
                        axi_to.aw.bits.len.eq(0),
                        axi_to.aw.bits.size.eq(axi_from.aw.bits.size),
                        axi_to.aw.bits.burst.eq(AXIBurst.INCR),
                        axi_to.aw.bits.id.eq(axi_from.aw.bits.id),
                        axi_to.aw.bits.lock.eq(axi_from.aw.bits.lock),
                        axi_to.aw.bits.prot.eq(axi_from.aw.bits.prot),
                        axi_to.aw.bits.cache.eq(axi_from.aw.bits.cache),
                        axi_to.aw.bits.qos.eq(axi_from.aw.bits.qos),
                        axi_to.aw.bits.region.eq(axi_from.aw.bits.region),
                        axi_from.aw.ready.eq(axi_to.aw.ready),
                    ]
                    if has_user:
                        m.d.comb += axi_to.aw.bits.user.eq(axi_from.aw.bits.user)
                    with m.If(axi_from.aw.valid & axi_to.aw.ready):
                        m.d.sync += [
                            cap_aw_lane.eq(
                                axi_from.aw.bits.addr[narrow_size_log2:wide_size_log2]),
                            narrow_aw_active.eq(1),
                        ]
                with m.Elif(~is_aw_slow):
                    m.d.comb += [
                        axi_to.aw.valid.eq(axi_from.aw.valid),
                        axi_to.aw.bits.addr.eq(axi_from.aw.bits.addr & ~wide_mask),
                        axi_to.aw.bits.len.eq(
                            ((axi_from.aw.bits.len + 1) << ratio_log2) - 1),
                        axi_to.aw.bits.size.eq(narrow_size_log2),
                        axi_to.aw.bits.id.eq(axi_from.aw.bits.id),
                        axi_to.aw.bits.lock.eq(axi_from.aw.bits.lock),
                        axi_to.aw.bits.prot.eq(axi_from.aw.bits.prot),
                        axi_to.aw.bits.cache.eq(axi_from.aw.bits.cache),
                        axi_to.aw.bits.qos.eq(axi_from.aw.bits.qos),
                        axi_to.aw.bits.region.eq(axi_from.aw.bits.region),
                        axi_from.aw.ready.eq(axi_to.aw.ready),
                    ]
                    burst_remap()
                    if has_user:
                        m.d.comb += axi_to.aw.bits.user.eq(axi_from.aw.bits.user)
                with m.Else():
                    m.d.comb += axi_from.aw.ready.eq(1)
                    with m.If(axi_from.aw.valid):
                        m.d.sync += [
                            cap_aw_addr.eq(axi_from.aw.bits.addr),
                            cap_aw_len.eq(axi_from.aw.bits.len),
                            cap_aw_size.eq(axi_from.aw.bits.size),
                            cap_aw_id.eq(axi_from.aw.bits.id),
                            cap_aw_lock.eq(axi_from.aw.bits.lock),
                            cap_aw_prot.eq(axi_from.aw.bits.prot),
                            cap_aw_cache.eq(axi_from.aw.bits.cache),
                            cap_aw_qos.eq(axi_from.aw.bits.qos),
                            cap_aw_region.eq(axi_from.aw.bits.region),
                            cap_aw_incr.eq(axi_from.aw.bits.burst == AXIBurst.INCR),
                            aw_emit_count.eq(0),
                            b_collected_resp.eq(AXIResp.OKAY),
                        ]
                        m.next = 'FIXED-EMIT-AW'

            with m.State('FIXED-EMIT-AW'):
                m.d.comb += [
                    fixed_aw_active.eq(1),
                    axi_to.aw.valid.eq(1),
                    axi_to.aw.bits.addr.eq(
                        (cap_aw_addr + Mux(cap_aw_incr, aw_emit_count << wide_size_log2, 0))
                        & ~wide_mask),
                    axi_to.aw.bits.len.eq(ratio - 1),
                    axi_to.aw.bits.burst.eq(AXIBurst.INCR),
                    axi_to.aw.bits.size.eq(narrow_size_log2),
                    axi_to.aw.bits.id.eq(cap_aw_id),
                    axi_to.aw.bits.lock.eq(cap_aw_lock),
                    axi_to.aw.bits.prot.eq(cap_aw_prot),
                    axi_to.aw.bits.cache.eq(cap_aw_cache),
                    axi_to.aw.bits.qos.eq(cap_aw_qos),
                    axi_to.aw.bits.region.eq(cap_aw_region),
                ]
                with m.If(axi_to.aw.ready):
                    m.next = 'FIXED-WAIT-B'

            with m.State('FIXED-WAIT-B'):
                m.d.comb += [
                    fixed_aw_active.eq(1),
                    axi_to.b.ready.eq(1),
                ]
                with m.If(axi_to.b.valid):
                    with m.If(axi_to.b.bits.resp > b_collected_resp):
                        m.d.sync += b_collected_resp.eq(axi_to.b.bits.resp)
                    with m.If(aw_emit_count == cap_aw_len):
                        m.next = 'FIXED-EMIT-B'
                    with m.Else():
                        m.d.sync += aw_emit_count.eq(aw_emit_count + 1)
                        m.next = 'FIXED-EMIT-AW'

            with m.State('FIXED-EMIT-B'):
                m.d.comb += [
                    fixed_aw_active.eq(1),
                    axi_from.b.valid.eq(1),
                    axi_from.b.bits.id.eq(cap_aw_id),
                    axi_from.b.bits.resp.eq(b_collected_resp),
                ]
                with m.If(axi_from.b.ready):
                    m.next = 'IDLE'

        # W channel.
        w_conv = m.submodules.w_conv = _StrideDown(dw_from, dw_to)

        narrow_w_data = Signal(dw_to)
        narrow_w_strb = Signal(dw_to // 8)
        m.d.comb += [
            narrow_w_data.eq(0),
            narrow_w_strb.eq(0),
        ]
        with m.Switch(cap_aw_lane):
            for i in range(ratio):
                with m.Case(i):
                    m.d.comb += [
                        narrow_w_data.eq(axi_from.w.bits.data[i * dw_to:(i + 1) * dw_to]),
                        narrow_w_strb.eq(
                            axi_from.w.bits.strb[i * (dw_to // 8):(i + 1) * (dw_to // 8)]),
                    ]

        m.d.comb += [
            w_conv.sink.valid.eq(
                axi_from.w.valid & ~narrow_aw_active & ~aw_narrow_pending),
            w_conv.sink.bits.data.eq(axi_from.w.bits.data),
            w_conv.sink.bits.strb.eq(axi_from.w.bits.strb),
            w_conv.sink.bits.last.eq(axi_from.w.bits.last),
        ]
        with m.If(narrow_aw_active):
            m.d.comb += [
                axi_to.w.valid.eq(axi_from.w.valid),
                axi_to.w.bits.data.eq(narrow_w_data),
                axi_to.w.bits.strb.eq(narrow_w_strb),
                axi_to.w.bits.last.eq(axi_from.w.bits.last),
                axi_from.w.ready.eq(axi_to.w.ready),
                w_conv.source.ready.eq(0),
            ]
            if is_axi3:
                m.d.comb += axi_to.w.bits.id.eq(axi_from.w.bits.id)
            if has_user:
                m.d.comb += axi_to.w.bits.user.eq(axi_from.w.bits.user)
        with m.Else():
            m.d.comb += [
                axi_to.w.valid.eq(w_conv.source.valid),
                axi_to.w.bits.data.eq(w_conv.source.bits.data),
                axi_to.w.bits.strb.eq(w_conv.source.bits.strb),
                w_conv.source.ready.eq(axi_to.w.ready),
                axi_from.w.ready.eq(Mux(aw_narrow_pending, 0, w_conv.sink.ready)),
            ]
            if is_axi3:
                m.d.comb += axi_to.w.bits.id.eq(axi_from.w.bits.id)
            if has_user:
                m.d.comb += axi_to.w.bits.user.eq(axi_from.w.bits.user)
            with m.If(fixed_aw_active):
                m.d.comb += axi_to.w.bits.last.eq(w_subbeat_count == ratio - 1)
            with m.Else():
                m.d.comb += axi_to.w.bits.last.eq(w_conv.source.bits.last)

        with m.If(axi_to.w.valid & axi_to.w.ready & ~narrow_aw_active):
            with m.If(w_subbeat_count == ratio - 1):
                m.d.sync += w_subbeat_count.eq(0)
            with m.Else():
                m.d.sync += w_subbeat_count.eq(w_subbeat_count + 1)

        # B channel: pass-through except the FIXED FSM drives it directly.
        with m.If(narrow_aw_active):
            m.d.comb += [
                axi_from.b.valid.eq(axi_to.b.valid),
                axi_to.b.ready.eq(axi_from.b.ready),
                axi_from.b.bits.id.eq(axi_to.b.bits.id),
                axi_from.b.bits.resp.eq(axi_to.b.bits.resp),
            ]
            if has_user:
                m.d.comb += axi_from.b.bits.user.eq(axi_to.b.bits.user)
        with m.Elif(~fixed_aw_active):
            m.d.comb += [
                axi_from.b.valid.eq(axi_to.b.valid),
                axi_to.b.ready.eq(axi_from.b.ready),
                axi_from.b.bits.id.eq(axi_to.b.bits.id),
                axi_from.b.bits.resp.eq(axi_to.b.bits.resp),
            ]
            if has_user:
                m.d.comb += axi_from.b.bits.user.eq(axi_to.b.bits.user)
        with m.If(narrow_aw_active & axi_to.b.valid & axi_from.b.ready):
            m.d.sync += narrow_aw_active.eq(0)

        # ============================= Read path: AR / R ===============================

        cap_ar_addr = Signal.like(axi_from.ar.bits.addr)
        cap_ar_len = Signal.like(axi_from.ar.bits.len)
        cap_ar_size = Signal.like(axi_from.ar.bits.size)
        cap_ar_id = Signal.like(axi_from.ar.bits.id)
        cap_ar_lock = Signal.like(axi_from.ar.bits.lock)
        cap_ar_prot = Signal.like(axi_from.ar.bits.prot)
        cap_ar_cache = Signal.like(axi_from.ar.bits.cache)
        cap_ar_qos = Signal.like(axi_from.ar.bits.qos)
        cap_ar_region = Signal.like(axi_from.ar.bits.region)
        cap_ar_lane = Signal(range(max(ratio, 2)))

        ar_emit_count = Signal.like(axi_from.ar.bits.len)
        r_wide_count = Signal.like(axi_from.ar.bits.len)

        cap_ar_incr = Signal()
        fixed_ar_active = Signal()
        narrow_ar_active = Signal()
        narrow_r_valid = Signal()
        narrow_r_data = Signal(dw_to)
        narrow_r_resp = Signal.like(axi_from.r.bits.resp)
        narrow_r_id = Signal.like(axi_from.r.bits.id)

        is_ar_narrow = (axi_from.ar.bits.len == 0) & (axi_from.ar.bits.size <= narrow_size_log2)
        is_ar_slow = (
            (axi_from.ar.bits.burst == AXIBurst.FIXED) & (axi_from.ar.bits.len != 0)
        ) | (axi_from.ar.bits.len > max_fast_len)

        with m.FSM():
            with m.State('IDLE'):
                with m.If(narrow_ar_active):
                    m.d.comb += axi_from.ar.ready.eq(0)
                with m.Elif(is_ar_narrow):
                    m.d.comb += [
                        axi_to.ar.valid.eq(axi_from.ar.valid),
                        axi_to.ar.bits.addr.eq(axi_from.ar.bits.addr),
                        axi_to.ar.bits.len.eq(0),
                        axi_to.ar.bits.size.eq(axi_from.ar.bits.size),
                        axi_to.ar.bits.burst.eq(AXIBurst.INCR),
                        axi_to.ar.bits.id.eq(axi_from.ar.bits.id),
                        axi_to.ar.bits.lock.eq(axi_from.ar.bits.lock),
                        axi_to.ar.bits.prot.eq(axi_from.ar.bits.prot),
                        axi_to.ar.bits.cache.eq(axi_from.ar.bits.cache),
                        axi_to.ar.bits.qos.eq(axi_from.ar.bits.qos),
                        axi_to.ar.bits.region.eq(axi_from.ar.bits.region),
                        axi_from.ar.ready.eq(axi_to.ar.ready),
                    ]
                    if has_user:
                        m.d.comb += axi_to.ar.bits.user.eq(axi_from.ar.bits.user)
                    with m.If(axi_from.ar.valid & axi_to.ar.ready):
                        m.d.sync += [
                            cap_ar_lane.eq(
                                axi_from.ar.bits.addr[narrow_size_log2:wide_size_log2]),
                            narrow_ar_active.eq(1),
                        ]
                with m.Elif(~is_ar_slow):
                    m.d.comb += [
                        axi_to.ar.valid.eq(axi_from.ar.valid),
                        axi_to.ar.bits.addr.eq(axi_from.ar.bits.addr & ~wide_mask),
                        axi_to.ar.bits.len.eq(
                            ((axi_from.ar.bits.len + 1) << ratio_log2) - 1),
                        axi_to.ar.bits.size.eq(narrow_size_log2),
                        axi_to.ar.bits.id.eq(axi_from.ar.bits.id),
                        axi_to.ar.bits.lock.eq(axi_from.ar.bits.lock),
                        axi_to.ar.bits.prot.eq(axi_from.ar.bits.prot),
                        axi_to.ar.bits.cache.eq(axi_from.ar.bits.cache),
                        axi_to.ar.bits.qos.eq(axi_from.ar.bits.qos),
                        axi_to.ar.bits.region.eq(axi_from.ar.bits.region),
                        axi_from.ar.ready.eq(axi_to.ar.ready),
                    ]
                    with m.Switch(axi_from.ar.bits.burst):
                        with m.Case(AXIBurst.FIXED):
                            m.d.comb += axi_to.ar.bits.burst.eq(AXIBurst.INCR)
                        with m.Case(AXIBurst.INCR):
                            m.d.comb += axi_to.ar.bits.burst.eq(AXIBurst.INCR)
                        with m.Case(AXIBurst.WRAP):
                            m.d.comb += axi_to.ar.bits.burst.eq(AXIBurst.WRAP)
                        with m.Case(AXIBurst.RESERVED):
                            m.d.comb += axi_to.ar.bits.burst.eq(AXIBurst.RESERVED)
                    if has_user:
                        m.d.comb += axi_to.ar.bits.user.eq(axi_from.ar.bits.user)
                with m.Else():
                    m.d.comb += axi_from.ar.ready.eq(1)
                    with m.If(axi_from.ar.valid):
                        m.d.sync += [
                            cap_ar_addr.eq(axi_from.ar.bits.addr),
                            cap_ar_len.eq(axi_from.ar.bits.len),
                            cap_ar_size.eq(axi_from.ar.bits.size),
                            cap_ar_id.eq(axi_from.ar.bits.id),
                            cap_ar_lock.eq(axi_from.ar.bits.lock),
                            cap_ar_prot.eq(axi_from.ar.bits.prot),
                            cap_ar_cache.eq(axi_from.ar.bits.cache),
                            cap_ar_qos.eq(axi_from.ar.bits.qos),
                            cap_ar_region.eq(axi_from.ar.bits.region),
                            cap_ar_incr.eq(axi_from.ar.bits.burst == AXIBurst.INCR),
                            ar_emit_count.eq(0),
                            r_wide_count.eq(0),
                        ]
                        m.next = 'FIXED-EMIT-AR'

            with m.State('FIXED-EMIT-AR'):
                m.d.comb += [
                    fixed_ar_active.eq(1),
                    axi_to.ar.valid.eq(1),
                    axi_to.ar.bits.addr.eq(
                        (cap_ar_addr + Mux(cap_ar_incr, ar_emit_count << wide_size_log2, 0))
                        & ~wide_mask),
                    axi_to.ar.bits.len.eq(ratio - 1),
                    axi_to.ar.bits.burst.eq(AXIBurst.INCR),
                    axi_to.ar.bits.size.eq(narrow_size_log2),
                    axi_to.ar.bits.id.eq(cap_ar_id),
                    axi_to.ar.bits.lock.eq(cap_ar_lock),
                    axi_to.ar.bits.prot.eq(cap_ar_prot),
                    axi_to.ar.bits.cache.eq(cap_ar_cache),
                    axi_to.ar.bits.qos.eq(cap_ar_qos),
                    axi_to.ar.bits.region.eq(cap_ar_region),
                ]
                with m.If(axi_to.ar.ready):
                    m.next = 'FIXED-DRAIN-R'

            with m.State('FIXED-DRAIN-R'):
                m.d.comb += fixed_ar_active.eq(1)
                with m.If(axi_from.r.valid & axi_from.r.ready):
                    with m.If(ar_emit_count == cap_ar_len):
                        m.next = 'IDLE'
                    with m.Else():
                        m.d.sync += ar_emit_count.eq(ar_emit_count + 1)
                        m.next = 'FIXED-EMIT-AR'

        # R channel.
        r_conv = m.submodules.r_conv = _StrideUp(dw_to, dw_from)

        full_r_resp = Signal.like(axi_from.r.bits.resp)
        full_r_id = Signal.like(axi_from.r.bits.id)

        narrow_r_wide_data = Signal(dw_from)
        m.d.comb += narrow_r_wide_data.eq(0)
        with m.Switch(cap_ar_lane):
            for i in range(ratio):
                with m.Case(i):
                    m.d.comb += narrow_r_wide_data[i * dw_to:(i + 1) * dw_to].eq(narrow_r_data)

        m.d.comb += [
            r_conv.sink.valid.eq(axi_to.r.valid & ~narrow_ar_active),
            r_conv.sink.bits.data.eq(axi_to.r.bits.data),
            r_conv.sink.bits.last.eq(axi_to.r.bits.last),
        ]
        with m.If(narrow_ar_active):
            m.d.comb += [
                axi_to.r.ready.eq(~narrow_r_valid),
                r_conv.source.ready.eq(0),
            ]
        with m.Else():
            m.d.comb += [
                axi_to.r.ready.eq(r_conv.sink.ready),
                r_conv.source.ready.eq(axi_from.r.ready),
            ]

        with m.If(narrow_r_valid):
            m.d.comb += [
                axi_from.r.valid.eq(1),
                axi_from.r.bits.data.eq(narrow_r_wide_data),
                axi_from.r.bits.resp.eq(narrow_r_resp),
                axi_from.r.bits.last.eq(1),
                axi_from.r.bits.id.eq(narrow_r_id),
            ]
        with m.Else():
            m.d.comb += [
                axi_from.r.valid.eq(r_conv.source.valid),
                axi_from.r.bits.data.eq(r_conv.source.bits.data),
                axi_from.r.bits.resp.eq(full_r_resp),
                axi_from.r.bits.id.eq(full_r_id),
            ]
            with m.If(fixed_ar_active):
                m.d.comb += axi_from.r.bits.last.eq(r_wide_count == cap_ar_len)
            with m.Else():
                m.d.comb += axi_from.r.bits.last.eq(r_conv.source.bits.last)
        if has_user:
            with m.If(narrow_r_valid):
                m.d.comb += axi_from.r.bits.user.eq(0)
            with m.Else():
                m.d.comb += axi_from.r.bits.user.eq(axi_to.r.bits.user)

        # Sub-beat tracking and id/resp accumulation.
        r_sub_count = Signal(range(max(ratio, 2)))
        with m.If(narrow_ar_active & axi_to.r.valid & axi_to.r.ready):
            m.d.sync += [
                narrow_r_valid.eq(1),
                narrow_r_data.eq(axi_to.r.bits.data),
                narrow_r_resp.eq(axi_to.r.bits.resp),
                narrow_r_id.eq(axi_to.r.bits.id),
            ]
        with m.If(narrow_r_valid & axi_from.r.ready):
            m.d.sync += [
                narrow_r_valid.eq(0),
                narrow_ar_active.eq(0),
            ]
        with m.If(axi_to.r.valid & axi_to.r.ready & ~narrow_ar_active):
            with m.If(r_sub_count == 0):
                m.d.sync += [
                    full_r_id.eq(axi_to.r.bits.id),
                    full_r_resp.eq(axi_to.r.bits.resp),
                ]
            with m.Elif(axi_to.r.bits.resp > full_r_resp):
                m.d.sync += full_r_resp.eq(axi_to.r.bits.resp)
            with m.If(r_sub_count == ratio - 1):
                m.d.sync += r_sub_count.eq(0)
            with m.Else():
                m.d.sync += r_sub_count.eq(r_sub_count + 1)

        with m.If(axi_from.r.valid & axi_from.r.ready):
            with m.If(fixed_ar_active):
                with m.If(r_wide_count == cap_ar_len):
                    m.d.sync += r_wide_count.eq(0)
                with m.Else():
                    m.d.sync += r_wide_count.eq(r_wide_count + 1)

        return m


# AXI Data-Width Converter ------------------------------------------------------------------------

class AXIConverter(Elaboratable):
    """AXI data-width converter.

    Connects ``master`` to ``slave``, transparently up- or down-converting the data
    width (which must differ by an integer ratio). Equal widths are wired directly.
    """

    def __init__(self, master, slave):
        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        dw_from = self.master.data_width
        dw_to = self.slave.data_width

        if dw_from > dw_to:
            m.submodules.conv = AXIDownConverter(self.master, self.slave)
        elif dw_from < dw_to:
            m.submodules.conv = AXIUpConverter(self.master, self.slave)
        else:
            m.d.comb += self.master.connect(self.slave)

        return m
