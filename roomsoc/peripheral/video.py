from amaranth import *
from amaranth.hdl.rec import *
from amaranth.lib.cdc import FFSynchronizer
from amaranth.utils import log2_int

from roomsoc.interconnect.stream import Decoupled, ClockDomainCrossing, Gearbox, SkidBuffer

from .peripheral import Peripheral

H_BITS = 12
V_BITS = 12

video_timings = {
    "640x480@60Hz": {
        "pix_clk": 25.175e6,
        "h_active": 640,
        "h_blanking": 160,
        "h_sync_offset": 16,
        "h_sync_width": 96,
        "v_active": 480,
        "v_blanking": 45,
        "v_sync_offset": 10,
        "v_sync_width": 2,
    },
    "1024x600@60Hz": {
        "pix_clk": 49e6,
        "h_active": 1024,
        "h_blanking": 288,
        "h_sync_offset": 40,
        "h_sync_width": 104,
        "v_active": 600,
        "v_blanking": 24,
        "v_sync_offset": 3,
        "v_sync_width": 10,
    },
    "1920x1080@60Hz": {
        "pix_clk": 148.5e6,
        "h_active": 1920,
        "h_blanking": 280,
        "h_sync_offset": 88,
        "h_sync_width": 44,
        "v_active": 1080,
        "v_blanking": 45,
        "v_sync_offset": 4,
        "v_sync_width": 5,
    },
}

video_timing_layout = [
    ("hsync", 1, DIR_FANOUT),
    ("vsync", 1, DIR_FANOUT),
    ("de", 1, DIR_FANOUT),
    ("hres", H_BITS, DIR_FANOUT),
    ("vres", V_BITS, DIR_FANOUT),
    ("hcount", H_BITS, DIR_FANOUT),
    ("vcount", V_BITS, DIR_FANOUT),
]

video_data_layout = [
    ("hsync", 1, DIR_FANOUT),
    ("vsync", 1, DIR_FANOUT),
    ("de", 1, DIR_FANOUT),
    ("r", 8, DIR_FANOUT),
    ("g", 8, DIR_FANOUT),
    ("b", 8, DIR_FANOUT),
]

_dvi_c2d = {'r': 2, 'g': 1, 'b': 0}


class TMDSEncoder(Elaboratable):

    CONTROL_TOKENS = [0b1101010100, 0b0010101011, 0b0101010100, 0b1010101011]

    def __init__(self):
        self.d = Signal(8)
        self.c = Signal(2)
        self.de = Signal()

        self.out = Signal(10)

    def elaborate(self, platform):
        m = Module()

        def sum_bits(d):
            acc = 0
            for i in range(len(d)):
                acc += d[i]
            return acc

        d = Signal(8)
        n1d = Signal(range(9))
        m.d.sync += [
            n1d.eq(sum_bits(self.d)),
            d.eq(self.d),
        ]

        q_m = Signal(9)
        q_m8_n = Signal()
        m.d.comb += q_m8_n.eq((n1d > 4) | ((n1d == 4) & ~d[0]))

        for i in range(8):
            if i:
                curval = curval ^ d[i] ^ q_m8_n
            else:
                curval = d[0]
            m.d.sync += q_m[i].eq(curval)

        m.d.sync += q_m[8].eq(~q_m8_n)

        q_m_r = Signal(9)
        n0q_m = Signal(range(9))
        n1q_m = Signal(range(9))
        m.d.sync += [
            n0q_m.eq(sum_bits(~q_m[:8])),
            n1q_m.eq(sum_bits(q_m[:8])),
            q_m_r.eq(q_m),
        ]

        cnt = Signal(signed(6))

        s_c = self.c
        s_de = self.de
        for _ in range(3):
            new_c = Signal(2)
            new_de = Signal()
            m.d.sync += [
                new_c.eq(s_c),
                new_de.eq(s_de),
            ]
            s_c, s_de = new_c, new_de

        with m.If(s_de):
            with m.If((cnt == 0) | (n1q_m == n0q_m)):
                m.d.sync += [
                    self.out[9].eq(~q_m_r[8]),
                    self.out[8].eq(q_m_r[8]),
                ]

                with m.If(q_m_r[8]):
                    m.d.sync += [
                        self.out[:8].eq(q_m_r[:8]),
                        cnt.eq(cnt + n1q_m - n0q_m),
                    ]
                with m.Else():
                    m.d.sync += [
                        self.out[:8].eq(~q_m_r[:8]),
                        cnt.eq(cnt + n0q_m - n1q_m),
                    ]
            with m.Else():
                with m.If((~cnt[5] & (n1q_m > n0q_m))
                          | (cnt[5] & (n0q_m > n1q_m))):
                    m.d.sync += [
                        self.out[9].eq(1),
                        self.out[8].eq(q_m_r[8]),
                        self.out[:8].eq(~q_m_r[:8]),
                        cnt.eq(cnt + Cat(0, q_m_r[8]) + n0q_m - n1q_m),
                    ]
                with m.Else():
                    m.d.sync += [
                        self.out[9].eq(0),
                        self.out[8].eq(q_m_r[8]),
                        self.out[:8].eq(q_m_r[:8]),
                        cnt.eq(cnt - Cat(0, ~q_m_r[8]) + n1q_m - n0q_m),
                    ]
        with m.Else():
            with m.Switch(s_c):
                for i, token in enumerate(self.CONTROL_TOKENS):
                    with m.Case(i):
                        m.d.sync += [
                            self.out.eq(token),
                            cnt.eq(0),
                        ]

        return m


class VideoTimingGenerator(Peripheral, Elaboratable):

    def __init__(self,
                 *,
                 name=None,
                 clock_domain="sync",
                 default_video_timings="1920x1080@60Hz"):
        super().__init__(name=name)

        self.clock_domain = clock_domain

        if isinstance(default_video_timings, str):
            try:
                self.video_timings = video_timings[default_video_timings]
            except KeyError:
                msg = [
                    f"Video Timings {default_video_timings} not supported, availables:"
                ]
                for video_timing in video_timings.keys():
                    msg.append(
                        f" - {video_timing} / {video_timings[video_timing]['pix_clk']/1e6:3.2f}MHz."
                    )
                raise ValueError("\n".join(msg))
        else:
            self.video_timings = default_video_timings

        bank = self.csr_bank()
        self._enable = bank.csr(1, 'rw')

        self._hres = bank.csr(H_BITS, 'rw')
        self._hsync_start = bank.csr(H_BITS, 'rw')
        self._hsync_end = bank.csr(H_BITS, 'rw')
        self._hscan = bank.csr(H_BITS, 'rw')

        self._vres = bank.csr(V_BITS, 'rw')
        self._vsync_start = bank.csr(V_BITS, 'rw')
        self._vsync_end = bank.csr(V_BITS, 'rw')
        self._vscan = bank.csr(V_BITS, 'rw')

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

        self.source = Decoupled(Record, video_timing_layout)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        _enable = Signal(reset=1)

        _hres = Signal(H_BITS, reset=self.video_timings["h_active"])
        _hsync_start = Signal(H_BITS,
                              reset=self.video_timings["h_active"] +
                              self.video_timings["h_sync_offset"])
        _hsync_end = Signal(H_BITS,
                            reset=self.video_timings["h_active"] +
                            self.video_timings["h_sync_offset"] +
                            self.video_timings["h_sync_width"])
        _hscan = Signal(H_BITS,
                        reset=self.video_timings["h_active"] +
                        self.video_timings["h_blanking"] - 1)

        _vres = Signal(V_BITS, reset=self.video_timings["v_active"])
        _vsync_start = Signal(V_BITS,
                              reset=self.video_timings["v_active"] +
                              self.video_timings["v_sync_offset"])
        _vsync_end = Signal(V_BITS,
                            reset=self.video_timings["v_active"] +
                            self.video_timings["v_sync_offset"] +
                            self.video_timings["v_sync_width"])
        _vscan = Signal(V_BITS,
                        reset=self.video_timings["v_active"] +
                        self.video_timings["v_blanking"] - 1)

        m.d.comb += [
            self._enable.r_data.eq(_enable),
            self._hres.r_data.eq(_hres),
            self._hsync_start.r_data.eq(_hsync_start),
            self._hsync_end.r_data.eq(_hsync_end),
            self._hscan.r_data.eq(_hscan),
            self._vres.r_data.eq(_vres),
            self._vsync_start.r_data.eq(_vsync_start),
            self._vsync_end.r_data.eq(_vsync_end),
            self._vscan.r_data.eq(_vscan),
        ]

        with m.If(self._enable.w_stb):
            m.d.sync += _enable.eq(self._enable.w_data)

        with m.If(self._hres.w_stb):
            m.d.sync += _hres.eq(self._hres.w_data)
        with m.If(self._hsync_start.w_stb):
            m.d.sync += _hsync_start.eq(self._hsync_start.w_data)
        with m.If(self._hsync_end.w_stb):
            m.d.sync += _hsync_end.eq(self._hsync_end.w_data)
        with m.If(self._hscan.w_stb):
            m.d.sync += _hscan.eq(self._hscan.w_data)

        with m.If(self._vres.w_stb):
            m.d.sync += _vres.eq(self._vres.w_data)
        with m.If(self._vsync_start.w_stb):
            m.d.sync += _vsync_start.eq(self._vsync_start.w_data)
        with m.If(self._vsync_end.w_stb):
            m.d.sync += _vsync_end.eq(self._vsync_end.w_data)
        with m.If(self._vscan.w_stb):
            m.d.sync += _vscan.eq(self._vscan.w_data)

        if self.clock_domain == 'sync':
            enable = _enable
            hres = _hres
            hsync_start = _hsync_start
            hsync_end = _hsync_end
            hscan = _hscan
            vres = _vres
            vsync_start = _vsync_start
            vsync_end = _vsync_end
            vscan = _vscan
        else:
            enable = Signal()

            hres = Signal.like(_hres)
            hsync_start = Signal.like(_hsync_start)
            hsync_end = Signal.like(_hsync_end)
            hscan = Signal.like(_hscan)

            vres = Signal.like(_vres)
            vsync_start = Signal.like(_vsync_start)
            vsync_end = Signal.like(_vsync_end)
            vscan = Signal.like(_vscan)

            m.submodules += FFSynchronizer(_enable,
                                           enable,
                                           o_domain=self.clock_domain)

            m.submodules += FFSynchronizer(_hres,
                                           hres,
                                           o_domain=self.clock_domain)
            m.submodules += FFSynchronizer(_hsync_start,
                                           hsync_start,
                                           o_domain=self.clock_domain)
            m.submodules += FFSynchronizer(_hsync_end,
                                           hsync_end,
                                           o_domain=self.clock_domain)
            m.submodules += FFSynchronizer(_hscan,
                                           hscan,
                                           o_domain=self.clock_domain)

            m.submodules += FFSynchronizer(_vres,
                                           vres,
                                           o_domain=self.clock_domain)
            m.submodules += FFSynchronizer(_vsync_start,
                                           vsync_start,
                                           o_domain=self.clock_domain)
            m.submodules += FFSynchronizer(_vsync_end,
                                           vsync_end,
                                           o_domain=self.clock_domain)
            m.submodules += FFSynchronizer(_vscan,
                                           vscan,
                                           o_domain=self.clock_domain)

        hactive = Signal()
        vactive = Signal()
        m.d.comb += self.source.bits.de.eq(hactive & vactive)

        with m.FSM(domain=self.clock_domain):
            with m.State('IDLE'):
                m.d[self.clock_domain] += [
                    hactive.eq(0),
                    vactive.eq(0),
                    self.source.bits.hres.eq(hres),
                    self.source.bits.vres.eq(vres),
                    self.source.bits.hcount.eq(0),
                    self.source.bits.vcount.eq(0),
                ]

                with m.If(enable):
                    m.next = 'RUN'

            with m.State('RUN'):
                with m.If(~enable):
                    m.next = 'IDLE'
                with m.Else():
                    m.d.comb += self.source.valid.eq(1)

                    with m.If(self.source.ready):
                        m.d[self.clock_domain] += self.source.bits.hcount.eq(
                            self.source.bits.hcount + 1)

                        with m.If(self.source.bits.hcount == 0):
                            m.d[self.clock_domain] += hactive.eq(1)
                        with m.If(self.source.bits.hcount == hres):
                            m.d[self.clock_domain] += hactive.eq(0)
                        with m.If(self.source.bits.hcount == hsync_start):
                            m.d[self.
                                clock_domain] += self.source.bits.hsync.eq(1)
                        with m.If(self.source.bits.hcount == hsync_end):
                            m.d[self.
                                clock_domain] += self.source.bits.hsync.eq(0)
                        with m.If(self.source.bits.hcount == hscan):
                            m.d[self.
                                clock_domain] += self.source.bits.hcount.eq(0)

                        with m.If(self.source.bits.hcount == hsync_start):
                            m.d[self.
                                clock_domain] += self.source.bits.vcount.eq(
                                    self.source.bits.vcount + 1)

                            with m.If(self.source.bits.vcount == 0):
                                m.d[self.clock_domain] += vactive.eq(1)
                            with m.If(self.source.bits.vcount == vres):
                                m.d[self.clock_domain] += vactive.eq(0)
                            with m.If(self.source.bits.vcount == vsync_start):
                                m.d[self.
                                    clock_domain] += self.source.bits.vsync.eq(
                                        1)
                            with m.If(self.source.bits.vcount == vsync_end):
                                m.d[self.
                                    clock_domain] += self.source.bits.vsync.eq(
                                        0)
                            with m.If(self.source.bits.vcount == vscan):
                                m.d[self.
                                    clock_domain] += self.source.bits.vcount.eq(
                                        0)

        return m


class ColorBarsPattern(Elaboratable):

    def __init__(self):
        self.vtg = Decoupled(Record, video_timing_layout)
        self.source = Decoupled(Record, video_data_layout)

    def elaborate(self, platform):
        m = Module()

        pix = Signal(H_BITS)
        bar = Signal(3)

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.vtg.ready.eq(1)

                m.d.sync += [
                    pix.eq(0),
                    bar.eq(0),
                ]

                with m.If(self.vtg.valid & (self.vtg.bits.hcount == 0)
                          & (self.vtg.bits.vcount == 0)):
                    m.d.comb += self.vtg.ready.eq(0)
                    m.next = 'RUN'

            with m.State('RUN'):
                m.d.comb += [
                    self.source.bits.de.eq(self.vtg.bits.de),
                    self.source.bits.hsync.eq(self.vtg.bits.hsync),
                    self.source.bits.vsync.eq(self.vtg.bits.vsync),
                    self.source.valid.eq(self.vtg.valid),
                    self.vtg.ready.eq(self.source.ready),
                ]

                with m.If(self.source.fire & self.source.bits.de):
                    m.d.sync += pix.eq(pix + 1)
                    with m.If(pix == (self.vtg.bits.hres[3:] - 1)):
                        m.d.sync += [
                            pix.eq(0),
                            bar.eq(bar + 1),
                        ]
                with m.Else():
                    m.d.sync += [
                        pix.eq(0),
                        bar.eq(0),
                    ]

        color_bar = [
            [0xff, 0xff, 0xff],
            [0xff, 0xff, 0x00],
            [0x00, 0xff, 0xff],
            [0x00, 0xff, 0x00],
            [0xff, 0x00, 0xff],
            [0xff, 0x00, 0x00],
            [0x00, 0x00, 0xff],
            [0x00, 0x00, 0x00],
        ]

        with m.Switch(bar):
            for i, (r, g, b) in enumerate(color_bar):
                with m.Case(i):
                    m.d.comb += [
                        self.source.bits.r.eq(r),
                        self.source.bits.g.eq(g),
                        self.source.bits.b.eq(b),
                    ]

        return m


def import_bdf_font(filename):
    import csv
    font = None
    font_width = 0
    font_height = 0

    with open(filename) as f:
        reader = csv.reader(f, delimiter=" ")
        char = None
        bitmap_enable = False
        bitmap_index = 0
        for l in reader:
            if l[0] == "FONTBOUNDINGBOX":
                font_width = int(l[1], 0)
                font_height = int(l[2], 0)
                font = [0] * font_height * 256
            if l[0] == "ENCODING":
                char = int(l[1], 0)
            if l[0] == "ENDCHAR":
                bitmap_enable = False
            if bitmap_enable:
                if char < 256:
                    font[char * font_height + bitmap_index] = int(
                        "0x" + l[0], 0)
                bitmap_index += 1
            if l[0] == "BITMAP":
                bitmap_enable = True
                bitmap_index = 0

    return font, font_width, font_height


class VideoTerminal(Peripheral, Elaboratable):

    def __init__(self,
                 bdf_font,
                 *,
                 name=None,
                 hres=800,
                 vres=600,
                 with_attrib=True,
                 clock_domain="sync",
                 clock_freq=25e6):
        super().__init__(name=name)

        self.bdf_font = bdf_font
        self.hres = hres
        self.vres = vres
        self.with_attrib = with_attrib
        self.clock_domain = clock_domain
        self.clock_freq = clock_freq

        self.char_width = 16 if with_attrib else 8

        self.font, self.font_width, self.font_height = import_bdf_font(
            self.bdf_font)
        self.term_cols = 128
        self.term_lines = self.vres // self.font_height // 2
        self.term_depth = self.term_cols * self.term_lines

        self.vtg = Decoupled(Record, video_timing_layout)
        self.source = Decoupled(Record, video_data_layout)

        self._term_bus = self.window(addr_width=Shape.cast(
            range(self.term_depth)).width,
                                     data_width=self.char_width,
                                     granularity=8)

        self._bridge = self.bridge(data_width=self.char_width,
                                   granularity=8,
                                   alignment=2)
        self.bus = self._bridge.bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        font_mem = Memory(width=self.font_width,
                          depth=self.font_height * 256,
                          init=self.font)
        font_rport = m.submodules.font_rport = font_mem.read_port(
            transparent=False, domain=self.clock_domain)

        term_init = [ord(" ")] * self.term_depth
        term_mem = Memory(width=self.char_width,
                          depth=self.term_depth,
                          init=term_init)
        term_rport = m.submodules.term_rport = term_mem.read_port(
            transparent=False, domain=self.clock_domain)

        #
        # Terminal memory access
        #

        term_bus_rport = m.submodules.term_bus_rport = term_mem.read_port(
            transparent=False)
        term_bus_wport = m.submodules.term_bus_wport = term_mem.write_port(
            granularity=8)

        m.d.comb += [
            term_bus_rport.addr.eq(self._term_bus.adr),
            self._term_bus.dat_r.eq(term_bus_rport.data),
        ]

        m.d.comb += [
            term_bus_wport.addr.eq(self._term_bus.adr),
            term_bus_wport.data.eq(self._term_bus.dat_w),
        ]

        m.d.comb += [
            term_bus_wport.en[i].eq(self._term_bus.cyc & self._term_bus.stb
                                    & self._term_bus.we
                                    & self._term_bus.sel[i])
            for i in range(self._term_bus.data_width // 8)
        ]

        m.d.sync += [
            self._term_bus.ack.eq(self._term_bus.cyc & self._term_bus.stb
                                  & ~self._term_bus.ack)
        ]

        #
        # Terminal display
        #

        timing_bufs = [
            DomainRenamer(self.clock_domain)(SkidBuffer(Record,
                                                        video_timing_layout,
                                                        pipe=False))
            for _ in range(2)
        ]
        m.d.comb += self.vtg.connect(timing_bufs[0].enq)
        for i in range(len(timing_bufs) - 1):
            m.d.comb += timing_bufs[i].deq.connect(timing_bufs[i + 1].enq)
        m.submodules += timing_bufs

        m.d.comb += [
            self.source.valid.eq(timing_bufs[-1].deq.valid),
            self.source.bits.de.eq(timing_bufs[-1].deq.bits.de),
            self.source.bits.hsync.eq(timing_bufs[-1].deq.bits.hsync),
            self.source.bits.vsync.eq(timing_bufs[-1].deq.bits.vsync),
            timing_bufs[-1].deq.ready.eq(self.source.ready),
        ]

        x = self.vtg.bits.hcount[log2_int(self.font_width):]
        y = self.vtg.bits.vcount[log2_int(self.font_height):]

        term_rdata = Signal(self.char_width)
        m.d.comb += [
            term_rport.addr.eq(x + y * self.term_cols),
            term_rdata.eq(
                Mux((x >= 80) | (y >= self.term_lines), ord(" "),
                    term_rport.data)),
        ]

        if self.with_attrib:
            term_attrib = Signal(8)
            m.d.sync += term_attrib.eq(term_rdata[8:])

        m.d.comb += font_rport.addr.eq(
            term_rdata[:8] * self.font_height +
            timing_bufs[0].deq.bits.vcount[:log2_int(self.font_height)])
        font_bit = Signal()
        with m.Switch(
                timing_bufs[1].deq.bits.hcount[:log2_int(self.font_width)]):
            for i in range(self.font_width):
                with m.Case(i):
                    m.d.comb += font_bit.eq(font_rport.data[self.font_width -
                                                            1 - i])

        fg_color = Signal(32)
        bg_color = Signal(32)

        if self.with_attrib:
            blink_counter = Signal(range(int(self.clock_freq // 2)))
            blink = Signal()
            m.d.sync += blink_counter.eq(blink_counter + 1)
            with m.If(blink_counter == int(self.clock_freq // 2) - 1):
                m.d.sync += [
                    blink_counter.eq(0),
                    blink.eq(~blink),
                ]

            fg_attrib = term_attrib[:4]
            bg_attrib = term_attrib[4:7]

            palette = {
                0x0: 0x000000,
                0x1: 0x800000,
                0x2: 0x008000,
                0x3: 0x808000,
                0x4: 0x000080,
                0x5: 0x800080,
                0x6: 0x008080,
                0x7: 0xc0c0c0,
                0x8: 0x808080,
                0x9: 0xff0000,
                0xa: 0x00ff00,
                0xb: 0xffff00,
                0xc: 0x0000ff,
                0xd: 0xff00ff,
                0xe: 0x00ffff,
                0xf: 0xffffff,
            }

            with m.Switch(fg_attrib):
                for k, v in palette.items():
                    with m.Case(k):
                        m.d.comb += fg_color.eq(v)
            with m.If(~blink & term_attrib[7]):
                m.d.comb += fg_color.eq(bg_color)

            with m.Switch(Cat(bg_attrib, 0)):
                for k, v in palette.items():
                    with m.Case(k):
                        m.d.comb += bg_color.eq(v)
        else:
            m.d.comb += [
                fg_color.eq(0xffffff),
                bg_color.eq(0),
            ]

        m.d.comb += Cat(self.source.bits.r, self.source.bits.g,
                        self.source.bits.b).eq(
                            Mux(font_bit, fg_color, bg_color))

        return m


class VideoFrameBuffer(Elaboratable):

    def __init__(self,
                 bus,
                 *,
                 hres=800,
                 vres=600,
                 base=0x00000000,
                 fifo_depth=65536,
                 clock_domain="sync",
                 format="rgb888"):
        self.bus = bus
        self.hres = hres
        self.vres = vres
        self.base = base
        self.fifo_depth = fifo_depth
        self.clock_domain = clock_domain

        self.vtg = Decoupled(Record, video_timing_layout)
        self.source = Decoupled(Record, video_data_layout)
        self.underflow = Signal()

        self.depth = {"rgb888": 32, "rgb565": 16}[format]

    def elaborate(self, platform):
        m = Module()

        from .dma import WishboneDMAReader
        dma = m.submodules.dma = WishboneDMAReader(
            bus=self.bus,
            with_csr=True,
            fifo_depth=self.fifo_depth // (self.bus.data_width // 8),
            default_base=self.base,
            default_length=self.hres * self.vres * self.depth // 8,
            default_loop=1,
            default_enable=1)

        cdc = m.submodules.cdc = ClockDomainCrossing(
            Record, [("data", self.depth), ("last", 1)],
            to_domain=self.clock_domain)
        m.d.comb += dma.source.connect(cdc.sink)

        video_data = cdc.source

        m.d.comb += [
            self.vtg.ready.eq(1),
            self.source.bits.de.eq(self.vtg.bits.de),
            self.source.bits.hsync.eq(self.vtg.bits.hsync),
            self.source.bits.vsync.eq(self.vtg.bits.vsync),
        ]

        with m.If(self.vtg.valid & self.vtg.bits.de):
            m.d.comb += [
                self.source.valid.eq(video_data.valid),
                video_data.ready.eq(self.source.ready),
                self.vtg.ready.eq(self.source.fire),
            ]

        if self.depth == 32:
            m.d.comb += [
                self.source.bits.r.eq(video_data.bits.data[0:8]),
                self.source.bits.g.eq(video_data.bits.data[8:16]),
                self.source.bits.b.eq(video_data.bits.data[16:24]),
            ]
        elif self.depth == 16:
            m.d.comb += [
                self.source.bits.r.eq(
                    Cat(Const(0, 3), video_data.bits.data[11:16])),
                self.source.bits.g.eq(
                    Cat(Const(0, 2), video_data.bits.data[5:11])),
                self.source.bits.b.eq(
                    Cat(Const(0, 3), video_data.bits.data[0:5])),
            ]

        m.d.comb += self.underflow.eq(~self.source.valid)

        return m


class VideoHDMI10to1Serializer(Elaboratable):

    def __init__(self, clock_domain):
        self.clock_domain = clock_domain

        self.i = Signal(10)
        self.o = Signal()

    def elaborate(self, platform):
        m = Module()

        cdc = m.submodules.cdc = ClockDomainCrossing(
            Signal,
            10,
            from_domain=self.clock_domain,
            to_domain=self.clock_domain + "5x")
        m.d.comb += [
            cdc.sink.valid.eq(1),
            cdc.sink.bits.eq(self.i),
        ]

        gearbox = m.submodules.gearbox = DomainRenamer(self.clock_domain +
                                                       "5x")(Gearbox(
                                                           i_dw=10,
                                                           o_dw=2,
                                                           msb_first=False))
        m.d.comb += [
            cdc.source.connect(gearbox.sink),
            gearbox.source.ready.eq(1),
        ]

        oddr = m.submodules.oddr = DomainRenamer(self.clock_domain + "5x")(
            platform.get_ddr_output())
        m.d.comb += [
            oddr.i1.eq(gearbox.source.bits[0]),
            oddr.i2.eq(gearbox.source.bits[1]),
            self.o.eq(oddr.o),
        ]

        return m


class VideoS7HDMI10to1Serializer(Elaboratable):

    def __init__(self, clock_domain):
        self.clock_domain = clock_domain

        self.i = Signal(10)
        self.o = Signal()

    def elaborate(self, platform):
        m = Module()

        data_m = Signal(8)
        data_s = Signal(8)
        m.d.comb += data_m[0:8].eq(self.i[:8])
        m.d.comb += data_s[2:4].eq(self.i[8:])

        shift = Signal(2)
        for data, serdes in zip([data_m, data_s], ["master", "slave"]):
            param_dict = dict(
                p_DATA_WIDTH=10,
                p_TRISTATE_WIDTH=1,
                p_DATA_RATE_OQ="DDR",
                p_DATA_RATE_TQ="DDR",
                p_SERDES_MODE=serdes.upper(),
                i_OCE=1,
                i_TCE=0,
                i_RST=ResetSignal(self.clock_domain),
                i_CLK=ClockSignal(self.clock_domain + "5x"),
                i_CLKDIV=ClockSignal(self.clock_domain),
                **{f"i_D{n+1}": data[n]
                   for n in range(8)},
                i_SHIFTIN1=shift[0] if serdes == "master" else 0,
                i_SHIFTIN2=shift[1] if serdes == "master" else 0,
            )

            if serdes == "master":
                param_dict['o_OQ'] = self.o
            else:
                param_dict['o_SHIFTOUT1'] = shift[0]
                param_dict['o_SHIFTOUT2'] = shift[1]

            m.submodules += Instance(
                "OSERDESE2",
                **param_dict,
            )

        return m


class VideoUSHDMI10to1Serializer(Elaboratable):

    def __init__(self, clock_domain, sim_device):
        self.clock_domain = clock_domain
        self.sim_device = sim_device

        self.i = Signal(10)
        self.o = Signal()

    def elaborate(self, platform):
        m = Module()

        cdc = m.submodules.cdc = ClockDomainCrossing(
            Signal,
            10,
            from_domain=self.clock_domain,
            to_domain=self.clock_domain + "1_25x")
        m.d.comb += [
            cdc.sink.valid.eq(1),
            cdc.sink.bits.eq(self.i),
        ]

        gearbox = m.submodules.gearbox = DomainRenamer(self.clock_domain +
                                                       "1_25x")(Gearbox(
                                                           i_dw=10,
                                                           o_dw=8,
                                                           msb_first=False))
        m.d.comb += [
            cdc.source.connect(gearbox.sink),
            gearbox.source.ready.eq(1),
        ]

        m.submodules += Instance(
            "OSERDESE3",
            ###
            p_DATA_WIDTH=8,
            p_INIT=0,
            p_IS_CLKDIV_INVERTED=0,
            p_IS_CLK_INVERTED=0,
            p_IS_RST_INVERTED=0,
            p_SIM_DEVICE=self.sim_device,
            ###
            i_CLK=ClockSignal(self.clock_domain + "5x"),
            i_CLKDIV=ClockSignal(self.clock_domain + "1_25x"),
            i_D=gearbox.source.bits,
            i_RST=ResetSignal(self.clock_domain),
            i_T=0,
            ###
            o_OQ=self.o,
        )

        return m


class VideoHDMIPHY(Elaboratable):

    def __init__(self, clock_domain='sync'):
        self.clock_domain = clock_domain

        self.sink = Decoupled(Record, video_data_layout)

        self.tmds_clk_p = Signal()
        self.tmds_clk_n = Signal()
        self.tmds_data_p = Signal(3)
        self.tmds_data_n = Signal(3)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.sink.ready.eq(1)

        for pol in ["p", "n"]:
            oddr = DomainRenamer(self.clock_domain)(platform.get_ddr_output())
            m.d.comb += [
                oddr.i1.eq({
                    "p": 1,
                    "n": 0
                }[pol]),
                oddr.i2.eq({
                    "p": 0,
                    "n": 1
                }[pol]),
                getattr(self, f"tmds_clk_{pol}").eq(oddr.o),
            ]
            m.submodules += oddr

        for pol in ["p", "n"]:
            for color, channel in _dvi_c2d.items():
                enc = DomainRenamer(self.clock_domain)(TMDSEncoder())
                setattr(m.submodules, f'encoder_{color}_{pol}', enc)

                m.d.comb += [
                    enc.d.eq(getattr(self.sink.bits, color)),
                    enc.c.eq(
                        Cat(self.sink.bits.hsync, self.sink.bits.vsync
                            ) if channel == 0 else 0),
                    enc.de.eq(self.sink.bits.de),
                ]

                serializer = VideoHDMI10to1Serializer(self.clock_domain)
                setattr(m.submodules, f'serializer_{color}_{pol}', serializer)

                m.d.comb += [
                    serializer.i.eq(enc.out),
                    getattr(self,
                            f"tmds_data_{pol}")[channel].eq(serializer.o),
                ]

        return m


class VideoS7HDMIPHY(Elaboratable):

    def __init__(self, clock_domain='sync'):
        self.clock_domain = clock_domain

        self.sink = Decoupled(Record, video_data_layout)

        self.tmds_clk_p = Signal()
        self.tmds_clk_n = Signal()
        self.tmds_data_p = Signal(3)
        self.tmds_data_n = Signal(3)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.sink.ready.eq(1)

        clk_serializer = m.submodules.clk_serializer = VideoS7HDMI10to1Serializer(
            self.clock_domain)
        m.d.comb += clk_serializer.i.eq(0b0000011111)
        m.submodules += Instance("OBUFDS",
                                 i_I=clk_serializer.o,
                                 o_O=self.tmds_clk_p,
                                 o_OB=self.tmds_clk_n)

        for color, channel in _dvi_c2d.items():
            enc = DomainRenamer(self.clock_domain)(TMDSEncoder())
            setattr(m.submodules, f'encoder_{color}', enc)

            m.d.comb += [
                enc.d.eq(getattr(self.sink.bits, color)),
                enc.c.eq(
                    Cat(self.sink.bits.hsync, self.sink.bits.vsync
                        ) if channel == 0 else 0),
                enc.de.eq(self.sink.bits.de),
            ]

            serializer = VideoS7HDMI10to1Serializer(self.clock_domain)
            setattr(m.submodules, f'serializer_{color}', serializer)

            m.d.comb += serializer.i.eq(enc.out)
            m.submodules += Instance("OBUFDS",
                                     i_I=serializer.o,
                                     o_O=self.tmds_data_p[channel],
                                     o_OB=self.tmds_data_n[channel])

        return m


class VideoUSHDMIPHY(Elaboratable):

    def __init__(self, clock_domain='sync', sim_device='ULTRASCALE'):
        self.clock_domain = clock_domain
        self.sim_device = sim_device

        self.sink = Decoupled(Record, video_data_layout)

        self.tmds_clk_p = Signal()
        self.tmds_clk_n = Signal()
        self.tmds_data_p = Signal(3)
        self.tmds_data_n = Signal(3)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.sink.ready.eq(1)

        clk_serializer = m.submodules.clk_serializer = VideoUSHDMI10to1Serializer(
            self.clock_domain, sim_device=self.sim_device)
        m.d.comb += clk_serializer.i.eq(0b0000011111)
        m.submodules += Instance("OBUFDS",
                                 i_I=clk_serializer.o,
                                 o_O=self.tmds_clk_p,
                                 o_OB=self.tmds_clk_n)

        for color, channel in _dvi_c2d.items():
            enc = DomainRenamer(self.clock_domain)(TMDSEncoder())
            setattr(m.submodules, f'encoder_{color}', enc)

            m.d.comb += [
                enc.d.eq(getattr(self.sink.bits, color)),
                enc.c.eq(
                    Cat(self.sink.bits.hsync, self.sink.bits.vsync
                        ) if channel == 0 else 0),
                enc.de.eq(self.sink.bits.de),
            ]

            serializer = VideoUSHDMI10to1Serializer(self.clock_domain,
                                                    sim_device=self.sim_device)
            setattr(m.submodules, f'serializer_{color}', serializer)

            m.d.comb += serializer.i.eq(enc.out)
            m.submodules += Instance("OBUFDS",
                                     i_I=serializer.o,
                                     o_O=self.tmds_data_p[channel],
                                     o_OB=self.tmds_data_n[channel])

        return m
