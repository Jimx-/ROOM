from amaranth import *

from room.interface import OBI
from room.fifo import SyncFIFO
from room.rvc import RVCDecoder
from room.types import MicroOp


def make_fetch_bundle_layout(fetch_width):
    return [('pc', 32), ('next_pc', 32), ('insts', 32 * fetch_width),
            ('exp_insts', 32 * fetch_width), ('mask', fetch_width)]


class FetchBundle(Record):

    def __init__(self, fetch_width, name=None):
        self.fetch_width = fetch_width
        super().__init__(make_fetch_bundle_layout(fetch_width=fetch_width),
                         name=name)

    def inst(self, i):
        return self.insts[i * 32:(i + 1) * 32]

    def exp_inst(self, i):
        return self.exp_insts[i * 32:(i + 1) * 32]


class FetchBuffer(Elaboratable):

    def __init__(self, params):
        self.fetch_width = params['fetch_width']
        self.depth = params['fetch_buffer_size']
        self.layout = make_fetch_bundle_layout(self.fetch_width)

        self.w_data = FetchBundle(self.fetch_width)
        self.r_data = [
            MicroOp(params, name=f'fb_uop{i}') for i in range(self.fetch_width)
        ]
        self.w_rdy = Signal()
        self.r_rdy = Signal()
        self.w_en = Signal()
        self.r_en = Signal()
        self.flush = Signal()

    def elaborate(self, platform):
        m = Module()

        fifo = m.submodules.fifo = SyncFIFO(width=self.w_data.shape().width,
                                            depth=self.depth)

        flatten_wdata = Cat(
            *[getattr(self.w_data, name) for name, _ in self.layout])

        m.d.comb += [
            fifo.w_data.eq(flatten_wdata),
            self.w_rdy.eq(fifo.w_rdy),
            self.r_rdy.eq(fifo.r_rdy),
            fifo.w_en.eq(self.w_en),
            fifo.r_en.eq(self.r_en),
            fifo.flush.eq(self.flush),
        ]

        r_data = FetchBundle(self.fetch_width)

        acc = 0
        for name, width in self.layout:
            m.d.comb += getattr(r_data, name).eq(fifo.r_data[acc:acc + width])
            acc += width

        for i in range(self.fetch_width):
            uop = self.r_data[i]
            m.d.comb += [
                uop.valid.eq(r_data.mask[i]),
                uop.inst.eq(r_data.exp_inst(i)),
                uop.is_rvc.eq(r_data.inst(i)[0:2] != 3)
            ]

        return m


class IFStage(Elaboratable):

    def __init__(self, ibus, params):
        self.ibus = ibus
        self.params = params

        self.fetch_width = params['fetch_width']
        self.fetch_buffer_size = params['fetch_buffer_size']

        self.fetch_packet = [
            MicroOp(params, name=f'fetch_uop{i}')
            for i in range(self.fetch_width)
        ]
        self.fetch_packet_valid = Signal()
        self.fetch_packet_ready = Signal()

    def elaborate(self, platform):
        m = Module()

        ibus = self.ibus

        # Record number of inflight icache reads
        count_up = Signal()
        count_down = Signal()
        count = Signal(range(2))
        flush_count = Signal.like(count)

        m.d.comb += [
            count_up.eq(self.ibus.req & self.ibus.gnt),
            count_down.eq(self.ibus.rvalid),
        ]

        with m.If((~count_up) & count_down):
            m.d.sync += count.eq(count - 1)
        with m.Elif(count_up & (~count_down)):
            m.d.sync += count.eq(count + 1)

        #
        # Next PC select
        #

        s0_vpc = Signal(32)
        s0_valid = Signal()

        reset_d1 = Signal(reset_less=True)
        m.d.sync += reset_d1.eq(ResetSignal())
        with m.If(reset_d1 & ~ResetSignal()):
            m.d.comb += [
                s0_vpc.eq(0),
                s0_valid.eq(1),
            ]

        m.d.comb += [
            ibus.addr.eq(s0_vpc),
            ibus.req.eq(s0_valid),
        ]

        #
        # ICache Access
        #

        s1_vpc = Signal(32)
        s1_valid = Signal()
        s1_trans_ready = Signal()
        f1_clear = Signal()
        f2_ready = Signal()

        m.d.comb += f1_clear.eq(ResetSignal())

        m.d.sync += [
            s1_vpc.eq(Mux(s0_valid, s0_vpc, s1_vpc)),
            s1_valid.eq(s0_valid),
            s1_trans_ready.eq(ibus.gnt),
        ]

        with m.If((s1_valid & ~s1_trans_ready) | (ibus.rvalid & ~f2_ready)):
            m.d.comb += [s0_valid.eq(1), s0_vpc.eq(s1_vpc)]

        #
        # ICache Response
        #

        f2_clear = Signal()
        f2_fifo = m.submodules.f2_fifo = SyncFIFO(width=ibus.rdata.width + 32,
                                                  depth=1)

        f3_ready = Signal()

        m.d.comb += [
            f2_fifo.flush.eq(f2_clear),
            f2_fifo.w_en.eq(~f1_clear & ibus.rvalid),
            f2_fifo.w_data.eq(Cat(s1_vpc, ibus.rdata)),
            f2_fifo.r_en.eq(f3_ready),
            f2_ready.eq(f2_fifo.w_rdy),
        ]

        f2_pc = Signal(32)
        f2_data = Signal(ibus.rdata.width)
        m.d.comb += [
            f2_pc.eq(f2_fifo.r_data[:32]),
            f2_data.eq(f2_fifo.r_data[32:]),
        ]

        f2_prev_half = Signal(16)
        f2_prev_half_valid = Signal()

        f2_insts = [Signal(32) for _ in range(self.fetch_width)]
        f2_exp_insts = [Signal(32) for _ in range(self.fetch_width)]
        f2_mask = [Signal() for _ in range(self.fetch_width)]
        f2_is_rvc = [Signal() for _ in range(self.fetch_width)]
        f2_plus4_mask = [Signal() for _ in range(self.fetch_width)]
        f2_fetch_bundle = FetchBundle(self.fetch_width)

        for i in range(self.fetch_width):
            m.d.comb += [
                f2_fetch_bundle.mask[i].eq(f2_mask[i]),
                f2_fetch_bundle.inst(i).eq(f2_insts[i]),
                f2_fetch_bundle.exp_inst(i).eq(f2_exp_insts[i]),
                f2_fetch_bundle.pc.eq(f2_pc),
            ]

        for w in range(self.fetch_width):
            valid = Signal()

            if w == 0:
                inst0 = Cat(f2_prev_half, f2_data[0:16])
                inst1 = f2_data[0:32]
                dec0 = RVCDecoder()
                dec1 = RVCDecoder()
                m.submodules += [dec0, dec1]

                m.d.comb += [dec0.instr_i.eq(inst0), dec1.instr_i.eq(inst1)]

                with m.If(f2_prev_half_valid):
                    pass
                with m.Else():
                    m.d.comb += [
                        f2_insts[w].eq(inst1),
                        f2_exp_insts[w].eq(dec1.instr_o),
                    ]

                m.d.comb += [valid.eq(1)]

            else:
                inst = Signal(32)
                dec = RVCDecoder()
                m.submodules += dec

                m.d.comb += [
                    f2_insts[w].eq(inst),
                    dec.instr_i.eq(inst),
                    f2_exp_insts[w].eq(dec.instr_o),
                ]

                if w == 1:
                    m.d.comb += [
                        inst.eq(f2_data[16:48]),
                        # insts[0] is not a 32-bit instruction
                        valid.eq(f2_prev_half_valid
                                 | ~(f2_mask[w - 1] &
                                     (f2_insts[w - 1][0:2] == 3))),
                    ]
                elif w == self.fetch_width - 1:
                    m.d.comb += [
                        inst.eq(Cat(f2_data[-16:], Const(0, 16))),
                        # Last instruction is not a 32-bit instruction and this is a RVC instruction
                        valid.eq(~((f2_mask[w - 1]
                                    & (f2_insts[w - 1][0:2] == 3))
                                   | (f2_insts[w][0:2] == 3))),
                    ]
                else:
                    m.d.comb += [
                        inst.eq(f2_data[w * 16:w * 16 + 32]),
                        # Last instruction is not a 32-bit instruction
                        valid.eq(~(f2_mask[w - 1]
                                   & (f2_insts[w - 1][0:2] == 3)))
                    ]

            m.d.comb += [
                f2_is_rvc[w].eq(f2_insts[w][0:2] != 3),
                f2_mask[w].eq(f2_fifo.r_rdy & valid),
            ]

            if w == 0:
                f2_plus4_mask[w].eq(
                    ((f2_insts[w][0:2] == 3) & ~f2_prev_half_valid))
            else:
                f2_plus4_mask[w].eq((f2_insts[w][0:2] == 3))

        last_inst = f2_insts[self.fetch_width - 1][0:16]
        with m.If(f2_fifo.r_en & f2_fifo.r_rdy):
            m.d.sync += [
                f2_prev_half.eq(last_inst),
                f2_prev_half_valid.eq(~(f2_mask[self.fetch_width - 2]
                                        & ~f2_is_rvc[self.fetch_width - 2])
                                      & (last_inst[0:2] != 3)),
            ]
        with m.Else():
            m.d.sync += f2_prev_half_valid.eq(0)

        f2_predicted_target = Signal(32)
        m.d.comb += [
            f2_predicted_target.eq(f2_fetch_bundle.pc + self.fetch_width * 2),
            f2_fetch_bundle.next_pc.eq(f2_predicted_target),
        ]

        with m.If(f2_fifo.r_rdy & f3_ready):
            with m.If((s1_valid & (s1_vpc != f2_predicted_target))
                      | ~s1_valid):
                m.d.comb += [
                    f1_clear.eq(1),
                    s0_valid.eq(1),
                    s0_vpc.eq(f2_predicted_target),
                ]

        #
        # Fetch Buffer
        #

        f3_clear = Signal()
        fb = m.submodules.fb = FetchBuffer(self.params)

        m.d.comb += [
            f3_ready.eq(fb.w_rdy),
            fb.w_data.eq(f2_fetch_bundle),
            fb.w_en.eq(f2_fifo.r_rdy & ~f2_clear),
            fb.r_en.eq(self.fetch_packet_ready),
            self.fetch_packet_valid.eq(fb.r_rdy),
        ]

        for i in range(self.fetch_width):
            m.d.comb += self.fetch_packet[i].eq(fb.r_data[i])

        return m
