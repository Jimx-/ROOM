from amaranth import *
from amaranth.lib.coding import PriorityEncoder

from room.consts import *
from room.fifo import SyncFIFO
from room.rvc import RVCDecoder
from room.id_stage import BranchSignals, BranchDecoder
from room.types import MicroOp


class FetchBundle:

    def __init__(self, fetch_width, name=None):
        self.fetch_width = fetch_width

        name = name is None and '' or f'{name}_'

        self.pc = Signal(32, name=f'{name}pc')
        self.next_pc = Signal(32, name=f'{name}next_pc')
        self.insts = [
            Signal(32, name=f'{name}inst{i}') for i in range(fetch_width)
        ]
        self.exp_insts = [
            Signal(32, name=f'{name}exp_inst{i}') for i in range(fetch_width)
        ]
        self.mask = Signal(fetch_width)

        self.cfi_valid = Signal(name=f'{name}cfi_valid')
        self.cfi_idx = Signal(range(fetch_width), name=f'{name}cfi_idx')
        self.cfi_type = Signal(CFIType, name=f'{name}cfi_type')

    def eq(self, rhs):
        ret = [
            self.pc.eq(rhs.pc),
            self.next_pc.eq(rhs.next_pc),
            self.mask.eq(rhs.mask),
            self.cfi_valid.eq(rhs.cfi_valid),
            self.cfi_idx.eq(rhs.cfi_idx),
            self.cfi_type.eq(rhs.cfi_type),
        ]
        for inst, rinst in zip(self.insts, rhs.insts):
            ret.append(inst.eq(rinst))
        for inst, rinst in zip(self.exp_insts, rhs.exp_insts):
            ret.append(inst.eq(rinst))
        return ret


class FetchBuffer(Elaboratable):

    def __init__(self, params):
        self.params = params
        self.fetch_width = params['fetch_width']
        self.core_width = params['core_width']
        self.depth = params['fetch_buffer_size']

        self.w_data = FetchBundle(self.fetch_width)
        self.r_data = [
            MicroOp(params, name=f'fb_uop{i}') for i in range(self.core_width)
        ]
        self.w_rdy = Signal()
        self.r_rdy = Signal()
        self.w_en = Signal()
        self.r_en = Signal()
        self.flush = Signal()

    def elaborate(self, platform):
        m = Module()

        num_entries = self.depth
        num_rows = self.depth // self.core_width

        mem = Array(
            MicroOp(self.params, name=f'mem{i}') for i in range(self.depth))

        head = Signal(num_rows, reset=1)
        tail = Signal(num_entries, reset=1)

        maybe_full = Signal()
        do_enq = Signal()
        do_deq = Signal()

        in_uops = [
            MicroOp(self.params, name=f'in_uop{i}')
            for i in range(self.fetch_width)
        ]
        in_mask = Signal(self.fetch_width)

        for w in range(self.fetch_width):
            m.d.comb += [
                in_mask[w].eq(self.w_data.mask[w] & self.w_en),
                in_uops[w].inst.eq(self.w_data.exp_insts[w]),
                in_uops[w].is_rvc.eq(in_uops[w].inst[0:2] != 3),
            ]

        wr_slots = [
            Signal(num_entries, name=f'wr_slot{i}')
            for i in range(self.fetch_width)
        ]
        next_slot = tail
        for wr_slot, mask in zip(wr_slots, in_mask):
            m.d.comb += wr_slot.eq(next_slot)
            next_slot = Mux(
                mask,
                Cat(next_slot[num_entries - 1], next_slot[:num_entries - 1]),
                next_slot)

        for w in range(self.fetch_width):
            for s in range(num_entries):
                with m.If(do_enq & in_mask[w] & wr_slots[w][s]):
                    m.d.sync += mem[s].eq(in_uops[w])

        wr_mask = Const(0, num_entries)
        for w in range(1, self.fetch_width):
            wr_mask |= Cat(tail[num_entries - w:num_entries],
                           tail[:num_entries - w])

        might_hit_head = 0
        at_head = 0
        for r in range(num_rows):
            might_hit_head |= head[r] & wr_mask[r * self.core_width]
            at_head |= head[r] & tail[r * self.core_width]

        m.d.comb += [
            do_enq.eq(~(at_head & maybe_full | might_hit_head)),
            self.w_rdy.eq(do_enq),
        ]

        tail_collisions = Array(
            Signal(self.core_width, name=f'tc{i}') for i in range(num_rows))
        for i in range(num_entries):
            m.d.comb += tail_collisions[i // self.core_width][
                i % self.core_width].eq(head[i // self.core_width]
                                        & (~maybe_full
                                           | (i % self.core_width != 0))
                                        & tail[i])
        slot_hit_tail = Signal(self.core_width)
        for i in range(num_rows):
            with m.If(head[i]):
                m.d.comb += slot_hit_tail.eq(tail_collisions[i])
        will_hit_tail = slot_hit_tail != 0

        m.d.comb += do_deq.eq(self.r_en & ~will_hit_tail)

        hit_mask = slot_hit_tail
        for i in range(self.core_width):
            hit_mask |= (hit_mask << 1)

        deq_valids = ~hit_mask[0:self.core_width]

        for i in range(num_rows):
            with m.If(head[i]):
                for w in range(self.core_width):
                    m.d.comb += [
                        self.r_data[w].eq(mem[i * self.core_width + w]),
                        self.r_data[w].valid.eq(deq_valids[w]),
                    ]
        m.d.comb += self.r_rdy.eq(deq_valids != 0)

        with m.If(do_enq):
            m.d.sync += tail.eq(next_slot)
            with m.If(in_mask != 0):
                m.d.sync += maybe_full.eq(1)

        with m.If(do_deq):
            m.d.sync += [
                head.eq(Cat(head[num_rows - 1], head[:num_rows - 1])),
                maybe_full.eq(0),
            ]

        with m.If(self.flush):
            m.d.sync += [
                head.eq(1),
                tail.eq(1),
                maybe_full.eq(0),
            ]

        return m


class IFStage(Elaboratable):

    def __init__(self, ibus, params):
        self.ibus = ibus
        self.params = params

        self.fetch_width = params['fetch_width']
        self.core_width = params['core_width']
        self.fetch_buffer_size = params['fetch_buffer_size']

        self.fetch_packet = [
            MicroOp(params, name=f'fetch_uop{i}')
            for i in range(self.core_width)
        ]
        self.fetch_packet_valid = Signal()
        self.fetch_packet_ready = Signal()

    def elaborate(self, platform):
        m = Module()

        ibus = self.ibus

        def fetch_mask(addr):
            off = (addr >> 1)[0:Shape.cast(range(self.fetch_width)).width]
            return (((1 << self.fetch_width) - 1) << off)[0:self.fetch_width]

        def fetch_align(addr):
            lsb_width = Shape.cast(range(ibus.dat_r.width >> 3)).width
            return Cat(Const(0, lsb_width), addr[lsb_width:])

        def next_fetch(addr):
            return fetch_align(addr) + self.fetch_width * 2

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

        #
        # ICache Access
        #

        s1_vpc = Signal(32)
        s1_valid = Signal()
        f1_clear = Signal()
        f2_ready = Signal()

        m.d.comb += [
            ibus.adr.eq(Mux(s0_valid, s0_vpc, s1_vpc)),
            ibus.cyc.eq((s0_valid | s1_valid) & ~f1_clear),
            ibus.stb.eq(ibus.cyc),
        ]

        m.d.sync += [
            s1_vpc.eq(Mux(s0_valid, s0_vpc, s1_vpc)),
            s1_valid.eq(s0_valid),
        ]

        with m.If(s0_valid):
            m.d.sync += s1_valid.eq(1)
        with m.Elif((ibus.ack & f2_ready) | f1_clear):
            m.d.sync += s1_valid.eq(0)

        #
        # ICache Response
        #

        f2_clear = Signal()
        f2_fifo = m.submodules.f2_fifo = SyncFIFO(width=ibus.dat_r.width + 32,
                                                  depth=1)

        f3_ready = Signal()

        m.d.comb += [
            f2_fifo.flush.eq(f2_clear),
            f2_fifo.w_en.eq(~f1_clear & ibus.ack),
            f2_fifo.w_data.eq(Cat(s1_vpc, ibus.dat_r)),
            f2_fifo.r_en.eq(f3_ready),
            f2_ready.eq(f2_fifo.w_rdy),
        ]

        s2_valid = f2_fifo.r_rdy
        s2_pc = Signal(32)
        s2_data = Signal(ibus.dat_r.width)
        m.d.comb += [
            s2_pc.eq(f2_fifo.r_data[:32]),
            s2_data.eq(f2_fifo.r_data[32:]),
        ]

        f2_prev_half = Signal(16)
        f2_prev_half_valid = Signal()

        f2_imemresp_mask = Signal(self.fetch_width)
        f2_aligned_pc = fetch_align(s2_pc)
        f2_insts = [Signal(32) for _ in range(self.fetch_width)]
        f2_exp_insts = [Signal(32) for _ in range(self.fetch_width)]
        f2_mask = Signal(self.fetch_width)
        f2_is_rvc = Signal(self.fetch_width)
        f2_redirects = Signal(self.fetch_width)
        f2_targets = Array(
            Signal(32, name=f'f2_target{i}') for i in range(self.fetch_width))
        f2_plus4_mask = Signal(self.fetch_width)
        f2_fetch_bundle = FetchBundle(self.fetch_width, name='f2_fetch_bundle')

        m.d.comb += f2_imemresp_mask.eq(fetch_mask(s2_pc))

        for binst, inst in zip(f2_fetch_bundle.insts, f2_insts):
            m.d.comb += binst.eq(inst)
        for binst, inst in zip(f2_fetch_bundle.exp_insts, f2_exp_insts):
            m.d.comb += binst.eq(inst)
        m.d.comb += [
            f2_fetch_bundle.mask.eq(f2_mask),
            f2_fetch_bundle.pc.eq(s2_pc),
        ]

        redirects_found = 0
        for w in range(self.fetch_width):
            valid = Signal()

            if w == 0:
                inst0 = Cat(f2_prev_half, s2_data[0:16])
                inst1 = s2_data[0:32]
                dec0 = RVCDecoder()
                dec1 = RVCDecoder()
                br_dec0 = BranchDecoder()
                br_dec1 = BranchDecoder()
                m.submodules += [dec0, dec1, br_dec0, br_dec1]

                pc0 = f2_aligned_pc - 2
                pc1 = f2_aligned_pc

                m.d.comb += [
                    dec0.instr_i.eq(inst0),
                    dec1.instr_i.eq(inst1),
                    br_dec0.inst.eq(dec0.instr_o),
                    br_dec0.pc.eq(pc0),
                    br_dec1.inst.eq(dec1.instr_o),
                    br_dec1.pc.eq(pc1),
                ]

                with m.If(f2_prev_half_valid):
                    pass
                with m.Else():
                    m.d.comb += [
                        f2_insts[w].eq(inst1),
                        f2_exp_insts[w].eq(dec1.instr_o),
                    ]
                    br_sigs = br_dec1.out

                m.d.comb += [valid.eq(1)]

            else:
                inst = Signal(32)
                pc = f2_aligned_pc + (w * 2)
                dec = RVCDecoder()
                br_dec = BranchDecoder()
                m.submodules += [dec, br_dec]

                m.d.comb += [
                    f2_insts[w].eq(inst),
                    dec.instr_i.eq(inst),
                    br_dec.inst.eq(dec.instr_o),
                    br_dec.pc.eq(pc),
                    f2_exp_insts[w].eq(dec.instr_o),
                ]

                br_sigs = br_dec.out

                if w == 1:
                    m.d.comb += [
                        inst.eq(s2_data[16:48]),
                        # insts[0] is not a 32-bit instruction
                        valid.eq(f2_prev_half_valid
                                 | ~(f2_mask[w - 1] &
                                     (f2_insts[w - 1][0:2] == 3))),
                    ]
                elif w == self.fetch_width - 1:
                    m.d.comb += [
                        inst.eq(Cat(s2_data[-16:], Const(0, 16))),
                        # Last instruction is not a 32-bit instruction and this is a RVC instruction
                        valid.eq(~((f2_mask[w - 1]
                                    & (f2_insts[w - 1][0:2] == 3))
                                   | (f2_insts[w][0:2] == 3))),
                    ]
                else:
                    m.d.comb += [
                        inst.eq(s2_data[w * 16:w * 16 + 32]),
                        # Last instruction is not a 32-bit instruction
                        valid.eq(~(f2_mask[w - 1]
                                   & (f2_insts[w - 1][0:2] == 3)))
                    ]

            m.d.comb += [
                f2_is_rvc[w].eq(f2_insts[w][0:2] != 3),
                f2_mask[w].eq(s2_valid & valid & f2_imemresp_mask[w]
                              & ~redirects_found),
                f2_targets[w].eq(br_sigs.target),
            ]

            if w == 0:
                f2_plus4_mask[w].eq(
                    ((f2_insts[w][0:2] == 3) & ~f2_prev_half_valid))
            else:
                f2_plus4_mask[w].eq((f2_insts[w][0:2] == 3))

            m.d.comb += f2_redirects[w].eq(br_sigs.cfi_type != CFIType.X)

            redirects_found |= f2_redirects[w]

        last_inst = f2_insts[self.fetch_width - 1][0:16]
        with m.If(f2_fifo.r_en & f2_fifo.r_rdy):
            m.d.sync += [
                f2_prev_half.eq(last_inst),
                f2_prev_half_valid.eq(~(f2_mask[self.fetch_width - 2]
                                        & ~f2_is_rvc[self.fetch_width - 2])
                                      & (last_inst[0:2] != 3)),
            ]
        with m.Elif(f2_clear):
            m.d.sync += f2_prev_half_valid.eq(0)

        f2_prio_enc = m.submodules.f2_prio_enc = PriorityEncoder(
            self.fetch_width)
        m.d.comb += [
            f2_prio_enc.i.eq(f2_redirects),
            f2_fetch_bundle.cfi_valid.eq(~f2_prio_enc.n),
            f2_fetch_bundle.cfi_idx.eq(f2_prio_enc.o),
        ]

        f2_predicted_target = Signal(32)
        m.d.comb += [
            f2_predicted_target.eq(
                Mux(f2_fetch_bundle.cfi_valid,
                    f2_targets[f2_fetch_bundle.cfi_idx],
                    next_fetch(f2_fetch_bundle.pc))),
            f2_fetch_bundle.next_pc.eq(f2_predicted_target),
        ]

        with m.If(s2_valid & f3_ready):
            with m.If(f2_fetch_bundle.cfi_valid):
                m.d.sync += f2_prev_half_valid.eq(0)

            with m.If((s1_valid & (s1_vpc != f2_predicted_target))
                      | ~s1_valid):
                m.d.comb += [
                    f1_clear.eq(s1_valid),
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

        for fp, rd in zip(self.fetch_packet, fb.r_data):
            m.d.comb += fp.eq(rd)

        return m
