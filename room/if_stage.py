from amaranth import *
from amaranth.lib.coding import PriorityEncoder

from room.consts import *
from room.fifo import SyncFIFO
from room.rvc import RVCDecoder
from room.id_stage import BranchDecoder
from room.types import MicroOp


class GetPCResp(Record):

    def __init__(self, name=None):
        super().__init__([
            ('pc', 32),
            ('commit_pc', 32),
            ('next_pc', 32),
            ('next_valid', 1),
        ],
                         name=name)


class FetchBundle:

    def __init__(self, params, name=None):
        fetch_width = params['fetch_width']
        ftq_size = params['fetch_buffer_size']
        name = (name is not None) and f'{name}_' or ''

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

        self.ftq_idx = Signal(range(ftq_size), name=f'{name}ftq_size')

    def eq(self, rhs):
        ret = [
            self.pc.eq(rhs.pc),
            self.next_pc.eq(rhs.next_pc),
            self.mask.eq(rhs.mask),
            self.cfi_valid.eq(rhs.cfi_valid),
            self.cfi_idx.eq(rhs.cfi_idx),
            self.cfi_type.eq(rhs.cfi_type),
            self.ftq_idx.eq(rhs.ftq_idx),
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

        self.w_data = FetchBundle(params)
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
            pc = self.w_data.pc + (w << 1)
            m.d.comb += [
                in_mask[w].eq(self.w_data.mask[w] & self.w_en),
                in_uops[w].ftq_idx.eq(self.w_data.ftq_idx),
                in_uops[w].pc_lsb.eq(pc),
                in_uops[w].inst.eq(self.w_data.exp_insts[w]),
                in_uops[w].is_rvc.eq(in_uops[w].inst[0:2] != 3),
                in_uops[w].taken.eq((self.w_data.cfi_idx == w)
                                    & self.w_data.cfi_valid),
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


def _incr(signal, modulo):
    if modulo == 2**len(signal):
        return signal + 1
    else:
        return Mux(signal == modulo - 1, 0, signal + 1)


class FetchTargetQueue(Elaboratable):

    def __init__(self, params):
        self.ftq_size = params['fetch_buffer_size']
        self.fetch_bytes = params['fetch_bytes']

        self.w_data = FetchBundle(params)
        self.w_idx = Signal(range(self.ftq_size))
        self.w_en = Signal()
        self.w_rdy = Signal()

        self.deq_idx = Signal(range(self.ftq_size))
        self.deq_valid = Signal()

        self.redirect_idx = Signal(range(self.ftq_size))
        self.redirect_valid = Signal()

        self.get_pc_idx = [
            Signal(range(self.ftq_size), name=f'get_pc_idx{i}')
            for i in range(2)
        ]
        self.get_pc = [GetPCResp(name=f'get_pc{i}') for i in range(2)]

    def elaborate(self, platform):
        m = Module()

        pc_lsb_w = Shape.cast(range(self.fetch_bytes)).width

        w_ptr = Signal(range(self.ftq_size), reset=1)
        deq_ptr = Signal(range(self.ftq_size))

        full = (_incr(_incr(w_ptr, self.ftq_size), self.ftq_size)
                == deq_ptr) | (_incr(w_ptr, self.ftq_size) == deq_ptr)

        pcs = Array(
            Signal(32 - pc_lsb_w, name=f'pc{i}') for i in range(self.ftq_size))

        with m.If(self.w_en & self.w_rdy):
            m.d.sync += [
                pcs[w_ptr].eq(self.w_data.pc >> pc_lsb_w),
                w_ptr.eq(_incr(w_ptr, self.ftq_size)),
            ]

        m.d.comb += self.w_idx.eq(w_ptr)

        with m.If(self.deq_valid):
            m.d.sync += deq_ptr.eq(self.deq_idx)

        m.d.comb += self.w_rdy.eq(~full)

        with m.If(self.redirect_valid):
            m.d.sync += w_ptr.eq(_incr(self.redirect_idx, self.ftq_size))

        for get_pc_idx, get_pc in zip(self.get_pc_idx, self.get_pc):
            m.d.sync += [
                get_pc.pc.eq(pcs[get_pc_idx] << pc_lsb_w),
                get_pc.commit_pc.eq(pcs[Mux(self.deq_valid, self.deq_idx,
                                            deq_ptr)] << pc_lsb_w),
            ]

        return m


class IFStage(Elaboratable):

    def __init__(self, ibus, params):
        self.ibus = ibus
        self.params = params

        self.fetch_width = params['fetch_width']
        self.core_width = params['core_width']
        self.fetch_buffer_size = params['fetch_buffer_size']
        self.fetch_addr_shift = Shape.cast(range(params['fetch_bytes'])).width
        ftq_size = self.fetch_buffer_size

        self.fetch_packet = [
            MicroOp(params, name=f'fetch_uop{i}')
            for i in range(self.core_width)
        ]
        self.fetch_packet_valid = Signal()
        self.fetch_packet_ready = Signal()

        self.commit = Signal(range(ftq_size))
        self.commit_valid = Signal()

        self.get_pc_idx = [
            Signal(ftq_size, name=f'get_pc_idx{i}') for i in range(2)
        ]
        self.get_pc = [GetPCResp(name=f'get_pc{i}') for i in range(2)]

        self.redirect_valid = Signal()
        self.redirect_pc = Signal(32)
        self.redirect_flush = Signal()
        self.redirect_ftq_idx = Signal(range(ftq_size))

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

        ibus_addr = Mux(s0_valid, s0_vpc, s1_vpc)
        m.d.comb += [
            ibus.adr.eq(ibus_addr[self.fetch_addr_shift:]),
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
        f2_fetch_bundle = FetchBundle(self.params, name='f2_fetch_bundle')

        m.d.comb += f2_imemresp_mask.eq(fetch_mask(s2_pc))

        for binst, inst in zip(f2_fetch_bundle.insts, f2_insts):
            m.d.comb += binst.eq(inst)
        for binst, inst in zip(f2_fetch_bundle.exp_insts, f2_exp_insts):
            m.d.comb += binst.eq(inst)
        m.d.comb += [
            f2_fetch_bundle.mask.eq(f2_mask),
            f2_fetch_bundle.pc.eq(f2_aligned_pc),
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

            m.d.comb += f2_redirects[w].eq(f2_mask[w] & (
                (br_sigs.cfi_type == CFIType.JAL)
                | (br_sigs.cfi_type == CFIType.JALR)))

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
        ftq = m.submodules.ftq = FetchTargetQueue(self.params)

        m.d.comb += [
            f3_ready.eq(fb.w_rdy & ftq.w_rdy),
            fb.w_data.eq(f2_fetch_bundle),
            fb.w_data.ftq_idx.eq(ftq.w_idx),
            fb.w_en.eq(f2_fifo.r_rdy & ftq.w_rdy & ~f2_clear),
            fb.r_en.eq(self.fetch_packet_ready),
            self.fetch_packet_valid.eq(fb.r_rdy),
            ftq.w_data.eq(f2_fetch_bundle),
            ftq.w_en.eq(f2_fifo.r_rdy & fb.w_rdy & ~f2_clear),
        ]

        for fp, rd in zip(self.fetch_packet, fb.r_data):
            m.d.comb += fp.eq(rd)

        m.d.comb += [
            ftq.deq_idx.eq(self.commit),
            ftq.deq_valid.eq(self.commit_valid),
        ]

        for a, b in zip(ftq.get_pc_idx, self.get_pc_idx):
            m.d.comb += a.eq(b)
        for a, b in zip(self.get_pc, ftq.get_pc):
            m.d.comb += a.eq(b)

        #
        # Redirect
        #

        with m.If(self.redirect_flush):
            m.d.comb += [
                f1_clear.eq(1),
                f2_clear.eq(1),
                f3_clear.eq(1),
                fb.flush.eq(1),
                s0_valid.eq(self.redirect_valid),
                s0_vpc.eq(self.redirect_pc),
                ftq.redirect_valid.eq(self.redirect_valid),
                ftq.redirect_idx.eq(self.redirect_ftq_idx),
            ]

        return m
