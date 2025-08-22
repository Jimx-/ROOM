from amaranth import *
from amaranth.utils import log2_int

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp
from vroom.fu import IterativeFunctionalUnit

from room.regfile import RFReadPort

from roomsoc.interconnect.stream import Queue, Valid


class VSlideUnit(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.opcode = Signal(VOpCode)
        self.vm = Signal()
        self.vma = Signal()
        self.vsew = Signal(3)
        self.vlmul_sign = Signal()
        self.vlmul_mag = Signal()
        self.vl = Signal(self.vl_bits)
        self.uop_idx = Signal(range(8))
        self.rs1_data = Signal(self.xlen)
        self.vs2_lo = Valid(Signal, self.vlen)
        self.vs2_hi = Valid(Signal, self.vlen)
        self.old_vd = Signal(self.vlen)
        self.vmask = Signal(self.vlen)

        self.resp_data = Signal(self.vlen)

    def elaborate(self, platform):
        m = Module()

        vslideup_data = Signal(self.vlen)
        vslidedown_data = Signal(self.vlen)
        vslide1up_data = Signal(self.vlen)
        vslide1down_data = Signal(self.vlen)

        vsew_bytes = 1 << self.vsew[:2]
        vl_bytes = self.vl << self.vsew
        vl_idx = Signal(range(8))
        m.d.comb += vl_idx.eq(vl_bytes[log2_int(self.vlen_bytes):] +
                              vl_bytes[:log2_int(self.vlen_bytes)].any() - 1)

        vl_remain_bytes = Mux(
            vl_bytes > (self.uop_idx << log2_int(self.vlen_bytes)),
            vl_bytes - (self.uop_idx << log2_int(self.vlen_bytes)), 0)

        vmask_uop = Signal(self.vlen_bytes)
        with m.Switch(self.vsew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    stride = self.vlen // n

                    with m.Switch(self.uop_idx):
                        for i in range(8):
                            with m.Case(i):
                                m.d.comb += vmask_uop.eq(
                                    self.vmask[i * stride:(i + 1) * stride])

        byte_mask = Signal(self.vlen_bytes)
        with m.Switch(self.vsew):
            for i in range(4):
                with m.Case(i):
                    m.d.comb += byte_mask.eq(
                        Cat(x.replicate(1 << i) for x in vmask_uop))

        vmask_byte = Signal(self.vlen_bytes)
        for i in range(self.vlen_bytes):
            with m.If(i < vl_remain_bytes):
                m.d.comb += vmask_byte[i].eq(byte_mask[i] | self.vm)

        vlmax_bytes = Signal(range(self.vlen_bytes + 1))
        m.d.comb += vlmax_bytes.eq(self.vlen_bytes)
        with m.If(self.vlmul_sign):
            with m.Switch(self.vlmul_mag):
                for i in range(1, 4):
                    m.d.comb += vlmax_bytes.eq(self.vlen_bytes >> (4 - i))

        vslide_offset = Mux((self.opcode == VOpCode.VSLIDE1UP) |
                            (self.opcode == VOpCode.VSLIDE1DOWN),
                            Const(1, self.xlen), self.rs1_data)
        vslide_bytes = vslide_offset << self.vsew
        vslide_byte_off = vslide_bytes[:log2_int(self.vlen_bytes)]

        old_vd_bytes = Array(self.old_vd[i * 8:(i + 1) * 8]
                             for i in range(self.vlen_bytes))
        vs2_lo_bytes = Array(self.vs2_lo.bits[i * 8:(i + 1) * 8]
                             for i in range(self.vlen_bytes))
        vs2_hi_bytes = Array(self.vs2_hi.bits[i * 8:(i + 1) * 8]
                             for i in range(self.vlen_bytes))
        rs1_bytes = Array(self.rs1_data[i * 8:(i + 1) * 8]
                          for i in range(self.xlen // 8))

        for i in range(self.vlen_bytes):
            with m.If(~self.vs2_lo.valid & ~self.vs2_hi.valid):
                m.d.comb += vslideup_data[i * 8:(i + 1) * 8].eq(
                    old_vd_bytes[i])
            with m.Elif(~self.vs2_lo.valid & self.vs2_hi.valid):
                with m.If(i < vslide_byte_off):
                    m.d.comb += vslideup_data[i * 8:(i + 1) * 8].eq(
                        old_vd_bytes[i])
                with m.Elif(vmask_byte[i]):
                    m.d.comb += vslideup_data[i * 8:(i + 1) * 8].eq(
                        vs2_hi_bytes[i - vslide_byte_off])
                with m.Else():
                    m.d.comb += vslideup_data[i * 8:(i + 1) * 8].eq(
                        Mux(self.vma, 0xff, old_vd_bytes[i]))
            with m.Elif(self.vs2_lo.valid & self.vs2_hi.valid):
                with m.If(vmask_byte[i]):
                    with m.If(i < vslide_byte_off):
                        m.d.comb += vslideup_data[i * 8:(i + 1) * 8].eq(
                            vs2_lo_bytes[self.vlen_bytes - vslide_byte_off +
                                         i])
                    with m.Else():
                        m.d.comb += vslideup_data[i * 8:(i + 1) * 8].eq(
                            vs2_hi_bytes[i - vslide_byte_off])
                with m.Else():
                    m.d.comb += vslideup_data[i * 8:(i + 1) * 8].eq(
                        Mux(self.vma, 0xff, old_vd_bytes[i]))

        for i in range(self.vlen_bytes):
            with m.If(vmask_byte[i]):
                with m.If((vslide_byte_off + i < vlmax_bytes)
                          & self.vs2_lo.valid):
                    m.d.comb += vslidedown_data[i * 8:(i + 1) * 8].eq(
                        vs2_lo_bytes[vslide_byte_off + i])
                with m.If((vslide_byte_off + i >= self.vlen_bytes)
                          & self.vs2_hi.valid):
                    m.d.comb += vslidedown_data[i * 8:(i + 1) * 8].eq(
                        vs2_hi_bytes[vslide_byte_off + i - self.vlen_bytes])
            with m.Else():
                m.d.comb += vslidedown_data[i * 8:(i + 1) * 8].eq(
                    Mux(self.vma, 0xff, old_vd_bytes[i]))

        m.d.comb += vslide1up_data.eq(vslideup_data)
        with m.If(self.uop_idx == 0):
            for i in range(self.xlen // 8):
                with m.If(i < vsew_bytes):
                    m.d.comb += vslide1up_data[i * 8:(i + 1) * 8].eq(
                        Mux(vmask_byte[i], rs1_bytes[i],
                            Mux(self.vma, 0xff, old_vd_bytes[i])))

        m.d.comb += vslide1down_data.eq(vslidedown_data)
        with m.If(self.uop_idx == vl_idx):
            for i in range(self.vlen_bytes):
                with m.If((i >= vl_remain_bytes - vsew_bytes) & vmask_byte[i]):
                    m.d.comb += vslide1down_data[i * 8:(i + 1) * 8].eq(
                        rs1_bytes[i + vsew_bytes - vl_remain_bytes])

        with m.Switch(self.opcode):
            with m.Case(VOpCode.VSLIDEUP):
                m.d.comb += self.resp_data.eq(vslideup_data)
            with m.Case(VOpCode.VSLIDEDOWN):
                m.d.comb += self.resp_data.eq(vslidedown_data)
            with m.Case(VOpCode.VSLIDE1UP):
                m.d.comb += self.resp_data.eq(vslide1up_data)
            with m.Case(VOpCode.VSLIDE1DOWN):
                m.d.comb += self.resp_data.eq(vslide1down_data)

        return m


class PermutationCore(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.uop = Valid(VMicroOp, params)
        self.rs1_data = Signal(self.xlen)
        self.lrs1_idx = Array(
            Signal(range(32), name=f'lrs1_idx{i}') for i in range(8))
        self.lrs2_idx = Array(
            Signal(range(32), name=f'lrs2_idx{i}') for i in range(8))
        self.old_vd_idx = Array(
            Signal(range(32), name=f'old_vd_idx{i}') for i in range(8))

        self.rd_port = RFReadPort(addr_width=5, data_width=self.vlen)

        self.wb_req = Valid(Signal, self.vlen)

        self.busy = Signal()

    def elaborate(self, platform):
        m = Module()

        uop = VMicroOp(self.params)
        with m.If(self.uop.valid):
            m.d.sync += uop.eq(self.uop.bits)

        is_vslideup = (uop.opcode == VOpCode.VSLIDEUP) | (uop.opcode
                                                          == VOpCode.VSLIDE1UP)
        is_vslidedown = (uop.opcode == VOpCode.VSLIDEDOWN) | (
            uop.opcode == VOpCode.VSLIDE1DOWN)
        is_vslide = is_vslideup | is_vslidedown

        rs1_data = Signal(self.xlen)
        rd_vlmul = Signal(range(8))
        with m.If(self.uop.valid):
            m.d.sync += rs1_data.eq(self.rs1_data)

            with m.If(self.uop.bits.vlmul_sign):
                m.d.sync += rd_vlmul.eq(0)
            with m.Else():
                m.d.sync += rd_vlmul.eq((1 << self.uop.bits.vlmul_mag) - 1)

        vs_idx = Signal(range(8))
        update_vs_idx = Signal()
        vd_idx = Signal(range(8))

        vs_rd_en = Signal()
        mask_rd_en = Signal()
        vs_rdata_valid = Signal()
        mask_rdata_valid = Signal()
        vs_data = Signal(self.vlen)
        old_vd_data = Signal(self.vlen)
        mask = Valid(Signal, self.vlen)
        m.d.sync += [
            vs_rdata_valid.eq(vs_rd_en),
            mask_rdata_valid.eq(mask_rd_en),
        ]

        with m.If(self.uop.valid):
            m.d.sync += mask_rd_en.eq(1)
        with m.Else():
            m.d.sync += mask_rd_en.eq(0)

        with m.If(is_vslide & mask_rd_en):
            m.d.sync += vs_rd_en.eq(1)
        with m.Elif(update_vs_idx & (vs_idx == rd_vlmul)):
            m.d.sync += vs_rd_en.eq(0)

        vl_remain = Signal(self.vl_bits)
        vl_remain_d1 = Signal.like(vl_remain)
        m.d.sync += vl_remain_d1.eq(vl_remain)

        wb_valid = Signal()
        wb_valid_d1 = Signal()
        wb_valid_d2 = Signal()
        wb_valid_d3 = Signal()
        m.d.sync += [
            wb_valid_d1.eq(wb_valid),
            wb_valid_d2.eq(wb_valid_d1),
            wb_valid_d3.eq(wb_valid_d2),
        ]

        wb_vd_idx = Signal(range(8))
        with m.If(wb_valid_d2):
            with m.If(wb_vd_idx == rd_vlmul):
                m.d.sync += wb_vd_idx.eq(0)
            with m.Else():
                m.d.sync += wb_vd_idx.eq(wb_vd_idx + 1)

        #
        # VSLIDEUP/VSLIDEDOWN
        #

        # Vector register read index generation

        vslide_offset = Mux((uop.opcode == VOpCode.VSLIDE1UP) |
                            (uop.opcode == VOpCode.VSLIDE1DOWN),
                            Const(1, self.xlen), rs1_data)
        vslide_bytes = vslide_offset << uop.vsew
        vslide_vreg_idx = vslide_bytes >> log2_int(self.vlen // 8)

        vslide_lo_valid = Signal()
        vslide_hi_valid = Signal()
        vslide_lo_valid_d1 = Signal()
        vslide_hi_valid_d1 = Signal()
        m.d.comb += [
            vslide_lo_valid.eq(
                Mux(
                    is_vslideup, vslide_vreg_idx + 1 <= vs_idx,
                    Mux(is_vslidedown, vs_idx + vslide_vreg_idx <= rd_vlmul,
                        0))),
            vslide_hi_valid.eq(
                Mux(
                    is_vslideup, vslide_vreg_idx <= vs_idx,
                    Mux(is_vslidedown, vs_idx + vslide_vreg_idx + 1
                        <= rd_vlmul, 0))),
        ]
        m.d.sync += [
            vslide_lo_valid_d1.eq(vslide_lo_valid),
            vslide_hi_valid_d1.eq(vslide_hi_valid),
        ]

        vslide_rd_cnt_max = vslide_lo_valid + vslide_hi_valid
        vslide_rd_cnt = Signal(range(3))
        vslide_rd_cnt_d1 = Signal(range(3))
        vslide_update_vs_idx = Signal()
        vslide_wb_valid = vslide_update_vs_idx
        m.d.comb += vslide_update_vs_idx.eq(
            is_vslide & (vslide_rd_cnt == vslide_rd_cnt_max) & vs_rd_en)
        m.d.sync += vslide_rd_cnt_d1.eq(vslide_rd_cnt)
        with m.If(is_vslide & vs_rd_en):
            with m.If(vslide_rd_cnt == vslide_rd_cnt_max):
                m.d.sync += vslide_rd_cnt.eq(0)
            with m.Else():
                m.d.sync += vslide_rd_cnt.eq(vslide_rd_cnt + 1)

        vslide_lo_idx = Signal(range(8))
        vslide_hi_idx = Signal(range(8))
        with m.If(is_vslideup):
            m.d.comb += [
                vslide_lo_idx.eq(vs_idx - vslide_vreg_idx - 1),
                vslide_hi_idx.eq(vs_idx - vslide_vreg_idx),
            ]
        with m.Elif(is_vslidedown):
            m.d.comb += [
                vslide_lo_idx.eq(vs_idx + vslide_vreg_idx),
                vslide_hi_idx.eq(vs_idx + vslide_vreg_idx + 1),
            ]

        vslide_lrs_idx = Signal(range(32))
        with m.If(mask_rd_en):
            m.d.comb += vslide_lrs_idx.eq(0)
        with m.Elif(is_vslide & vs_rd_en):
            with m.If(vslide_rd_cnt == 0):
                m.d.comb += vslide_lrs_idx.eq(self.old_vd_idx[vs_idx])
            with m.Elif((vslide_rd_cnt == 1) & vslide_lo_valid):
                m.d.comb += vslide_lrs_idx.eq(self.lrs2_idx[vslide_lo_idx])
            with m.Elif((vslide_rd_cnt == 1) & ~vslide_lo_valid
                        & vslide_hi_valid):
                m.d.comb += vslide_lrs_idx.eq(self.lrs2_idx[vslide_hi_idx])
            with m.Elif(vslide_rd_cnt == 2):
                m.d.comb += vslide_lrs_idx.eq(self.lrs2_idx[vslide_hi_idx])

        # Vector register read data

        vslide_vs2_lo = Signal(self.vlen)
        vslide_vs2_hi = Signal(self.vlen)
        vslide_old_vd = Signal(self.vlen)

        with m.If(vs_rdata_valid):
            with m.If(is_vslideup):
                with m.If(~vslide_lo_valid_d1 & ~vslide_hi_valid_d1):
                    m.d.sync += vslide_old_vd.eq(self.rd_port.data)
                with m.Elif(~vslide_lo_valid_d1 & vslide_hi_valid_d1):
                    m.d.sync += [
                        vslide_old_vd.eq(old_vd_data),
                        vslide_vs2_hi.eq(self.rd_port.data),
                    ]
                with m.Elif(vslide_lo_valid_d1 & vslide_hi_valid_d1):
                    m.d.sync += [
                        vslide_old_vd.eq(old_vd_data),
                        vslide_vs2_lo.eq(vs_data),
                        vslide_vs2_hi.eq(self.rd_port.data),
                    ]

            with m.Elif(is_vslidedown):
                with m.If(vslide_lo_valid_d1 | vslide_hi_valid_d1):
                    m.d.sync += vslide_old_vd.eq(old_vd_data)
                with m.Else():
                    m.d.sync += vslide_old_vd.eq(self.rd_port.data)

                with m.If(vslide_lo_valid_d1 & vslide_hi_valid_d1):
                    m.d.sync += vslide_vs2_lo.eq(vs_data)
                with m.Else():
                    m.d.sync += vslide_vs2_lo.eq(self.rd_port.data)

                m.d.sync += vslide_vs2_hi.eq(self.rd_port.data)

        # Slide unit
        slide_unit = m.submodules.vslide = VSlideUnit(self.params)
        m.d.comb += [
            slide_unit.opcode.eq(uop.opcode),
            slide_unit.vm.eq(uop.vm),
            slide_unit.vma.eq(uop.vma),
            slide_unit.vsew.eq(uop.vsew),
            slide_unit.vlmul_sign.eq(uop.vlmul_sign),
            slide_unit.vlmul_mag.eq(uop.vlmul_mag),
            slide_unit.vl.eq(uop.vl),
            slide_unit.uop_idx.eq(wb_vd_idx),
            slide_unit.rs1_data.eq(rs1_data),
            slide_unit.vs2_lo.bits.eq(vslide_vs2_lo),
            slide_unit.vs2_hi.bits.eq(vslide_vs2_hi),
            slide_unit.old_vd.eq(vslide_old_vd),
            slide_unit.vmask.eq(mask.bits),
        ]
        m.d.sync += [
            slide_unit.vs2_lo.valid.eq(vslide_lo_valid_d1),
            slide_unit.vs2_hi.valid.eq(vslide_hi_valid_d1),
        ]

        m.d.comb += [
            update_vs_idx.eq(vslide_update_vs_idx),
            wb_valid.eq(vslide_wb_valid),
        ]
        with m.If(self.uop.valid):
            m.d.sync += vs_idx.eq(0)
        with m.Elif(update_vs_idx):
            with m.If(vs_idx == rd_vlmul):
                m.d.sync += vs_idx.eq(0)
            with m.Else():
                m.d.sync += vs_idx.eq(vs_idx + 1)

        update_vd_idx = wb_valid_d3
        with m.If(self.uop.valid):
            m.d.sync += vd_idx.eq(0)
        with m.Elif(update_vd_idx):
            with m.If(vd_idx == rd_vlmul):
                m.d.sync += vd_idx.eq(0)
            with m.Else():
                m.d.sync += vd_idx.eq(vd_idx + 1)

        with m.If(self.uop.valid):
            m.d.sync += vl_remain.eq(self.uop.bits.vl)
        with m.Elif(wb_valid_d2):
            with m.Switch(uop.vsew):
                for w in range(4):
                    with m.Case(w):
                        n = 1 << (3 + w)
                        vreg_elems = self.vlen // n
                        m.d.sync += vl_remain.eq(
                            Mux(vl_remain > vreg_elems, vl_remain - vreg_elems,
                                0))

        with m.If(is_vslide):
            m.d.comb += self.rd_port.addr.eq(vslide_lrs_idx)

        with m.If(mask_rdata_valid):
            m.d.sync += [
                mask.bits.eq(self.rd_port.data),
                mask.valid.eq(1),
            ]
        with m.Else():
            m.d.sync += mask.valid.eq(0)

        with m.If(is_vslide & ~mask_rdata_valid & vs_rdata_valid
                  & (vslide_rd_cnt_d1 == 0)):
            m.d.sync += old_vd_data.eq(self.rd_port.data)
        with m.If(is_vslide & ~mask_rdata_valid & vs_rdata_valid
                  & (vslide_rd_cnt_d1 == 1)):
            m.d.sync += vs_data.eq(self.rd_port.data)

        vd_reg = Signal(self.vlen)
        with m.If(is_vslide & wb_valid_d2):
            m.d.sync += vd_reg.eq(slide_unit.resp_data)

        vl_remain_bytes_d1 = vl_remain_d1 << uop.vsew
        tail_bytes = Mux(vl_remain_bytes_d1 >= self.vlen_bytes, 0,
                         self.vlen_bytes - vl_remain_bytes_d1)
        vd_mask = Signal(self.vlen)
        with m.Switch(tail_bytes):
            for i in range(self.vlen_bytes):
                with m.Case(i):
                    n = (self.vlen_bytes - i) << 3
                    m.d.comb += vd_mask.eq((1 << n) - 1)

        vd_masked_data = Signal(self.vlen)
        m.d.comb += vd_masked_data.eq(vd_reg)
        with m.If(update_vd_idx):
            with m.If(uop.vta):
                m.d.comb += vd_masked_data.eq(vd_reg | ~vd_mask)
            with m.Else():
                m.d.comb += vd_masked_data.eq((vd_reg & vd_mask)
                                              | (old_vd_data & ~vd_mask))

        m.d.comb += self.wb_req.bits.eq(vd_masked_data)
        with m.If(is_vslide):
            m.d.comb += self.wb_req.valid.eq(wb_valid_d3)

        with m.If(self.uop.valid):
            m.d.sync += self.busy.eq(1)
        with m.Elif(is_vslide & (vd_idx == rd_vlmul) & update_vd_idx):
            m.d.sync += self.busy.eq(0)

        return m


class VPermutationUnit(IterativeFunctionalUnit):

    def __init__(self, params):
        super().__init__(params)

        self.rd_port = RFReadPort(addr_width=5, data_width=self.vlen)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop_q = m.submodules.uop_q = Queue(8, VMicroOp, self.params)
        m.d.comb += [
            uop_q.enq.valid.eq(self.req.valid),
            uop_q.enq.bits.eq(self.req.bits.uop),
        ]

        rs1_data = Signal(self.xlen)
        lrs1_idx = [Signal(range(32), name=f'lrs1_idx{i}') for i in range(8)]
        lrs2_idx = [Signal(range(32), name=f'lrs2_idx{i}') for i in range(8)]
        old_vd_idx = [
            Signal(range(32), name=f'old_vd_idx{i}') for i in range(8)
        ]

        uop = Valid(VMicroOp, self.params)
        m.d.sync += uop.valid.eq(0)
        with m.If(self.req.fire):
            m.d.sync += [
                uop.bits.eq(self.req.bits.uop),
                rs1_data.eq(self.req.bits.rs1_data),
            ]

            with m.Switch(self.req.bits.uop.expd_idx):
                for i in range(8):
                    with m.Case(i):
                        m.d.sync += [
                            lrs1_idx[i].eq(self.req.bits.uop.lrs1),
                            lrs2_idx[i].eq(self.req.bits.uop.lrs2),
                            old_vd_idx[i].eq(self.req.bits.uop.ldst),
                        ]

            with m.If(self.req.bits.uop.expd_end):
                m.d.sync += uop.valid.eq(1)

        perm = m.submodules.perm = PermutationCore(self.params)
        m.d.comb += [
            perm.uop.eq(uop),
            perm.rs1_data.eq(rs1_data),
            perm.rd_port.connect(self.rd_port),
        ]
        for i in range(8):
            m.d.comb += [
                perm.lrs1_idx[i].eq(lrs1_idx[i]),
                perm.lrs2_idx[i].eq(lrs2_idx[i]),
                perm.old_vd_idx[i].eq(old_vd_idx[i]),
            ]

        wb_data_q = m.submodules.wb_data_q = Queue(8, Signal, self.vlen)
        m.d.comb += [
            wb_data_q.enq.valid.eq(perm.wb_req.valid),
            wb_data_q.enq.bits.eq(perm.wb_req.bits),
        ]

        m.d.comb += [
            self.req.ready.eq(~uop.valid & ~perm.busy & ~wb_data_q.deq.valid),
            self.resp.valid.eq(uop_q.deq.valid & wb_data_q.deq.valid),
            self.resp.bits.uop.eq(uop_q.deq.bits),
            self.resp.bits.vd_data.eq(wb_data_q.deq.bits),
            uop_q.deq.ready.eq(self.resp.ready & wb_data_q.deq.valid),
            wb_data_q.deq.ready.eq(self.resp.ready & uop_q.deq.valid),
        ]

        return m
