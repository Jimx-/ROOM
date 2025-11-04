from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.utils import log2_int

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp
from vroom.fu import ExecReq, ExecResp
from vroom.utils import TailGen

from room.consts import MemoryCommand
from room.mmu import CoreMemRequest, CoreMemResponse
from room.utils import Decoupled, Valid

from roomsoc.interconnect.stream import Queue


class IndexData(Record):

    def __init__(self, name=None, src_loc_at=0):
        Record.__init__(self, [
            ('index', 64, Direction.FANOUT),
            ('mask', 1, Direction.FANOUT),
            ('last', 1, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class StrideClassifier(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.mem_size = Signal(2)
        self.stride = Signal(self.xlen)

        self.out = Signal(4)
        self.log_stride = Signal(2)

    def elaborate(self, platform):
        m = Module()

        with m.If(self.stride == 0):
            m.d.comb += self.out[0].eq(1)
        with m.Elif(((self.mem_size == 0)
                     & ((self.stride == 1) | (self.stride == -1)))
                    | ((self.mem_size == 1)
                       & ((self.stride == 2) | (self.stride == -2)))
                    | ((self.mem_size == 2)
                       & ((self.stride == 4) | (self.stride == -4)))
                    | ((self.mem_size == 3)
                       & ((self.stride == 8) | (self.stride == -8)))):
            m.d.comb += self.out[1].eq(1)
        with m.Elif(((self.mem_size == 0)
                     & ((self.stride == 2) | (self.stride == -2)))
                    | ((self.mem_size == 1)
                       & ((self.stride == 4) | (self.stride == -4)))
                    | ((self.mem_size == 2)
                       & ((self.stride == 8) | (self.stride == -8)))):
            m.d.comb += self.out[2].eq(1)
        with m.Elif(((self.mem_size == 0)
                     & ((self.stride == 4) | (self.stride == -4)))):
            m.d.comb += self.out[3].eq(1)

        m.d.comb += self.log_stride.eq(3)
        with m.If(self.out[1]):
            m.d.comb += self.log_stride.eq(0)
        with m.Elif(self.out[2]):
            m.d.comb += self.log_stride.eq(1)
        with m.Elif(self.out[3]):
            m.d.comb += self.log_stride.eq(2)

        return m


class BaseLoadGenerator(HasVectorParams, Elaboratable):

    class Response(HasVectorParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasVectorParams.__init__(self, params)

            Record.__init__(self, [
                ('addr', self.xlen, Direction.FANOUT),
                ('vdest', range(32), Direction.FANOUT),
                ('elem_idx', range(self.max_vlmax), Direction.FANOUT),
                ('elem_offset', range(self.max_elem_count), Direction.FANOUT),
                ('elem_count', range(self.max_elem_count + 1),
                 Direction.FANOUT),
                ('mask', self.xlen // 8, Direction.FANOUT),
                ('mask_valid', 1, Direction.FANOUT),
                ('dir', 1, Direction.FANOUT),
                ('last', 1, Direction.FANOUT),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params, with_index=False):
        super().__init__(params)

        self.req = Valid(ExecResp, params)
        self.resp = Decoupled(BaseLoadGenerator.Response, params)

        if with_index:
            self.idx_data = Decoupled(IndexData)


class PackedLoadGenerator(BaseLoadGenerator):

    def elaborate(self, platform):
        m = Module()

        stride_cls = StrideClassifier(self.params)
        m.submodules += stride_cls
        m.d.comb += [
            stride_cls.mem_size.eq(self.req.bits.uop.mem_size),
            stride_cls.stride.eq(self.req.bits.stride),
        ]

        cur_addr = Signal(self.xlen)
        cur_vdest = Signal(range(32))
        cur_offset = Signal(range(self.max_elem_count))
        log_stride = Signal(2)
        cur_max = Signal(range(self.max_elem_count + 1))
        cur_vmax = Signal(range(self.vlen_bytes + 1))
        cur_idx = Signal(range(self.max_vlmax))
        cur_vidx = Signal(range(self.vlen_bytes))
        cur_vl = Signal(self.vl_bits)
        is_masked = Signal()
        cur_mask_data = Signal(self.vlen)
        cur_mask_offset = Signal(range(self.vlen))

        rem_line = cur_max - (cur_offset >> log_stride)
        rem_vreg = cur_vmax - cur_vidx
        rem_vl = cur_vl - cur_idx

        vreg_count = Mux(rem_line > rem_vreg, rem_vreg, rem_line)
        vl_count = Mux(vreg_count > rem_vl, rem_vl, vreg_count)

        next_vreg = vl_count == rem_vreg
        next_line = vl_count == rem_line

        m.d.comb += [
            self.resp.bits.addr.eq(cur_addr),
            self.resp.bits.vdest.eq(cur_vdest),
            self.resp.bits.elem_idx.eq(cur_idx),
            self.resp.bits.elem_offset.eq(cur_offset),
            self.resp.bits.elem_count.eq(vl_count),
            self.resp.bits.mask_valid.eq(is_masked),
            self.resp.bits.mask.eq(cur_mask_data
                                   & ((1 << self.resp.bits.elem_count) - 1)),
            self.resp.bits.last.eq(vl_count == rem_vl),
        ]

        with m.FSM():
            with m.State('IDLE'):
                with m.If(self.req.valid):
                    log_stride_n = Mux(self.req.bits.uop.unit_stride, 0,
                                       stride_cls.log_stride)

                    m.d.sync += [
                        cur_addr.eq(self.req.bits.base_addr),
                        cur_vdest.eq(self.req.bits.uop.ldst),
                        cur_offset.eq(
                            self.req.bits.base_addr[:log2_int(self.xlen // 8)]
                            >> self.req.bits.uop.mem_size),
                        log_stride.eq(log_stride_n),
                        self.resp.bits.dir.eq(self.req.bits.uop.strided
                                              & self.req.bits.stride[-1]),
                        cur_max.eq((self.xlen // 8) >> (
                            self.req.bits.uop.mem_size + log_stride_n)),
                        cur_vmax.eq(
                            self.vlen_bytes >> self.req.bits.uop.mem_size),
                        cur_idx.eq(0),
                        cur_vidx.eq(0),
                        cur_vl.eq(self.req.bits.uop.vl),
                        is_masked.eq(~self.req.bits.uop.vm),
                        cur_mask_data.eq(self.req.bits.mask),
                        cur_mask_offset.eq(0),
                    ]

                    m.next = 'PACK'

            with m.State('PACK'):
                m.d.comb += self.resp.valid.eq(1)

                with m.If(self.resp.fire):
                    m.d.sync += [
                        cur_idx.eq(cur_idx + self.resp.bits.elem_count),
                        cur_mask_offset.eq(cur_mask_offset +
                                           self.resp.bits.elem_count),
                        cur_mask_data.eq(
                            cur_mask_data >> self.resp.bits.elem_count),
                    ]

                    with m.If(next_vreg):
                        m.d.sync += [
                            cur_vdest.eq(cur_vdest + 1),
                            cur_vidx.eq(0),
                        ]
                    with m.Else():
                        m.d.sync += cur_vidx.eq(cur_vidx +
                                                self.resp.bits.elem_count)

                    with m.If(next_line):
                        m.d.sync += cur_offset.eq(0)

                        with m.If(self.resp.bits.dir):
                            m.d.sync += cur_addr.eq(cur_addr - self.xlen // 8)
                        with m.Else():
                            m.d.sync += cur_addr.eq(cur_addr + self.xlen // 8)
                    with m.Else():
                        m.d.sync += cur_offset.eq(cur_offset + (
                            self.resp.bits.elem_count << log_stride))

                    with m.If(self.resp.bits.last):
                        m.next = 'IDLE'

        return m


class IndexedLoadGenerator(BaseLoadGenerator):

    def __init__(self, params):
        super().__init__(params, with_index=True)

    def elaborate(self, platform):
        m = Module()

        cur_uop = VMicroOp(self.params)
        stride = Signal(self.xlen)
        dest_eew = Mux(cur_uop.indexed, cur_uop.vsew, cur_uop.mem_size)

        base_addr = Signal(self.xlen)
        cur_addr = Signal(self.xlen)
        next_addr = Signal(self.xlen)
        cur_vdest = Signal(range(32))
        cur_offset = cur_addr[:log2_int(self.xlen // 8)] >> dest_eew
        cur_max = Signal(range(self.max_elem_count + 1))
        cur_vmax = Signal(range(self.vlen_bytes + 1))
        cur_idx = Signal(range(self.max_vlmax))
        cur_vidx = Signal(range(self.vlen_bytes))
        cur_vl = Signal(self.vl_bits)
        is_masked = Signal()
        cur_mask_data = Signal(self.vlen)
        cur_mask_offset = Signal(range(self.vlen))

        rem_seg = 1
        rem_line = cur_max - cur_offset
        rem_vreg = cur_vmax - cur_vidx
        rem_vl = cur_vl - cur_idx

        seg_count = Mux(rem_line > rem_seg, rem_seg, rem_line)
        vreg_count = Mux(seg_count > rem_vreg, rem_vreg, seg_count)
        vl_count = Mux(vreg_count > rem_vl, rem_vl, vreg_count)

        next_seg = vl_count == rem_seg
        next_vreg = vl_count == rem_vreg

        m.d.comb += [
            self.resp.bits.addr.eq(cur_addr),
            self.resp.bits.vdest.eq(cur_vdest),
            self.resp.bits.elem_idx.eq(cur_idx),
            self.resp.bits.elem_offset.eq(cur_offset),
            self.resp.bits.elem_count.eq(vl_count),
            self.resp.bits.mask_valid.eq(is_masked),
            self.resp.bits.mask.eq(cur_mask_data
                                   & ((1 << self.resp.bits.elem_count) - 1)),
            self.resp.bits.last.eq(vl_count == rem_vl),
        ]

        with m.FSM():
            with m.State('IDLE'):
                with m.If(self.req.valid):
                    m.d.sync += [
                        cur_uop.eq(self.req.bits.uop),
                        stride.eq(self.req.bits.stride),
                        base_addr.eq(self.req.bits.base_addr),
                        cur_addr.eq(self.req.bits.base_addr),
                        cur_vdest.eq(self.req.bits.uop.ldst),
                        self.resp.bits.dir.eq(self.req.bits.uop.strided
                                              & self.req.bits.stride[-1]),
                        cur_max.eq((self.xlen // 8) >> Mux(
                            self.req.bits.uop.indexed, self.req.bits.uop.vsew,
                            self.req.bits.uop.mem_size)),
                        cur_vmax.eq(
                            self.vlen_bytes >> self.req.bits.uop.mem_size),
                        cur_idx.eq(0),
                        cur_vidx.eq(0),
                        cur_vl.eq(self.req.bits.uop.vl),
                        is_masked.eq(~self.req.bits.uop.vm),
                        cur_mask_data.eq(self.req.bits.mask),
                        cur_mask_offset.eq(0),
                    ]

                    with m.If(self.req.bits.uop.indexed):
                        m.next = 'WAIT_START'
                    with m.Else():
                        m.next = 'SCAN'

            with m.State('WAIT_START'):
                m.d.comb += [
                    self.idx_data.ready.eq(1),
                    next_addr.eq(self.req.bits.base_addr +
                                 self.idx_data.bits.index),
                ]

                with m.If(self.idx_data.valid):
                    m.d.sync += cur_addr.eq(next_addr)
                    m.next = 'SCAN'

            with m.State('SCAN'):
                m.d.comb += [
                    self.resp.valid.eq(~cur_uop.indexed
                                       | self.idx_data.valid
                                       | self.resp.bits.last),
                    next_addr.eq(
                        Mux(cur_uop.indexed,
                            base_addr + self.idx_data.bits.index,
                            cur_addr + stride)),
                ]

                with m.If(self.resp.fire):
                    m.d.sync += [
                        cur_idx.eq(cur_idx + self.resp.bits.elem_count),
                        cur_mask_offset.eq(cur_mask_offset +
                                           self.resp.bits.elem_count),
                        cur_mask_data.eq(
                            cur_mask_data >> self.resp.bits.elem_count),
                    ]

                    with m.If(next_vreg):
                        m.d.sync += [
                            cur_vdest.eq(cur_vdest + 1),
                            cur_vidx.eq(0),
                        ]
                    with m.Else():
                        m.d.sync += cur_vidx.eq(cur_vidx +
                                                self.resp.bits.elem_count)

                    with m.If(next_seg):
                        m.d.comb += self.idx_data.ready.eq(1)
                        m.d.sync += cur_addr.eq(next_addr)

                    with m.If(self.resp.bits.last):
                        m.next = 'IDLE'

        return m


class VLoadGenerator(BaseLoadGenerator):

    def __init__(self, params):
        super().__init__(params, with_index=True)

    def elaborate(self, platform):
        m = Module()

        stride_cls = StrideClassifier(self.params)
        m.submodules += stride_cls
        m.d.comb += [
            stride_cls.mem_size.eq(self.req.bits.uop.mem_size),
            stride_cls.stride.eq(self.req.bits.stride),
        ]

        packed_gen = m.submodules.packed_gen = PackedLoadGenerator(self.params)
        m.d.comb += packed_gen.req.bits.eq(self.req.bits)

        indexed_gen = m.submodules.indexed_gen = IndexedLoadGenerator(
            self.params)
        m.d.comb += [
            indexed_gen.req.bits.eq(self.req.bits),
            self.idx_data.connect(indexed_gen.idx_data),
        ]

        with m.FSM():
            with m.State('IDLE'):
                can_pack = self.req.bits.uop.unit_stride | (
                    self.req.bits.uop.strided & stride_cls.out[1:].any())

                with m.If(self.req.valid & self.req.bits.uop.vl.any()):
                    with m.If(can_pack):
                        m.d.comb += packed_gen.req.valid.eq(1)
                        m.next = 'PACK'

                    with m.Else():
                        m.d.comb += indexed_gen.req.valid.eq(1)
                        m.next = 'SCAN'

            with m.State('PACK'):
                m.d.comb += packed_gen.resp.connect(self.resp)

                with m.If(self.resp.fire & self.resp.bits.last):
                    m.next = 'IDLE'

            with m.State('SCAN'):
                m.d.comb += indexed_gen.resp.connect(self.resp)

                with m.If(self.resp.fire & self.resp.bits.last):
                    m.next = 'IDLE'
        return m


class VStorePacker(HasVectorParams, Elaboratable):

    class Response(HasVectorParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasVectorParams.__init__(self, params)

            Record.__init__(self, [
                ('data_incr', range(self.vlen // 8), Direction.FANOUT),
                ('next_vreg', 1, Direction.FANOUT),
                ('mem_size', 2, Direction.FANOUT),
                ('last', 1, Direction.FANOUT),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params)

        self.req = Valid(ExecResp, params)
        self.resp = Decoupled(VStorePacker.Response, params)

    def elaborate(self, platform):
        m = Module()

        mem_size = Signal.like(self.req.bits.uop.mem_size)
        cur_offset = Signal(range(self.max_elem_count))
        cur_max = Signal(range(self.max_elem_count + 1))
        cur_vmax = Signal(range(self.vlen_bytes + 1))
        cur_idx = Signal(range(self.max_vlmax))
        cur_vidx = Signal(range(self.vlen_bytes))
        cur_vl = Signal(self.vl_bits)

        def round_down(rem_rnd, rem):
            for i in range(len(rem)):
                with m.If(rem[i]):
                    m.d.comb += rem_rnd.eq(1 << i)

        rem_line = cur_max - cur_offset
        rem_vreg = cur_vmax - cur_vidx
        rem_vl = cur_vl - cur_idx

        rem_line_rnd = Signal.like(rem_line)
        rem_vreg_rnd = Signal.like(rem_vreg)
        rem_vl_rnd = Signal.like(rem_vl)
        round_down(rem_line_rnd, rem_line)
        round_down(rem_vreg_rnd, rem_vreg)
        round_down(rem_vl_rnd, rem_vl)

        vreg_count = Mux(rem_line_rnd > rem_vreg_rnd, rem_vreg_rnd,
                         rem_line_rnd)
        vl_count = Mux(vreg_count > rem_vl_rnd, rem_vl_rnd, vreg_count)

        m.d.comb += [
            self.resp.bits.data_incr.eq(vl_count << mem_size),
            self.resp.bits.next_vreg.eq(vl_count == rem_vreg),
            self.resp.bits.last.eq(vl_count == rem_vl),
        ]

        m.d.comb += self.resp.bits.mem_size.eq(mem_size)
        with m.Switch(vl_count):
            for i in range(4):
                with m.Case(1 << i):
                    m.d.comb += self.resp.bits.mem_size.eq(mem_size + i)

        with m.FSM():
            with m.State('IDLE'):
                with m.If(self.req.valid):
                    with m.If(self.req.bits.uop.unit_stride
                              & ~self.req.bits.uop.mask
                              & self.req.bits.uop.vl.any()):

                        m.d.sync += [
                            mem_size.eq(self.req.bits.uop.mem_size),
                            cur_offset.eq(
                                self.req.bits.base_addr[:log2_int(self.xlen //
                                                                  8)] >>
                                self.req.bits.uop.mem_size),
                            cur_max.eq((self.xlen //
                                        8) >> self.req.bits.uop.mem_size),
                            cur_vmax.eq(
                                self.vlen_bytes >> self.req.bits.uop.mem_size),
                            cur_idx.eq(0),
                            cur_vidx.eq(0),
                            cur_vl.eq(self.req.bits.uop.vl),
                        ]

                        m.next = 'PACK'

            with m.State('PACK'):
                m.d.comb += self.resp.valid.eq(1)

                with m.If(self.resp.fire):
                    m.d.sync += cur_idx.eq(cur_idx + vl_count)

                    with m.If(self.resp.bits.next_vreg):
                        m.d.sync += cur_vidx.eq(0)
                    with m.Else():
                        m.d.sync += cur_vidx.eq(cur_vidx + vl_count)

                    with m.If(cur_offset + vl_count == cur_max):
                        m.d.sync += cur_offset.eq(0)
                    with m.Else():
                        m.d.sync += cur_offset.eq(cur_offset + vl_count)

                    with m.If(self.resp.bits.last):
                        m.next = 'IDLE'

        return m


class StoreDataBuffer(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.write = Valid(Signal, self.vlen)
        self.read = Decoupled(Signal, self.xlen)

        self.data_incr = Signal(range(self.vlen // 8))
        self.skip = Signal()

        self.clear = Signal()

    def elaborate(self, platform):
        m = Module()

        buffer = Array(Signal(self.vlen, name=f'buffer{i}') for i in range(8))
        r_ptr = Signal(range(8))
        w_ptr = Signal(range(8))

        with m.If(self.write.valid):
            m.d.sync += [
                buffer[w_ptr].eq(self.write.bits),
                w_ptr.eq(w_ptr + 1),
            ]

        cur_idx = Signal(range(self.vlen // 8))
        with m.If(self.clear):
            m.d.sync += cur_idx.eq(0)

        m.d.comb += self.read.valid.eq(1)
        with m.Switch(cur_idx):
            for i in range(self.vlen // 8):
                with m.Case(i):
                    m.d.comb += self.read.bits.eq(buffer[r_ptr][i * 8:i * 8 +
                                                                self.xlen])

        with m.If(self.read.fire):
            with m.If(self.skip):
                m.d.sync += [
                    r_ptr.eq(r_ptr + 1),
                    cur_idx.eq(0),
                ]
            with m.Else():
                m.d.sync += cur_idx.eq(cur_idx + self.data_incr)

        return m


class MemTransactionGenerator(HasVectorParams, Elaboratable):

    class LoadResponse(HasVectorParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasVectorParams.__init__(self, params)

            Record.__init__(self, [
                ('vdest', range(32), Direction.FANOUT),
                ('elem_idx', range(self.max_vlmax), Direction.FANOUT),
                ('elem_offset', range(self.max_elem_count), Direction.FANOUT),
                ('elem_count', range(self.max_elem_count + 1),
                 Direction.FANOUT),
                ('data', self.xlen, Direction.FANOUT),
                ('mask', self.xlen // 8, Direction.FANOUT),
                ('mask_valid', 1, Direction.FANOUT),
                ('last', 1, Direction.FANOUT),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params)

        self.req = Decoupled(ExecResp, params)
        self.done = Signal()

        self.ld_resp = Valid(MemTransactionGenerator.LoadResponse, params)
        self.idx_data = Decoupled(IndexData)
        self.st_data = Valid(Signal, self.vlen)

        self.mem_req = Decoupled(CoreMemRequest, params)
        self.mem_nack = Signal()
        self.mem_resp = Valid(CoreMemResponse, params)

    def elaborate(self, platform):
        m = Module()

        req = ExecResp(self.params)

        ld_gen = m.submodules.ld_gen = VLoadGenerator(self.params)
        m.d.comb += ld_gen.req.bits.eq(self.req.bits)

        st_packer = m.submodules.st_packer = VStorePacker(self.params)
        m.d.comb += st_packer.req.bits.eq(self.req.bits)

        st_data_buf = m.submodules.st_data_buf = StoreDataBuffer(self.params)
        m.d.comb += st_data_buf.write.eq(self.st_data)

        idx_queue = m.submodules.idx_queue = Queue(4, IndexData, flow=True)
        m.d.comb += self.idx_data.connect(idx_queue.enq)

        cur_addr = Signal(self.xlen)
        m.d.comb += [
            self.ld_resp.bits.vdest.eq(ld_gen.resp.bits.vdest),
            self.ld_resp.bits.elem_idx.eq(ld_gen.resp.bits.elem_idx),
            self.ld_resp.bits.elem_offset.eq(ld_gen.resp.bits.elem_offset),
            self.ld_resp.bits.elem_count.eq(ld_gen.resp.bits.elem_count),
            self.ld_resp.bits.mask_valid.eq(ld_gen.resp.bits.mask_valid),
            self.ld_resp.bits.mask.eq(ld_gen.resp.bits.mask),
            self.ld_resp.bits.last.eq(ld_gen.resp.bits.last),
            self.ld_resp.bits.data.eq(self.mem_resp.bits.data),
        ]

        st_mem_size = st_packer.resp.bits.mem_size

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += [
                    self.req.ready.eq(1),
                    self.done.eq(1),
                ]

                with m.If(self.req.fire):
                    m.d.sync += [
                        req.eq(self.req.bits),
                        cur_addr.eq(self.req.bits.base_addr),
                    ]

                    with m.If(self.req.bits.uop.is_ld):
                        m.d.comb += ld_gen.req.valid.eq(1)
                        m.next = 'LOAD_REQ'
                    with m.Else():
                        m.d.comb += [
                            st_packer.req.valid.eq(1),
                            st_data_buf.clear.eq(1),
                        ]
                        m.next = 'STORE_REQ'

            with m.State('LOAD_REQ'):
                m.d.comb += [
                    self.mem_req.valid.eq(ld_gen.resp.valid),
                    self.mem_req.bits.cmd.eq(MemoryCommand.READ),
                    self.mem_req.bits.addr.eq(ld_gen.resp.bits.addr),
                    self.mem_req.bits.size.eq(log2_int(self.xlen // 8)),
                    idx_queue.deq.connect(ld_gen.idx_data),
                ]

                with m.If(self.mem_req.fire):
                    m.next = 'LOAD_RESP'

            with m.State('LOAD_RESP'):
                m.d.comb += idx_queue.deq.connect(ld_gen.idx_data)

                with m.If(self.mem_nack):
                    m.next = 'LOAD_REQ'

                with m.Elif(self.mem_resp.valid):
                    m.d.comb += [
                        ld_gen.resp.ready.eq(1),
                        self.ld_resp.valid.eq(1),
                    ]

                    with m.If(ld_gen.resp.bits.last):
                        m.next = 'IDLE'
                    with m.Else():
                        m.next = 'LOAD_REQ'

            with m.State('STORE_REQ'):
                m.d.comb += [
                    self.mem_req.valid.eq(1),
                    self.mem_req.bits.cmd.eq(MemoryCommand.WRITE),
                    self.mem_req.bits.addr.eq(cur_addr),
                    self.mem_req.bits.data.eq(st_data_buf.read.bits),
                    self.mem_req.bits.size.eq(st_mem_size),
                ]

                with m.If(self.mem_req.fire):
                    m.next = 'STORE_RESP'

            with m.State('STORE_RESP'):
                with m.If(self.mem_nack):
                    m.next = 'STORE_REQ'

                with m.Elif(self.mem_resp.valid):
                    m.d.comb += [
                        st_packer.resp.ready.eq(1),
                        st_data_buf.read.ready.eq(1),
                        st_data_buf.skip.eq(st_packer.resp.bits.last
                                            | st_packer.resp.bits.next_vreg),
                        st_data_buf.data_incr.eq(
                            st_packer.resp.bits.data_incr),
                    ]

                    with m.If(st_packer.resp.valid):
                        m.d.sync += cur_addr.eq(cur_addr +
                                                st_packer.resp.bits.data_incr)

                    with m.If(st_packer.resp.bits.last):
                        m.next = 'IDLE'
                    with m.Else():
                        m.next = 'STORE_REQ'

        return m


class LoadStoreUnit(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.exec_req = Decoupled(ExecResp, params)
        self.exec_resp = Decoupled(ExecResp, params)

        self.mem_req = Decoupled(CoreMemRequest, params)
        self.mem_nack = Signal()
        self.mem_resp = Valid(CoreMemResponse, params)

    def elaborate(self, platform):
        m = Module()

        mem_txn = m.submodules.mem_txn = MemTransactionGenerator(self.params)
        m.d.comb += [
            mem_txn.req.bits.eq(self.exec_req.bits),
            mem_txn.st_data.valid.eq(self.exec_req.fire
                                     & self.exec_req.bits.uop.is_st),
            mem_txn.st_data.bits.eq(self.exec_req.bits.old_vd),
            mem_txn.mem_req.connect(self.mem_req),
            mem_txn.mem_nack.eq(self.mem_nack),
            mem_txn.mem_resp.eq(self.mem_resp),
        ]

        #
        # Request
        #

        req = ExecResp(self.params)
        req_sew = req.uop.vsew
        req_vlmul_mag = req.uop.vlmul_mag
        req_vlmul_sign = req.uop.vlmul_sign
        req_eew = req.uop.funct3[:2]
        req_dest_eew = req.uop.mem_size
        req_vstart = 0
        req_vl = req.uop.vl
        req_nf = req.uop.funct6[3:]

        uop_table = [
            Valid(VMicroOp, self.params, name=f'uop_table{i}')
            for i in range(8)
        ]
        ld_data_buf = [
            Signal(self.vlen, name=f'ld_data_buf{i}') for i in range(8)
        ]
        buf_rptr = Signal(range(8))
        buf_wptr = Signal(range(8))

        tail_gen = m.submodules.tail_gen = TailGen(self.params)
        m.d.comb += [
            tail_gen.vl.eq(self.exec_req.bits.uop.vl),
            tail_gen.uop_idx.eq(self.exec_req.bits.uop.expd_idx),
            tail_gen.eew.eq(self.exec_req.bits.uop.dest_eew()),
        ]
        tail_mask = Signal(self.vlen_bytes)
        with m.Switch(self.exec_req.bits.uop.dest_eew()):
            for i in range(4):
                with m.Case(i):
                    m.d.comb += tail_mask.eq(
                        Cat(x.replicate(1 << i) for x in tail_gen.tail))

        write_one_mask = Signal(self.vlen)
        for i in range(self.vlen_bytes):
            with m.If(tail_mask[i]):
                m.d.comb += write_one_mask[i * 8:(i + 1) * 8].eq(
                    self.exec_req.bits.uop.vta.replicate(8))
            with m.Else():
                m.d.comb += write_one_mask[i * 8:(i + 1) * 8].eq(
                    (self.exec_req.bits.uop.vm
                     | self.exec_req.bits.uop.vma).replicate(8))

        with m.If(self.exec_req.fire):
            m.d.sync += buf_wptr.eq(buf_wptr + 1)
            with m.Switch(buf_wptr):
                for i in range(8):
                    with m.Case(i):
                        m.d.sync += [
                            uop_table[i].valid.eq(1),
                            uop_table[i].bits.eq(self.exec_req.bits.uop),
                            ld_data_buf[i].eq(self.exec_req.bits.old_vd
                                              | write_one_mask),
                        ]

        wb_done = Signal()
        with m.If(wb_done):
            for i in range(8):
                m.d.sync += uop_table[i].valid.eq(0)

        #
        # Load
        #

        ld_resp_q = m.submodules.ld_resp_q = Queue(
            9, MemTransactionGenerator.LoadResponse, self.params)
        m.d.comb += [
            ld_resp_q.enq.valid.eq(mem_txn.ld_resp.valid),
            ld_resp_q.enq.bits.eq(mem_txn.ld_resp.bits),
        ]

        # Reverse data for negative strides
        s0_ld_data_valid = ld_resp_q.deq.fire
        s0_ld_data = ld_resp_q.deq.bits.data
        s0_ld_last = ld_resp_q.deq.bits.last
        ld_data_reversed = Signal.like(s0_ld_data)
        with m.If(req.stride[-1]):
            with m.Switch(req_dest_eew):
                for i in range(4):
                    with m.Case(i):
                        sew = 1 << (i + 3)
                        chunks = [
                            s0_ld_data[i:i + sew]
                            for i in range(0, len(s0_ld_data), sew)
                        ]
                        m.d.comb += ld_data_reversed.eq(Cat(chunks[::-1]))
        with m.Else():
            m.d.comb += ld_data_reversed.eq(s0_ld_data)

        # Reorganize data for stride of +/-2 and +/-4
        ld_data_reorg2 = Signal.like(s0_ld_data)
        ld_data_reorg4 = Signal.like(s0_ld_data)
        elem_off = ld_resp_q.deq.bits.elem_offset
        elem_off_reorg2 = Signal.like(elem_off)
        elem_off_reorg4 = Signal.like(elem_off)
        with m.Switch(req_dest_eew):
            for i in range(4):
                with m.Case(i):
                    sew = 1 << (i + 3)
                    chunks = [
                        ld_data_reversed[i:i + sew]
                        for i in range(0, len(ld_data_reversed), sew)
                    ]
                    m.d.comb += [
                        ld_data_reorg2.eq(
                            Cat(sum([chunks[x::2] for x in range(2)], []))),
                        ld_data_reorg4.eq(
                            Cat(sum([chunks[x::4] for x in range(4)], []))),
                        elem_off_reorg2.eq(Cat(elem_off[1:6 - i],
                                               elem_off[:1])),
                        elem_off_reorg4.eq(Cat(elem_off[2:6 - i],
                                               elem_off[:2])),
                    ]

        ld_stride_cls = StrideClassifier(self.params)
        m.submodules += ld_stride_cls
        m.d.comb += [
            ld_stride_cls.mem_size.eq(req_dest_eew),
            ld_stride_cls.stride.eq(req.stride),
        ]

        s1_ld_data_valid = Signal()
        s1_ld_data = Signal.like(s0_ld_data)
        s1_ld_elem_idx = Signal.like(ld_resp_q.deq.bits.elem_idx)
        s1_ld_elem_off = Signal.like(elem_off)
        s1_ld_elem_count = Signal.like(ld_resp_q.deq.bits.elem_count)
        s1_ld_byte_mask = Signal(self.xlen // 8)
        s1_ld_last = Signal()
        m.d.sync += s1_ld_data_valid.eq(s0_ld_data_valid)
        with m.If(s0_ld_data_valid):
            m.d.sync += [
                s1_ld_data.eq(ld_data_reversed),
                s1_ld_elem_idx.eq(ld_resp_q.deq.bits.elem_idx),
                s1_ld_elem_off.eq(elem_off),
                s1_ld_elem_count.eq(ld_resp_q.deq.bits.elem_count),
                s1_ld_last.eq(s0_ld_last),
            ]

            with m.If(ld_stride_cls.out[2]):
                m.d.sync += [
                    s1_ld_data.eq(ld_data_reorg2),
                    s1_ld_elem_off.eq(elem_off_reorg2),
                ]
            with m.Elif(ld_stride_cls.out[3]):
                m.d.sync += [
                    s1_ld_data.eq(ld_data_reorg4),
                    s1_ld_elem_off.eq(elem_off_reorg4),
                ]

        ld_elem_idx_low = Signal(range(self.vlen_bytes))
        ld_elem_idx_high = Signal(3)
        with m.Switch(req_dest_eew):
            for i in range(4):
                with m.Case(i):
                    m.d.comb += [
                        ld_elem_idx_low.eq(
                            s1_ld_elem_idx[:len(ld_elem_idx_low) - i]),
                        ld_elem_idx_high.eq(
                            s1_ld_elem_idx[len(ld_elem_idx_low) - i:]),
                    ]

        # Shift data by element index within vreg
        preshift_data = Signal(self.vlen)
        m.d.comb += preshift_data.eq(s1_ld_data)

        shin_r = preshift_data
        shin_l = preshift_data[::-1]

        shin = Signal(self.vlen)
        shamt = Signal(range(self.vlen))
        do_rshift = Signal()
        m.d.comb += shin.eq(Mux(do_rshift, shin_r, shin_l))
        with m.If(ld_elem_idx_low > s1_ld_elem_off):
            m.d.comb += shamt.eq((ld_elem_idx_low -
                                  s1_ld_elem_off) << (req_dest_eew + 3))
        with m.Else():
            m.d.comb += [
                shamt.eq((s1_ld_elem_off - ld_elem_idx_low) << (req_dest_eew +
                                                                3)),
                do_rshift.eq(1),
            ]

        shout = shin >> shamt
        ld_data_shifted = Signal(self.vlen)
        m.d.comb += ld_data_shifted.eq(Mux(do_rshift, shout, shout[::-1]))

        # Mask load data by element count
        elem_count_bytes = s1_ld_elem_count << req_dest_eew
        elem_count_mask_preshift = Signal(self.vlen_bytes)
        elem_count_mask = Signal(self.vlen_bytes)
        with m.Switch(elem_count_bytes):
            for i in range(self.vlen_bytes):
                with m.Case(i):
                    m.d.comb += elem_count_mask_preshift.eq((1 << i) - 1)
                    elem_count_shamt = Signal(range(self.vlen_bytes))
        with m.Switch(req_dest_eew):
            for i in range(4):
                with m.Case(i):
                    m.d.comb += [
                        elem_count_shamt.eq(ld_elem_idx_low << i),
                        elem_count_mask.eq(
                            elem_count_mask_preshift << elem_count_shamt),
                    ]

        ld_resp_done = s1_ld_data_valid & s1_ld_last

        # Load mask
        m.d.sync += s1_ld_byte_mask.eq(~0)
        with m.If(ld_resp_q.deq.bits.mask_valid):
            with m.Switch(req_dest_eew):
                for i in range(4):
                    with m.Case(i):
                        n = 1 << i
                        nb = n << 3
                        m.d.sync += s1_ld_byte_mask.eq(
                            Cat(
                                b.replicate(n)
                                for b in ld_resp_q.deq.bits.mask[:self.xlen //
                                                                 nb]))

        s1_ld_mask = Signal.like(elem_count_mask)
        m.d.comb += s1_ld_mask.eq(s1_ld_byte_mask << elem_count_shamt)

        # Write load data buffer
        ld_write_mask = elem_count_mask & s1_ld_mask
        ld_write_bit_mask = Signal(self.vlen)
        m.d.comb += ld_write_bit_mask.eq(
            Cat(b.replicate(8) for b in ld_write_mask))
        with m.If(s1_ld_data_valid):
            with m.Switch(ld_elem_idx_high):
                for i in range(8):
                    with m.Case(i):
                        m.d.sync += ld_data_buf[i].eq(
                            (ld_data_shifted & ld_write_bit_mask)
                            | (ld_data_buf[i] & ~ld_write_bit_mask))

        idx_buf = Array(
            Signal(self.vlen, name=f'idx_buf{i}') for i in range(8))
        with m.If(self.exec_req.fire):
            m.d.sync += idx_buf[buf_wptr].eq(self.exec_req.bits.index)

        idx_offset = Signal(self.vl_bits)
        with m.If(mem_txn.idx_data.fire):
            m.d.sync += idx_offset.eq(idx_offset + 1)
            with m.If(idx_offset == req_vl - 1):
                m.d.sync += mem_txn.idx_data.valid.eq(0)

        m.d.comb += mem_txn.idx_data.bits.last.eq(idx_offset == req_vl - 1)
        with m.Switch(req_eew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (w + 3)

                    with m.Switch(idx_offset):
                        for i in range(self.vlen * 8 // n):
                            with m.Case(i):
                                m.d.comb += mem_txn.idx_data.bits.index.eq(
                                    Cat(idx_buf)[i * n:(i + 1) * n])

        with m.Switch(idx_offset):
            for i in range(self.vlen):
                with m.Case(i):
                    m.d.comb += mem_txn.idx_data.bits.mask.eq(req.mask[i]
                                                              | req.uop.vm)

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += [
                    mem_txn.req.valid.eq(self.exec_req.valid),
                    self.exec_req.ready.eq(mem_txn.req.ready),
                ]

                with m.If(self.exec_req.fire):
                    m.d.sync += req.eq(self.exec_req.bits)

                    with m.If(self.exec_req.bits.uop.expd_end):
                        with m.If(self.exec_req.bits.uop.is_ld):
                            m.next = 'LOAD'
                        with m.Else():
                            m.next = 'STORE'
                    with m.Else():
                        m.next = 'WAIT_FOR_UOP'

            with m.State('WAIT_FOR_UOP'):
                m.d.comb += self.exec_req.ready.eq(1)

                with m.If(self.exec_req.fire
                          & self.exec_req.bits.uop.expd_end):
                    with m.If(self.exec_req.bits.uop.is_ld):
                        with m.If(req.uop.indexed):
                            m.d.sync += [
                                mem_txn.idx_data.valid.eq(1),
                                idx_offset.eq(0),
                            ]

                        m.next = 'LOAD'
                    with m.Else():
                        m.next = 'STORE'

            with m.State('LOAD'):
                m.d.comb += ld_resp_q.deq.ready.eq(~ld_resp_done)

                with m.If(ld_resp_done):
                    m.next = 'WRITEBACK'

            with m.State('STORE'):
                with m.If(mem_txn.done):
                    m.next = 'WRITEBACK'

            with m.State('WRITEBACK'):
                with m.Switch(buf_rptr):
                    for i in range(7):
                        with m.Case(i):
                            m.d.comb += wb_done.eq(~uop_table[i + 1].valid)
                    with m.Case(7):
                        m.d.comb += wb_done.eq(1)

                with m.Switch(buf_rptr):
                    for i in range(7):
                        with m.Case(i):
                            m.d.comb += [
                                self.exec_resp.valid.eq(uop_table[i].valid),
                                self.exec_resp.bits.vd_data.eq(ld_data_buf[i]),
                                self.exec_resp.bits.uop.eq(uop_table[i].bits),
                            ]

                with m.If(self.exec_resp.fire):
                    m.d.sync += [
                        buf_rptr.eq(Mux(wb_done, 0, buf_rptr + 1)),
                        buf_wptr.eq(0),
                    ]
                    with m.If(wb_done):
                        m.next = 'IDLE'

        return m
