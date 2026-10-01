from amaranth import *
from amaranth import tracer
from amaranth.utils import log2_int
from amaranth.hdl.ast import ValueCastable

from groom.lsu import DMA_ID_WIDTH, SharedMemoryDMACommit, SharedMemoryDMAReq
from room.types import HasCoreParams
from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Decoupled, Queue


class AsyncCopyCmd(HasCoreParams, ValueCastable):

    def __init__(self, params, core_id_width=1, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.id = Signal(DMA_ID_WIDTH, name=f'{name}_id')
        self.core = Signal(core_id_width, name=f'{name}_core')
        self.src_addr = Signal(32, name=f'{name}_src_addr')
        self.nbytes = Signal(16, name=f'{name}_nbytes')
        # Full operand width: the destination must be bounds-checked before
        # any truncation, or an offset like 0x8000 against 16 KiB of shared
        # memory would silently wrap to a valid unintended destination.
        self.dst_offset = Signal(32, name=f'{name}_dst_offset')
        self.mode = Signal(name=f'{name}_mode')
        self.src_base = Signal(32, name=f'{name}_src_base')
        self.row_count = Signal(16, name=f'{name}_row_count')
        self.row_bytes = Signal(16, name=f'{name}_row_bytes')
        self.g_stride = Signal(16, name=f'{name}_g_stride')
        self.s_stride = Signal(16, name=f'{name}_s_stride')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.id, self.core, self.src_addr, self.nbytes,
                   self.dst_offset, self.mode, self.src_base, self.row_count,
                   self.row_bytes, self.g_stride, self.s_stride)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class AsyncCopyDone(HasCoreParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.id = Signal(DMA_ID_WIDTH, name=f'{name}_id')
        self.error = Signal(name=f'{name}_error')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.id, self.error)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class AsyncCopyEngine(HasCoreParams, Elaboratable):

    def __init__(self, n_cores, params, block_bytes=64, n_slots=8):
        super().__init__(params)

        self.n_cores = n_cores
        self.core_bits = max(1, Shape.cast(range(n_cores)).width)
        self.block_bytes = block_bytes
        self.beat_bytes = 8
        self.beats_per_line = block_bytes // self.beat_bytes
        self.n_slots = n_slots

        if block_bytes < self.beat_bytes:
            raise ValueError(
                f'block_bytes must cover at least one {self.beat_bytes}-byte '
                f'beat, got {block_bytes}')
        if (block_bytes & (block_bytes - 1)) != 0:
            raise ValueError(
                f'block_bytes must be a power of two, got {block_bytes}')
        if block_bytes > (1 << 7):
            raise ValueError(f'block_bytes must be at most {1 << 7} bytes, '
                             f'got {block_bytes}')
        if (n_slots & (n_slots - 1)) != 0:
            raise ValueError(f'n_slots must be a power of two, got {n_slots}')

        self.line_bits = log2_int(block_bytes)
        self.slot_bits = log2_int(n_slots)

        self.mem_bus = tl.Interface(data_width=64,
                                    addr_width=32,
                                    size_width=3,
                                    source_id_width=self.slot_bits)

        self.cmd = Decoupled(AsyncCopyCmd,
                             params,
                             core_id_width=self.core_bits)

        self.done = Decoupled(AsyncCopyDone, params)

        self.dma_req = [
            Decoupled(SharedMemoryDMAReq, self.params, name=f'dma_req{i}')
            for i in range(n_cores)
        ]
        self.dma_commit = [
            Decoupled(SharedMemoryDMACommit,
                      self.params,
                      name=f'dma_commit{i}') for i in range(n_cores)
        ]

        self.busy = Signal()
        self.error = Signal()

    def elaborate(self, platform):
        m = Module()

        beats = self.beats_per_line
        beat_shift = log2_int(self.beat_bytes)
        # Fragment offsets presented to the shared-memory endpoint are
        # aperture width; command/queue/transfer destinations keep the
        # full 32-bit operand until launch validation has bounded them.
        dst_bits = log2_int(self.smem_size) + 1
        dst_full = 32
        cmd_queue_depth = 8

        state_invalid = 0
        state_recv = 1
        state_drain = 2

        #
        # Command queue
        #

        q_id = [
            Signal(DMA_ID_WIDTH, name=f'cmd_q_id{i}')
            for i in range(cmd_queue_depth)
        ]
        q_core = [
            Signal(self.core_bits, name=f'cmd_q_core{i}')
            for i in range(cmd_queue_depth)
        ]
        q_src = [
            Signal(32, name=f'cmd_q_src{i}') for i in range(cmd_queue_depth)
        ]
        q_nbytes = [
            Signal(16, name=f'cmd_q_nbytes{i}') for i in range(cmd_queue_depth)
        ]
        q_dst = [
            Signal(dst_full, name=f'cmd_q_dst{i}')
            for i in range(cmd_queue_depth)
        ]
        q_mode = [
            Signal(name=f'cmd_q_mode{i}') for i in range(cmd_queue_depth)
        ]
        q_base = [
            Signal(32, name=f'cmd_q_base{i}') for i in range(cmd_queue_depth)
        ]
        q_rcnt = [
            Signal(16, name=f'cmd_q_rcnt{i}') for i in range(cmd_queue_depth)
        ]
        q_rbytes = [
            Signal(16, name=f'cmd_q_rbytes{i}') for i in range(cmd_queue_depth)
        ]
        q_gs = [
            Signal(16, name=f'cmd_q_gs{i}') for i in range(cmd_queue_depth)
        ]
        q_ss = [
            Signal(16, name=f'cmd_q_ss{i}') for i in range(cmd_queue_depth)
        ]
        q_head = Signal(range(cmd_queue_depth))
        q_tail = Signal(range(cmd_queue_depth))
        q_count = Signal(range(cmd_queue_depth + 1))

        q_id_arr = Array(q_id)
        q_core_arr = Array(q_core)
        q_src_arr = Array(q_src)
        q_nbytes_arr = Array(q_nbytes)
        q_dst_arr = Array(q_dst)
        q_mode_arr = Array(q_mode)
        q_base_arr = Array(q_base)
        q_rcnt_arr = Array(q_rcnt)
        q_rbytes_arr = Array(q_rbytes)
        q_gs_arr = Array(q_gs)
        q_ss_arr = Array(q_ss)

        m.d.comb += self.cmd.ready.eq(q_count != cmd_queue_depth)
        with m.If(self.cmd.fire):
            with m.Switch(q_tail):
                for i in range(cmd_queue_depth):
                    with m.Case(i):
                        m.d.sync += [
                            q_id[i].eq(self.cmd.bits.id),
                            q_core[i].eq(self.cmd.bits.core),
                            q_src[i].eq(self.cmd.bits.src_addr),
                            q_nbytes[i].eq(self.cmd.bits.nbytes),
                            q_dst[i].eq(self.cmd.bits.dst_offset),
                            q_mode[i].eq(self.cmd.bits.mode),
                            q_base[i].eq(self.cmd.bits.src_base),
                            q_rcnt[i].eq(self.cmd.bits.row_count),
                            q_rbytes[i].eq(self.cmd.bits.row_bytes),
                            q_gs[i].eq(self.cmd.bits.g_stride),
                            q_ss[i].eq(self.cmd.bits.s_stride),
                        ]
            m.d.sync += q_tail.eq((q_tail + 1) % cmd_queue_depth)

        cmd_pop = Signal()
        with m.If(cmd_pop):
            m.d.sync += q_head.eq((q_head + 1) % cmd_queue_depth)
        m.d.sync += q_count.eq(q_count + self.cmd.fire - cmd_pop)

        #
        # Transfer context
        #
        # Every transfer walks `row_count` linear rows of `row_bytes` bytes;
        # a 1D command is the single-row case with zero strides. Row r reads
        # [src0 + r*g_stride, +row_bytes) into [dst + r*s_stride, +row_bytes)
        # where src0 = src_base + src_addr for 2D and src_addr for 1D.

        xfer_valid = Signal()
        xfer_id = Signal(DMA_ID_WIDTH)
        xfer_core = Signal(self.core_bits)
        xfer_failed = Signal()
        xfer_src = Signal(32)
        xfer_end = Signal(33)
        xfer_dst = Signal(dst_full)
        xfer_line = Signal(32)
        xfer_last = Signal(32)
        xfer_row = Signal(16)
        xfer_rows = Signal(16)
        xfer_rbytes = Signal(16)
        xfer_gs = Signal(16)
        xfer_ss = Signal(16)
        addr_done = Signal()
        lines_out = Signal(range(self.n_slots + 1))
        frags_out = Signal(range(1 << 14))

        head_mode2 = q_mode_arr[q_head] != 0
        head_rows = Mux(head_mode2, q_rcnt_arr[q_head], 1)
        head_rbytes = Mux(head_mode2, q_rbytes_arr[q_head],
                          q_nbytes_arr[q_head])
        head_gs = Mux(head_mode2, q_gs_arr[q_head], 0)
        head_ss = Mux(head_mode2, q_ss_arr[q_head], 0)
        head_src0 = Mux(head_mode2, q_base_arr[q_head] + q_src_arr[q_head],
                        q_src_arr[q_head])
        head_row_end = head_src0 + head_rbytes
        head_row_last = Cat(Const(0, self.line_bits),
                            (head_row_end - 1)[self.line_bits:])

        # The whole tensor span is validated before any traffic: rows are
        # monotonic under unsigned strides, so the covering range bounds
        # every row. The destination walk is checked the same way.
        head_rowsm1 = Mux(head_rows == 0, 0, head_rows - 1)
        head_g_off = head_rowsm1 * head_gs
        head_s_off = head_rowsm1 * head_ss
        head_total_end = head_src0 + head_g_off + head_rbytes
        head_dst_end = q_dst_arr[q_head] + head_s_off + head_rbytes
        head_last = Cat(Const(0, self.line_bits),
                        (head_total_end - 1)[self.line_bits:])

        head_nonempty = (head_rows != 0) & (head_rbytes != 0)
        read_start = Cat(Const(0, self.line_bits), head_src0[self.line_bits:])
        read_end = head_last + self.block_bytes
        cmd_overflow = head_total_end > (1 << 32)
        cmd_mmio = Const(0)
        for base, size in self.io_regions.items():
            cmd_mmio |= ((read_start < base + size) & (read_end > base))

        # Full-line overfetch is permitted only inside a declared readable,
        # cacheable PMA region. Reject unknown maps and unsupported region
        # boundaries before any read or destination write. PMAs are static
        # elaboration parameters, so queued commands see the acceptance map.
        source_allowed = Const(0)
        source_forbidden = Const(0)
        for base, size, mode, cacheable in self.pma_regions or []:
            if cacheable and 'r' in mode:
                source_allowed |= ((read_start >= base)
                                   & (read_end <= base + size))
            else:
                source_forbidden |= ((read_start < base + size)
                                     & (read_end > base))
        cmd_reject = ((q_core_arr[q_head] >= self.n_cores)
                      | (head_dst_end > self.smem_size)
                      | (head_nonempty & (cmd_overflow | cmd_mmio
                                          | ~source_allowed
                                          | source_forbidden)))

        start = ~xfer_valid & ~self.done.valid & (q_count != 0)
        m.d.comb += cmd_pop.eq(start)

        with m.If(start):
            with m.If(cmd_reject):
                m.d.sync += [
                    self.done.valid.eq(1),
                    self.done.bits.id.eq(q_id_arr[q_head]),
                    self.done.bits.error.eq(1),
                    self.error.eq(1),
                ]
            with m.Else():
                m.d.sync += [
                    xfer_valid.eq(1),
                    xfer_id.eq(q_id_arr[q_head]),
                    xfer_core.eq(q_core_arr[q_head]),
                    xfer_src.eq(head_src0),
                    xfer_end.eq(head_row_end),
                    xfer_dst.eq(q_dst_arr[q_head]),
                    xfer_line.eq(read_start),
                    xfer_last.eq(head_row_last[:32]),
                    xfer_row.eq(0),
                    xfer_rows.eq(head_rows),
                    xfer_rbytes.eq(head_rbytes),
                    xfer_gs.eq(head_gs),
                    xfer_ss.eq(head_ss),
                    xfer_failed.eq(0),
                    addr_done.eq(~head_nonempty),
                    lines_out.eq(0),
                    frags_out.eq(0),
                ]

        #
        # Read slots
        #

        slot_state = [
            Signal(2, name=f'slot_state{i}') for i in range(self.n_slots)
        ]
        slot_line = [
            Signal(32, name=f'slot_line{i}') for i in range(self.n_slots)
        ]
        slot_bad = [Signal(name=f'slot_bad{i}') for i in range(self.n_slots)]
        slot_beats = [
            Signal(range(beats + 1), name=f'slot_beats{i}')
            for i in range(self.n_slots)
        ]
        slot_drain = [
            Signal(range(beats), name=f'slot_drain{i}')
            for i in range(self.n_slots)
        ]
        slot_data = [
            Signal(64 * beats, name=f'slot_data{i}')
            for i in range(self.n_slots)
        ]
        slot_mask = [
            Signal(8 * beats, name=f'slot_mask{i}')
            for i in range(self.n_slots)
        ]

        state_arr = Array(slot_state)
        line_arr = Array(slot_line)
        drain_arr = Array(slot_drain)
        mask_arr = Array(slot_mask)
        data_arr = Array(slot_data)

        #
        # Address generation and Get issue
        #

        issue_slot = Signal(self.slot_bits)
        issue_found = Signal()
        issue_locked = Signal()
        issue_held_slot = Signal(self.slot_bits)
        free_slot = Const(0, 1)
        free_slot_idx = Const(0, self.slot_bits)
        for i in range(self.n_slots):
            take = (slot_state[i] == state_invalid) & ~free_slot
            free_slot_idx = Mux(take, i, free_slot_idx)
            free_slot = free_slot | take
        m.d.comb += [
            issue_found.eq(free_slot),
            issue_slot.eq(free_slot_idx),
        ]
        with m.If(issue_locked):
            m.d.comb += issue_slot.eq(issue_held_slot)

        # A presented Get is irrevocable, including when another outstanding
        # response fails. Finish that offer, then drain it with the others.
        can_issue = issue_locked | (xfer_valid & ~addr_done & ~xfer_failed
                                    & issue_found)

        line_masks = []
        for b in range(beats):
            mask_b = Const(0, 8)
            for k in range(8):
                byte_addr = xfer_line + ((b << beat_shift) + k)
                in_range = (byte_addr >= xfer_src) & (byte_addr < xfer_end)
                mask_b = mask_b | (in_range << k)
            line_masks.append(mask_b)

        m.d.comb += [
            self.mem_bus.a.valid.eq(can_issue),
            self.mem_bus.a.bits.opcode.eq(tl.ChannelAOpcode.Get),
            self.mem_bus.a.bits.param.eq(0),
            self.mem_bus.a.bits.size.eq(self.line_bits),
            self.mem_bus.a.bits.source.eq(issue_slot),
            self.mem_bus.a.bits.address.eq(xfer_line),
            self.mem_bus.a.bits.mask.eq(~0),
            self.mem_bus.a.bits.corrupt.eq(0),
        ]

        a_fire = self.mem_bus.a.fire

        with m.If(can_issue & ~self.mem_bus.a.ready):
            m.d.sync += [issue_locked.eq(1), issue_held_slot.eq(issue_slot)]
        with m.If(a_fire):
            m.d.sync += issue_locked.eq(0)

        with m.If(a_fire):
            m.d.sync += [
                xfer_line.eq(xfer_line + self.block_bytes),
                addr_done.eq(xfer_line == xfer_last),
            ]
            for i in range(self.n_slots):
                with m.If(issue_slot == i):
                    m.d.sync += [
                        slot_state[i].eq(state_recv),
                        slot_line[i].eq(xfer_line),
                        slot_bad[i].eq(0),
                        slot_beats[i].eq(0),
                        slot_drain[i].eq(0),
                        slot_mask[i].eq(Cat(*line_masks)),
                    ]

        #
        # Response reception
        #

        d_slot = self.mem_bus.d.bits.source
        m.d.comb += self.mem_bus.d.ready.eq(state_arr[d_slot] == state_recv)

        d_fire = self.mem_bus.d.fire
        d_bad = self.mem_bus.d.bits.denied | self.mem_bus.d.bits.corrupt

        err_release_terms = []
        for i in range(self.n_slots):
            beat_done = d_fire & (d_slot == i) & (slot_beats[i] == beats - 1)
            with m.If(d_fire & (d_slot == i)):
                m.d.sync += [
                    slot_data[i].word_select(slot_beats[i],
                                             64).eq(self.mem_bus.d.bits.data),
                    slot_beats[i].eq(slot_beats[i] + 1),
                ]
                with m.If(d_bad):
                    m.d.sync += [
                        slot_bad[i].eq(1),
                        xfer_failed.eq(1),
                        self.error.eq(1),
                    ]
                with m.If(beat_done):
                    with m.If(slot_bad[i] | d_bad):
                        m.d.sync += slot_state[i].eq(state_invalid)
                        err_release_terms.append(beat_done
                                                 & (slot_bad[i] | d_bad))
                    with m.Else():
                        m.d.sync += slot_state[i].eq(state_drain)

        err_release = Cat(*err_release_terms).any() if err_release_terms \
            else Const(0)

        #
        # Destination drain
        #

        drain_rr = Signal(self.slot_bits)
        drain_pick = Signal(self.slot_bits)
        drain_pick_valid = Signal()
        drain_locked = Signal()
        drain_slot = Signal(self.slot_bits)

        m.d.comb += [
            drain_pick.eq(drain_rr),
            drain_pick_valid.eq(state_arr[drain_rr] == state_drain),
        ]
        for j in reversed(range(1, self.n_slots)):
            idx = (drain_rr + j)[:self.slot_bits]
            with m.If(state_arr[idx] == state_drain):
                m.d.comb += [
                    drain_pick.eq(idx),
                    drain_pick_valid.eq(1),
                ]

        # A newly returned line must not replace an already offered fragment.
        # The selected slot and beat remain live until the endpoint accepts it.
        with m.If(drain_locked):
            m.d.comb += [drain_pick.eq(drain_slot), drain_pick_valid.eq(1)]

        drain_pick_line = line_arr[drain_pick]
        drain_pick_beat = drain_arr[drain_pick]
        raw_mask = mask_arr[drain_pick].word_select(drain_pick_beat, 8)
        beat_data = data_arr[drain_pick].word_select(drain_pick_beat, 64)
        beat_addr = drain_pick_line | (drain_pick_beat << beat_shift)
        down = Mux(xfer_src > beat_addr, (xfer_src - beat_addr)[:3], 0)
        frag_data = beat_data >> (8 * down)
        frag_mask = raw_mask >> down
        dst_delta = beat_addr + down - xfer_src
        frag_offset = (xfer_dst + dst_delta)[:dst_bits]

        frag_empty = drain_pick_valid & (raw_mask == 0)

        offer_valid = drain_pick_valid & (raw_mask != 0)
        for c in range(self.n_cores):
            with m.Switch(xfer_core):
                with m.Case(c):
                    m.d.comb += [
                        self.dma_req[c].valid.eq(offer_valid),
                        self.dma_req[c].bits.id.eq(xfer_id),
                        self.dma_req[c].bits.offset.eq(frag_offset),
                        self.dma_req[c].bits.data.eq(frag_data),
                        self.dma_req[c].bits.byte_enable.eq(frag_mask),
                    ]

        offer_fire = offer_valid & Cat(*[port.fire
                                         for port in self.dma_req]).any()

        with m.If(offer_valid & ~offer_fire):
            m.d.sync += [drain_locked.eq(1), drain_slot.eq(drain_pick)]
        with m.If(offer_fire):
            m.d.sync += drain_locked.eq(0)

        drain_advance = frag_empty | offer_fire

        drain_release_terms = []
        for p in range(self.n_slots):
            slot_finish = drain_advance & (drain_pick == p) & (drain_pick_beat
                                                               == beats - 1)
            with m.If(drain_advance & (drain_pick == p)):
                with m.If(drain_pick_beat == beats - 1):
                    m.d.sync += [
                        slot_state[p].eq(state_invalid),
                        drain_rr.eq(drain_pick + 1),
                    ]
                    drain_release_terms.append(slot_finish)
                with m.Else():
                    m.d.sync += slot_drain[p].eq(drain_pick_beat + 1)

        drain_release = Cat(*drain_release_terms).any() \
            if drain_release_terms else Const(0)

        commit_fire = Cat(*[port.fire for port in self.dma_commit]).any()
        for port in self.dma_commit:
            m.d.comb += port.ready.eq(1)

        m.d.sync += [
            lines_out.eq(lines_out + (can_issue & a_fire) - err_release -
                         drain_release),
            frags_out.eq(frags_out + offer_fire - commit_fire),
        ]

        #
        # Completion
        #

        # A failed transfer stops issuing further Gets, so addr_done may
        # never be reached; completion must also fire once the failure has
        # been latched and all outstanding traffic has drained. A failure
        # ends the whole transfer rather than advancing to later rows.
        row_done = (xfer_valid & (addr_done | xfer_failed)
                    & (lines_out == 0) & (frags_out == 0) & ~issue_locked)
        xfer_last_row = (xfer_row + 1 >= xfer_rows) | xfer_failed
        xfer_complete = row_done & ~self.done.valid
        with m.If(xfer_complete):
            with m.If(xfer_last_row):
                m.d.sync += [
                    xfer_valid.eq(0),
                    self.done.valid.eq(1),
                    self.done.bits.id.eq(xfer_id),
                    self.done.bits.error.eq(xfer_failed),
                ]
            with m.Else():
                row_src = xfer_src + xfer_gs
                row_end = row_src + xfer_rbytes
                m.d.sync += [
                    xfer_row.eq(xfer_row + 1),
                    xfer_src.eq(row_src),
                    xfer_end.eq(row_end),
                    xfer_dst.eq((xfer_dst + xfer_ss)[:dst_full]),
                    xfer_line.eq(
                        Cat(Const(0, self.line_bits),
                            row_src[self.line_bits:])),
                    xfer_last.eq(
                        Cat(Const(0, self.line_bits),
                            (row_end - 1)[self.line_bits:])[:32]),
                    addr_done.eq(xfer_rbytes == 0),
                ]

        with m.If(self.done.fire):
            m.d.sync += self.done.valid.eq(0)

        m.d.comb += self.busy.eq((q_count != 0) | xfer_valid
                                 | self.done.valid
                                 | Cat(*(s != state_invalid
                                         for s in slot_state)).any())

        return m


class AsyncCopyLaunch(HasCoreParams, ValueCastable):
    """A copy command together with its completion-token identity."""

    def __init__(self, params, core_id_width=1, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.cmd = AsyncCopyCmd(params, core_id_width, name=f'{name}_cmd')
        self.gen = Signal(name=f'{name}_gen')
        self.wid = Signal(range(self.n_warps), name=f'{name}_wid')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.cmd, self.gen, self.wid)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class AsyncCopyWait(HasCoreParams, ValueCastable):
    """A completion-token query from one warp of one core.

    ``query`` distinguishes a parking wait from a non-blocking status read.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.wid = Signal(range(self.n_warps), name=f'{name}_wid')
        self.id = Signal(DMA_ID_WIDTH, name=f'{name}_id')
        self.gen = Signal(name=f'{name}_gen')
        self.query = Signal(name=f'{name}_query')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.wid, self.id, self.gen, self.query)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class AsyncCopyCompletion(HasCoreParams, Elaboratable):
    """Cluster-side completion tokens for asynchronous copies.

    Consumes the engine's ``done`` events, arbitrates per-core launch and
    wait requests, and releases parked warps. Tokens are software
    allocated: a launch must name a FREE slot with the expected
    generation, and a wait must match the same generation to observe the
    transfer. Every ``done`` event is also mirrored to ``done_out`` for
    external observation; events for ids with no live token pass through
    untouched.

    A wait on a FREE token is answered stale rather than parked: only a
    FLIGHT token may park a waiter. Each token retains the result of its
    own most recently consumed transfer, so a ``gcopystat`` (or an
    idempotent repeat wait) with a spent generation still reports that
    transfer's ``{error, done}`` instead of a bare stale, unaffected by
    other tokens' completions. Instruction launches are the only command
    source; done events for ids with no live token (including every id
    while the table is idle) simply pass through the mirror.
    """

    def __init__(self, n_cores, params, core_id_width=1, n_tokens=8):
        super().__init__(params)

        self.n_cores = n_cores
        self.core_bits = max(1, Shape.cast(range(n_cores)).width)
        self.n_tokens = n_tokens

        if not 1 <= n_tokens <= (1 << DMA_ID_WIDTH):
            raise ValueError(
                f'n_tokens must be between 1 and {1 << DMA_ID_WIDTH}, '
                f'got {n_tokens}')

        self.launch = [
            Decoupled(AsyncCopyLaunch,
                      params,
                      core_id_width=self.core_bits,
                      name=f'copy_launch{i}') for i in range(n_cores)
        ]
        self.wait = [
            Decoupled(AsyncCopyWait, params, name=f'copy_wait{i}')
            for i in range(n_cores)
        ]
        self.cmd = Decoupled(AsyncCopyCmd,
                             params,
                             core_id_width=self.core_bits)
        self.done = Decoupled(AsyncCopyDone, params)
        self.done_out = Decoupled(AsyncCopyDone, params)

        self.ack_valid = [
            Signal(name=f'copy_ack_valid{i}') for i in range(n_cores)
        ]
        self.ack_wid = [
            Signal(range(self.n_warps), name=f'copy_ack_wid{i}')
            for i in range(n_cores)
        ]
        self.ack_reject = [
            Signal(name=f'copy_ack_reject{i}') for i in range(n_cores)
        ]
        self.wake_valid = [
            Signal(name=f'copy_wake_valid{i}') for i in range(n_cores)
        ]
        self.wake_wid = [
            Signal(range(self.n_warps), name=f'copy_wake_wid{i}')
            for i in range(n_cores)
        ]
        self.wake_stale = [
            Signal(name=f'copy_wake_stale{i}') for i in range(n_cores)
        ]
        self.wake_error = [
            Signal(name=f'copy_wake_error{i}') for i in range(n_cores)
        ]
        self.wake_done = [
            Signal(name=f'copy_wake_done{i}') for i in range(n_cores)
        ]

        self.busy = Signal()

    def elaborate(self, platform):
        m = Module()

        n_cores = self.n_cores
        core_bits = self.core_bits

        #
        # Token table
        #

        # State predicates are driven by the independent token FSMs below.
        tk_free = [Signal(name=f'tk_free{i}') for i in range(self.n_tokens)]
        tk_flight = [
            Signal(name=f'tk_flight{i}') for i in range(self.n_tokens)
        ]
        tk_done = [Signal(name=f'tk_done{i}') for i in range(self.n_tokens)]
        tk_error = [Signal(name=f'tk_error{i}') for i in range(self.n_tokens)]
        tk_wait_consume = [
            Signal(name=f'tk_wait_consume{i}') for i in range(self.n_tokens)
        ]
        tk_gen = [Signal(name=f'tk_gen{i}') for i in range(self.n_tokens)]
        tk_waiter = [
            Signal(name=f'tk_waiter{i}') for i in range(self.n_tokens)
        ]
        tk_wcore = [
            Signal(core_bits, name=f'tk_wcore{i}')
            for i in range(self.n_tokens)
        ]
        tk_wwid = [
            Signal(range(self.n_warps), name=f'tk_wwid{i}')
            for i in range(self.n_tokens)
        ]
        # Retained result per token: the outcome of the token's most
        # recently consumed transfer. Wait and stat with a spent
        # generation report it instead of a bare stale, so the normal
        # launch/compute/wait sequence can still read the transfer's
        # error status after the wait consumed the token. Retention is
        # per token so another token's consumption cannot overwrite it.
        tk_res_valid = [
            Signal(name=f'tk_res_valid{i}') for i in range(self.n_tokens)
        ]
        tk_res_gen = [
            Signal(name=f'tk_res_gen{i}') for i in range(self.n_tokens)
        ]
        tk_res_error = [
            Signal(name=f'tk_res_error{i}') for i in range(self.n_tokens)
        ]

        #
        # Done-event mirror
        #

        done_q = m.submodules.done_q = Queue(2,
                                             AsyncCopyDone,
                                             self.params,
                                             flow=True)
        m.d.comb += [
            self.done.connect(done_q.enq),
            done_q.deq.connect(self.done_out),
        ]

        done_fire = self.done.fire
        done_id = self.done.bits.id
        done_error = self.done.bits.error

        # A done event with a registered waiter releases the waiter and
        # consumes the token; without one the token stays queryable until
        # a matching wait consumes it, and events for ids with no live
        # token pass through untouched.
        done_has_waiter = [(done_fire & (done_id == i)
                            & tk_flight[i] & tk_waiter[i])
                           for i in range(self.n_tokens)]
        done_wake = Cat(*done_has_waiter).any()
        done_wake_core = Signal(core_bits)
        done_wake_wid = Signal(range(self.n_warps))
        done_wake_error = Signal()
        for i in range(self.n_tokens):
            with m.If(done_has_waiter[i]):
                m.d.comb += [
                    done_wake_core.eq(tk_wcore[i]),
                    done_wake_wid.eq(tk_wwid[i]),
                    done_wake_error.eq(done_error),
                ]

        #
        # Wait and status requests. At most one is served per cycle, and
        # it is deferred when a done-driven wake aims at the same core so
        # each core observes at most one wake per cycle.
        #

        wait_idx_bits = max(1, Shape.cast(range(n_cores)).width)
        wait_rr = Signal(wait_idx_bits)
        wait_pick = Signal(wait_idx_bits)
        wait_pick_valid = Signal()
        wait_arr = Array(self.wait)
        wait_blocked_arr = Array(
            [Signal(name=f'wait_blocked{i}') for i in range(n_cores)])

        for i in range(n_cores):
            m.d.comb += wait_blocked_arr[i].eq(done_wake
                                               & (done_wake_core == i))

        m.d.comb += [
            wait_pick.eq(wait_rr),
            wait_pick_valid.eq(wait_arr[wait_rr].valid
                               & ~wait_blocked_arr[wait_rr]),
        ]
        for j in reversed(range(1, n_cores)):
            idx = (wait_rr + j) % n_cores
            with m.If(wait_arr[idx].valid & ~wait_blocked_arr[idx]):
                m.d.comb += [
                    wait_pick.eq(idx),
                    wait_pick_valid.eq(1),
                ]

        for i in range(n_cores):
            m.d.comb += self.wait[i].ready.eq(wait_pick_valid
                                              & (wait_pick == i))

        wait_sel = wait_arr[wait_pick]
        wait_fire = wait_sel.fire
        wait_id = wait_sel.bits.id
        wait_gen = wait_sel.bits.gen
        wait_query = wait_sel.bits.query
        wait_wid = wait_sel.bits.wid
        wait_core = Signal(wait_idx_bits)
        m.d.comb += wait_core.eq(wait_pick)

        # A wait's answer must account for a done event landing on the
        # same token in the same cycle: the effective predicates and
        # generation describe the post-done state. Consumption takes
        # priority over completion in each token FSM. A request matching
        # no token consults the retained result before being answered
        # stale, so a spent handle still reports its transfer's outcome.
        ans_valid = Signal()
        ans_stale = Signal()
        ans_error = Signal()
        ans_done = Signal()
        m.d.comb += [
            ans_valid.eq(0),
            ans_stale.eq(0),
            ans_error.eq(0),
            ans_done.eq(0),
        ]

        any_matched = Signal()
        for i in range(self.n_tokens):
            completing = done_fire & (done_id == i) & tk_flight[i]
            gen_eff = Mux(done_has_waiter[i], ~tk_gen[i], tk_gen[i])
            free_eff = tk_free[i] | done_has_waiter[i]
            flight_eff = tk_flight[i] & ~completing
            done_eff = tk_done[i] | (completing & ~tk_waiter[i])
            error_eff = Mux(completing, done_error, tk_error[i])

            matched = wait_fire & (wait_id == i) & (gen_eff == wait_gen)
            with m.If(matched):
                m.d.comb += any_matched.eq(1)
            with m.If(matched & flight_eff):
                # In flight: park unless querying or another warp already
                # waits.
                with m.If(wait_query):
                    m.d.comb += [
                        ans_valid.eq(1),
                        ans_stale.eq(0),
                    ]
                with m.Elif(tk_waiter[i]):
                    m.d.comb += [
                        ans_valid.eq(1),
                        ans_stale.eq(1),
                    ]
                with m.Else():
                    m.d.sync += [
                        tk_waiter[i].eq(1),
                        tk_wcore[i].eq(wait_core),
                        tk_wwid[i].eq(wait_wid),
                    ]

            with m.If(matched & free_eff):
                # A never-launched (or spent and re-toggled) generation
                # matching a FREE token must answer stale; parking here
                # would sleep forever, since no transfer can complete it.
                m.d.comb += [
                    ans_valid.eq(1),
                    ans_stale.eq(1),
                ]

            with m.If(matched & done_eff):
                m.d.comb += [
                    ans_valid.eq(1),
                    ans_error.eq(error_eff),
                    ans_done.eq(1),
                    tk_wait_consume[i].eq(~wait_query),
                ]

        with m.If(wait_fire):
            m.d.sync += wait_rr.eq((wait_rr + 1) % n_cores)

        # An unmatched token (unknown id, or a generation that matches no
        # live or completed token) consults that token's retained result
        # before being refused: a spent handle still reports its
        # transfer's outcome, and anything else is answered stale rather
        # than leaving the requester unanswered. Retention is keyed by
        # the exact spent {id, generation}, so another token's
        # consumption cannot substitute its own result here.
        with m.If(wait_fire & ~any_matched):
            latch_hit = Signal()
            latch_error = Signal()
            m.d.comb += [
                latch_hit.eq(0),
                latch_error.eq(0),
            ]
            for i in range(self.n_tokens):
                with m.If(tk_res_valid[i] & (wait_id == i)
                          & (wait_gen == tk_res_gen[i])):
                    m.d.comb += [
                        latch_hit.eq(1),
                        latch_error.eq(tk_res_error[i]),
                    ]
                # A consumption by wake landing in this request's own
                # fire cycle has not reached the retained registers yet
                # (they update at the edge), and the live match saw the
                # post-consumption state. Forward the completing
                # transfer's result so a valid handle queried from
                # another core never reads stale during completion.
                with m.If(done_has_waiter[i] & (wait_id == i)
                          & (wait_gen == tk_gen[i])):
                    m.d.comb += [
                        latch_hit.eq(1),
                        latch_error.eq(done_error),
                    ]
            with m.If(latch_hit):
                m.d.comb += [
                    ans_valid.eq(1),
                    ans_error.eq(latch_error),
                    ans_done.eq(1),
                ]
            with m.Else():
                m.d.comb += [
                    ans_valid.eq(1),
                    ans_stale.eq(1),
                ]

        for c in range(n_cores):
            m.d.comb += self.wake_valid[c].eq((done_wake
                                               & (done_wake_core == c))
                                              | (ans_valid & (wait_core == c)))
            with m.If(done_wake & (done_wake_core == c)):
                m.d.comb += [
                    self.wake_wid[c].eq(done_wake_wid),
                    self.wake_stale[c].eq(0),
                    self.wake_error[c].eq(done_wake_error),
                    self.wake_done[c].eq(1),
                ]
            with m.If(ans_valid & (wait_core == c)):
                m.d.comb += [
                    self.wake_wid[c].eq(wait_wid),
                    self.wake_stale[c].eq(ans_stale),
                    self.wake_error[c].eq(ans_error),
                    self.wake_done[c].eq(ans_done),
                ]

        #
        # Launch arbitration into the engine
        #
        # Round-robin over the per-core instruction launches. A launch's
        # token must be FREE with the expected generation before the
        # command is offered to the engine; a launch naming anything else
        # is consumed and acknowledged with the reject status set, so the
        # issuing warp never hangs and software can observe the drop.
        #

        launch_arr = Array(self.launch)
        launch_valid_arr = Array([l.valid for l in self.launch])

        launch_rr = Signal(max(1, Shape.cast(range(n_cores)).width))
        launch_pick = Signal.like(launch_rr)
        launch_pick_valid = Signal()
        m.d.comb += [
            launch_pick.eq(launch_rr),
            launch_pick_valid.eq(launch_valid_arr[launch_rr]),
        ]
        for j in reversed(range(1, n_cores)):
            idx = (launch_rr + j) % n_cores
            with m.If(launch_valid_arr[idx]):
                m.d.comb += [
                    launch_pick.eq(idx),
                    launch_pick_valid.eq(1),
                ]

        token_ok = Signal()
        picked_id = Signal(DMA_ID_WIDTH)
        picked_gen = Signal()
        m.d.comb += [
            picked_id.eq(launch_arr[launch_pick].bits.cmd.id),
            picked_gen.eq(launch_arr[launch_pick].bits.gen),
        ]

        token_ok_terms = []
        for i in range(self.n_tokens):
            token_ok_terms.append((picked_id == i) & tk_free[i]
                                  & (tk_gen[i] == picked_gen))
        m.d.comb += token_ok.eq(Cat(*token_ok_terms).any())

        m.d.comb += [
            self.cmd.valid.eq(launch_pick_valid & token_ok),
            self.cmd.bits.eq(launch_arr[launch_pick].bits.cmd),
        ]
        for i in range(n_cores):
            with m.If(launch_pick == i):
                m.d.comb += self.cmd.bits.core.eq(i)

        cmd_fire = self.cmd.fire

        launch_serviced = []
        for i in range(n_cores):
            picked = (launch_pick == i) & launch_pick_valid
            accept = picked & token_ok
            drop = picked & ~token_ok
            m.d.comb += [
                self.launch[i].ready.eq((accept & cmd_fire) | drop),
                self.ack_valid[i].eq((accept & cmd_fire) | drop),
                self.ack_wid[i].eq(launch_arr[i].bits.wid),
                self.ack_reject[i].eq(drop),
            ]
            launch_serviced.append((accept & cmd_fire) | drop)

        with m.If(Cat(*launch_serviced).any()):
            m.d.sync += launch_rr.eq((launch_rr + 1) % n_cores)

        # Each token progresses independently. A wait arriving with done
        # can consume straight from FLIGHT; it must not leave an extra
        # DONE cycle or toggle the generation twice.
        for i in range(self.n_tokens):
            consume = done_has_waiter[i] | tk_wait_consume[i]
            with m.If(consume):
                m.d.sync += [
                    tk_gen[i].eq(~tk_gen[i]),
                    tk_waiter[i].eq(0),
                    tk_res_valid[i].eq(1),
                    tk_res_gen[i].eq(tk_gen[i]),
                    tk_res_error[i].eq(
                        Mux(tk_flight[i], done_error, tk_error[i])),
                ]

            with m.FSM(name=f'token{i}', reset='FREE') as token_fsm:
                m.d.comb += [
                    tk_free[i].eq(token_fsm.ongoing('FREE')),
                    tk_flight[i].eq(token_fsm.ongoing('FLIGHT')),
                    tk_done[i].eq(
                        token_fsm.ongoing('DONE_OK')
                        | token_fsm.ongoing('DONE_ERR')),
                    tk_error[i].eq(token_fsm.ongoing('DONE_ERR')),
                ]
                with m.State('FREE'):
                    with m.If(cmd_fire & (picked_id == i)):
                        m.next = 'FLIGHT'

                with m.State('FLIGHT'):
                    with m.If(consume):
                        m.next = 'FREE'
                    with m.Elif(done_fire & (done_id == i)):
                        with m.If(done_error):
                            m.next = 'DONE_ERR'
                        with m.Else():
                            m.next = 'DONE_OK'

                with m.State('DONE_OK'):
                    with m.If(consume):
                        m.next = 'FREE'

                with m.State('DONE_ERR'):
                    with m.If(consume):
                        m.next = 'FREE'

        m.d.comb += self.busy.eq(~Cat(*tk_free).all() | done_q.count.any())

        return m
