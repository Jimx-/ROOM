from amaranth import *
from amaranth import tracer
from amaranth.utils import log2_int
from amaranth.hdl.ast import ValueCastable

from groom.lsu import DMA_ID_WIDTH, SharedMemoryDMACommit, SharedMemoryDMAReq
from room.types import HasCoreParams
from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Decoupled


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
        self.dst_offset = Signal(log2_int(self.smem_size) + 1,
                                 name=f'{name}_dst_offset')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.id, self.core, self.src_addr, self.nbytes,
                   self.dst_offset)

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
        self.line_bits = log2_int(block_bytes)
        self.beat_bytes = 8
        self.beats_per_line = block_bytes // self.beat_bytes
        self.n_slots = n_slots
        self.slot_bits = log2_int(n_slots)

        assert block_bytes >= self.beat_bytes
        assert self.beats_per_line >= 1
        assert (block_bytes & (block_bytes - 1)) == 0
        assert (n_slots & (n_slots - 1)) == 0
        assert self.line_bits <= 7

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
        dst_bits = log2_int(self.smem_size) + 1
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
            Signal(dst_bits, name=f'cmd_q_dst{i}')
            for i in range(cmd_queue_depth)
        ]
        q_head = Signal(range(cmd_queue_depth))
        q_tail = Signal(range(cmd_queue_depth))
        q_count = Signal(range(cmd_queue_depth + 1))

        q_id_arr = Array(q_id)
        q_core_arr = Array(q_core)
        q_src_arr = Array(q_src)
        q_nbytes_arr = Array(q_nbytes)
        q_dst_arr = Array(q_dst)

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
                        ]
            m.d.sync += q_tail.eq((q_tail + 1) % cmd_queue_depth)

        cmd_pop = Signal()
        with m.If(cmd_pop):
            m.d.sync += q_head.eq((q_head + 1) % cmd_queue_depth)
        m.d.sync += q_count.eq(q_count + self.cmd.fire - cmd_pop)

        #
        # Transfer context
        #

        xfer_valid = Signal()
        xfer_id = Signal(DMA_ID_WIDTH)
        xfer_core = Signal(self.core_bits)
        xfer_failed = Signal()
        xfer_src = Signal(32)
        xfer_end = Signal(33)
        xfer_dst = Signal(dst_bits)
        xfer_line = Signal(32)
        xfer_last = Signal(32)
        addr_done = Signal()
        lines_out = Signal(range(self.n_slots + 1))
        frags_out = Signal(range(1 << 14))

        head_src = q_src_arr[q_head]
        head_end = head_src + q_nbytes_arr[q_head]
        head_last = Cat(Const(0, self.line_bits),
                        (head_end - 1)[self.line_bits:])

        head_nonempty = q_nbytes_arr[q_head] != 0
        read_start = Cat(Const(0, self.line_bits), head_src[self.line_bits:])
        read_end = head_last + self.block_bytes
        cmd_overflow = head_end > (1 << 32)
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
        dst_end = q_dst_arr[q_head] + q_nbytes_arr[q_head]
        cmd_reject = ((q_core_arr[q_head] >= self.n_cores)
                      | (dst_end > self.smem_size)
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
                    xfer_src.eq(head_src),
                    xfer_end.eq(head_end),
                    xfer_dst.eq(q_dst_arr[q_head]),
                    xfer_line.eq(
                        Cat(Const(0, self.line_bits),
                            head_src[self.line_bits:])),
                    xfer_last.eq(head_last[:32]),
                    xfer_failed.eq(0),
                    addr_done.eq(q_nbytes_arr[q_head] == 0),
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
        # been latched and all outstanding traffic has drained.
        xfer_complete = (xfer_valid & (addr_done | xfer_failed)
                         & (lines_out == 0) & (frags_out == 0)
                         & ~self.done.valid & ~issue_locked)
        with m.If(xfer_complete):
            m.d.sync += [
                xfer_valid.eq(0),
                self.done.valid.eq(1),
                self.done.bits.id.eq(xfer_id),
                self.done.bits.error.eq(xfer_failed),
            ]

        with m.If(self.done.fire):
            m.d.sync += self.done.valid.eq(0)

        m.d.comb += self.busy.eq((q_count != 0) | xfer_valid
                                 | self.done.valid
                                 | Cat(*(s != state_invalid
                                         for s in slot_state)).any())

        return m
