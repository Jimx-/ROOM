"""Common types for the troom tensor accelerator.

``HasTRoomParams`` carries the validated parameter set (following
``room.types.HasCoreParams``); the 2D DMA descriptor records and their
layout are shared by the command sequencer and the DMA. Record types
with a single consumer live with that consumer: the host
command/completion contract in ``troom.core``, the engine-internal DMA
command in ``troom.dma``, and the matrix-unit payloads in
``troom.matrix``.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.utils import log2_int

from troom.consts import Format


class HasTRoomParams:
    """Validated troom parameters.

    Defaults match the initial proposal in
    ``plans/2026-10-05-troom-smollm-decoding.md``: a 32-element reduction
    segment, two independent output pipelines, one token lane, and a
    256 KiB TMEM. These are planning inputs, not characterized results.
    """

    def __init__(self, params):
        self.params = params

        #
        # Bus and DMA interface
        #
        self.addr_width = params.get('addr_width', 32)
        self.data_width = params.get('data_width', 64)
        self.id_width = params.get('id_width', 4)

        if (self.data_width % 8
                or log2_int(self.data_width, need_pow2=True) < 3):
            raise ValueError(
                f'data_width {self.data_width} must be a power of two of '
                f'at least 8 bits')

        self.beat_bytes = self.data_width // 8
        self.lg_beat = log2_int(self.beat_bytes)

        #
        # Systolic array geometry
        #
        self.seg_len = params.get('seg_len', 32)
        self.n_outputs = params.get('n_outputs', 2)
        self.n_tokens = params.get('n_tokens', 1)

        log2_int(self.seg_len, need_pow2=True)

        if self.n_outputs < 1:
            raise ValueError('n_outputs must be at least 1')

        # The matrix unit maps output rows to pipelines by dividing by
        # n_outputs; a power of two keeps that mapping shifts only.
        log2_int(self.n_outputs, need_pow2=True)

        if self.n_tokens < 1:
            raise ValueError('n_tokens must be at least 1')

        self.peak_macs = self.seg_len * self.n_outputs * self.n_tokens

        # The matrix unit's unpack/decode is defined for the GGUF block
        # sub-block size of 32 elements (Q5_0/Q8_0 blocks, and one
        # eighth of a K-quant super-block).
        if self.seg_len != 32:
            raise ValueError(
                f'seg_len {self.seg_len} must be 32 for the GGUF block '
                f'formats')

        #
        # Activation RAM
        #
        self.act_max_k = params.get('act_max_k', 2048)

        log2_int(self.act_max_k, need_pow2=True)
        if self.act_max_k % self.seg_len:
            raise ValueError(
                f'act_max_k {self.act_max_k} must be a multiple of '
                f'seg_len {self.seg_len}')

        self.lg_act_k = log2_int(self.act_max_k)
        self.act_segs = self.act_max_k // self.seg_len

        # A power-of-two segment count keeps ACT segment indices alias
        # free.
        self.lg_act_segs = log2_int(self.act_segs, need_pow2=True)

        #
        # Matrix unit queues
        #
        self.mm_formats = tuple(
            params.get('mm_formats',
                       (Format.Q5_0, Format.Q8_0, Format.Q4_K, Format.Q6_K)))
        self.mm_seg_fifo = params.get('mm_seg_fifo', 8)
        self.mm_act_fifo = params.get('mm_act_fifo', 4)
        self.mm_out_depth = params.get('mm_out_depth', 16)
        self.mm_result_q = params.get('mm_result_q', 4)

        if not self.mm_formats:
            raise ValueError('mm_formats must name at least one format')

        for f in self.mm_formats:
            if not isinstance(f, Format):
                raise ValueError(f'mm_formats contains {f!r}, not a '
                                 f'Format')

        # Positions of the configured formats inside mm_formats; the
        # supply decoders build only for the subset.
        self.fmt_pos = {f: i for i, f in enumerate(self.mm_formats)}

        if self.mm_seg_fifo < 1:
            raise ValueError('mm_seg_fifo must be at least 1')

        if self.mm_act_fifo < 1:
            raise ValueError('mm_act_fifo must be at least 1')

        if self.mm_out_depth < 1:
            raise ValueError('mm_out_depth must be at least 1')

        if self.mm_result_q < 1:
            raise ValueError('mm_result_q must be at least 1')

        #
        # Vector/reduction unit
        #
        self.vec_result_q = params.get('vec_result_q', 4)
        self.vec_sq_q = params.get('vec_sq_q', 4)

        if self.vec_result_q < 1:
            raise ValueError('vec_result_q must be at least 1')

        if self.vec_sq_q < 1:
            raise ValueError('vec_sq_q must be at least 1')

        #
        # Tensor memory
        #
        self.tmem_bytes = params.get('tmem_bytes', 256 * 1024)
        self.n_tmem_banks = params.get('n_tmem_banks', 4)

        self.tmem_addr_width = log2_int(self.tmem_bytes, need_pow2=True)
        log2_int(self.n_tmem_banks, need_pow2=True)

        if self.tmem_bytes % (self.n_tmem_banks * self.beat_bytes):
            raise ValueError(
                f'tmem_bytes {self.tmem_bytes} must be a multiple of '
                f'{self.n_tmem_banks} banks x {self.beat_bytes} bytes')

        self.tmem_bank_words = (self.tmem_bytes //
                                (self.n_tmem_banks * self.beat_bytes))

        #
        # 2D DMA descriptors
        #
        self.row_bytes_bits = params.get('row_bytes_bits', 16)
        self.rows_bits = params.get('rows_bits', 16)

        # A descriptor must be able to name at least one full beat and a
        # non-zero row count for the alignment checks to be expressible.
        if self.row_bytes_bits < self.lg_beat + 1:
            raise ValueError(
                f'row_bytes_bits {self.row_bytes_bits} must address at '
                f'least one {self.beat_bytes}-byte beat')

        if self.rows_bits < 1:
            raise ValueError('rows_bits must be at least 1')

        #
        # DMA engine sizing
        #
        self.dma_max_burst_beats = params.get('dma_max_burst_beats', 16)
        self.dma_max_outstanding = params.get('dma_max_outstanding', 8)
        self.dma_fifo_depth = params.get('dma_fifo_depth', 16)
        self.dma_queue_depth = params.get('dma_queue_depth', 4)

        if not (1 <= self.dma_max_burst_beats <= 256):
            raise ValueError(
                f'dma_max_burst_beats {self.dma_max_burst_beats} must be '
                f'in 1..256')

        if self.dma_max_outstanding < 1:
            raise ValueError('dma_max_outstanding must be at least 1')

        if self.dma_fifo_depth < 1:
            raise ValueError('dma_fifo_depth must be at least 1')

        if self.dma_queue_depth < 1:
            raise ValueError('dma_queue_depth must be at least 1')

        #
        # Command sequencing
        #
        self.cmd_queue_depth = params.get('cmd_queue_depth', 8)
        self.n_events = params.get('n_events', 8)
        self.seq_bits = params.get('seq_bits', 16)

        if self.cmd_queue_depth < 1:
            raise ValueError('cmd_queue_depth must be at least 1')

        # A power-of-two event count keeps event indices alias-free.
        self.evt_bits = log2_int(self.n_events, need_pow2=True)

        if self.seq_bits < 1:
            raise ValueError('seq_bits must be at least 1')


DMA_DESC_FIELDS = ('dram_addr', 'tmem_addr', 'row_bytes', 'rows',
                   'dram_stride', 'tmem_stride')


def dma_desc_layout(hp):
    """2D DMA descriptor layout for a validated parameter set.

    Field widths come from the parameters: DRAM addresses and strides
    share the byte-address width so descriptor arithmetic never
    truncates, TMEM addresses and strides are bounded by TMEM, and row
    lengths and counts use their own dedicated widths.
    """
    return [
        ('dram_addr', hp.addr_width),
        ('tmem_addr', hp.tmem_addr_width),
        ('row_bytes', hp.row_bytes_bits),
        ('rows', hp.rows_bits),
        ('dram_stride', hp.addr_width),
        ('tmem_stride', hp.tmem_addr_width),
    ]


class DmaDescriptor(HasTRoomParams, Record):
    """2D transfer descriptor payload for ``LOAD_2D``/``STORE_2D``.

    ``row_bytes`` bytes are moved for each of ``rows`` rows; row ``r``
    starts at ``dram_addr + r * dram_stride`` in DRAM and at
    ``tmem_addr + r * tmem_stride`` in TMEM. The initial contract
    requires every field to be a non-zero multiple of the bus beat,
    matching the aligned, padded device layouts from the plan; byte
    tails are future work.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [(name, width, Direction.FANOUT)
                               for name, width in dma_desc_layout(self)],
                        name=name,
                        src_loc_at=src_loc_at + 1)
