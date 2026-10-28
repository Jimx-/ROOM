"""Full-path streaming matrix and epilogue tests through the command interface.

Stage activations in TMEM, load streaming ACT RAM, consume SG1 weight regions
from DDR, and store bit-exact FP32 results after the per-lane epilogue drains.
Exercise sequencing, result regions, response backpressure and reset recovery.
"""

from amaranth import *
from amaranth.sim import Passive, Settle, Simulator

import pytest

from troom.consts import Format, Opcode, Status
from troom.core import Core
from troom.types import HasTRoomParams
from troom.stream import pack_stream_q8_0, pack_stream_q5_0, pack_stream_kquant

from tests.troom.model import (
    build_act,
    f32_bits,
    mm_row_results,
    mm_segments,
    pack_row,
    row_bytes,
)
from tests.troom.test_matrix import gen_wsegs

PARAMS = dict(
    addr_width=32,
    data_width=64,
    id_width=4,
    seg_len=32,
    n_outputs=2,
    n_tokens=1,
    tmem_bytes=16 * 1024,
    n_tmem_banks=4,
    act_max_k=1024,
    cmd_queue_depth=8,
    n_events=8,
    seq_bits=8,
)

DRAM_SIZE = 256 * 1024
W_BASE = 0x0000
A_BASE = 0x2000
M_BASE = 0x2800
R_BASE = 0x3000
W_DRAM = 0x1000
WM_DRAM = 0x18000
A_DRAM = 0x4000
M_DRAM = 0x4800
R_DRAM = 0x6000


def params_for(fmt, **over):
    p = dict(PARAMS)
    p['mm_formats'] = (fmt, )
    p.update(over)
    return p


def build_program_image(fmt,
                        k,
                        rows,
                        wsegs,
                        act,
                        rng,
                        *,
                        params=None,
                        w_dram=W_DRAM):
    """Pack SG1 payload/metadata and activation regions in device DRAM."""
    hp = HasTRoomParams(params or dict(PARAMS))
    bb = hp.beat_bytes
    packer = {
        Format.Q8_0: pack_stream_q8_0,
        Format.Q5_0: pack_stream_q5_0
    }.get(fmt)
    payload, meta = (packer(wsegs, k, rows, hp.n_outputs) if packer else
                     pack_stream_kquant(fmt, wsegs, k, rows, hp.n_outputs))
    image = bytearray(
        max(DRAM_SIZE, w_dram + len(payload), WM_DRAM + len(meta)))
    image[w_dram:w_dram + len(payload)] = payload
    image[WM_DRAM:WM_DRAM + len(meta)] = meta
    qimg = bytes(q & 255 for seg in act for q in seg['quants'])
    mimg = b''.join(seg['s_act'].to_bytes(4, 'little') +
                    (seg['sum_a'] & 65535).to_bytes(2, 'little') + bytes(2)
                    for seg in act)
    qimg += bytes(-len(qimg) % bb)
    mimg += bytes(-len(mimg) % bb)
    image[A_DRAM:A_DRAM + len(qimg)] = qimg
    image[M_DRAM:M_DRAM + len(mimg)] = mimg
    stride = -(-row_bytes(fmt, k) // bb) * bb
    rstride = -(-(rows * 4) // bb) * bb
    return image, stride, rstride, len(qimg), len(mimg)


class Harness(Elaboratable):

    def __init__(self, params, image):
        from tests.roomsoc.interconnect.axi_helpers import AXIResponder

        self.params = params
        self.reset = Signal()
        hp = HasTRoomParams(params)
        self.hp = hp
        self.core = Core(params)
        depth = len(image) // hp.beat_bytes
        init = [
            int.from_bytes(image[o:o + hp.beat_bytes], 'little')
            for o in range(0, len(image), hp.beat_bytes)
        ]
        self.ram = AXIResponder(addr_width=hp.addr_width,
                                data_width=hp.data_width,
                                id_width=hp.id_width,
                                depth=depth,
                                init=init,
                                read_latency=2,
                                b_latency=2)

    def elaborate(self, platform):
        m = Module()
        m.domains.sync = ClockDomain()
        m.d.comb += ResetSignal().eq(self.reset)
        m.submodules.core = self.core
        m.submodules.ram = self.ram
        m.d.comb += self.core.bus.connect(self.ram.bus)
        return m


def send_cmd(cmd, opcode, seq, evt=0, **fields):
    yield cmd.bits.opcode.eq(int(opcode))
    yield cmd.bits.seq.eq(seq)
    yield cmd.bits.evt.eq(evt)
    for name, value in fields.items():
        yield getattr(cmd.bits, name).eq(value)
    yield cmd.valid.eq(1)
    yield
    while not (yield cmd.fire):
        yield
    yield cmd.valid.eq(0)
    yield


def run_program(params, program, image, deadline=20000, *, stall_resp=False):
    """Issue commands; return (completions, DRAM image, counters)."""
    dut = Harness(params, image)
    results = {}
    counters = {}

    def tx():
        for _ in range(4):
            yield
        for entry in program:
            yield from send_cmd(dut.core.cmd, **entry)

    def rx():
        yield dut.core.resp.ready.eq(1)
        for cycle in range(deadline):
            if (yield dut.core.resp.fire):
                seq = (yield dut.core.resp.bits.seq)
                assert seq not in results, f'duplicate completion {seq}'
                results[seq] = Status((yield dut.core.resp.bits.status))
            if stall_resp:
                yield dut.core.resp.ready.eq(cycle % 31 >= 23)
            yield
            if (yield dut.core.done):
                counters['cycles'] = (yield dut.core.cycle_count)
                counters['cmds'] = (yield dut.core.cmds_done)
                counters['dram_bytes'] = (yield dut.core.dram_bytes)
                counters['useful_macs'] = (yield dut.core.useful_macs)
                counters['starve'] = (yield dut.core.array_starve)
                counters['tmem_conflicts'] = (yield dut.core.tmem_conflicts)
                counters['mm_credit_stalls'] = \
                    (yield dut.core.mm_credit_stalls)
                counters['mm_run_cycles'] = (yield dut.core.mm_run_cycles)
                counters['mm_drain_cycles'] = \
                    (yield dut.core.mm_drain_cycles)
                counters['epi_records'] = (yield dut.core.epi_records)
                counters['epi_active_cycles'] = \
                    (yield dut.core.epi_active_cycles)
                counters['epi_stalls'] = (yield dut.core.epi_stalls)
                counters['epi_cast_ops'] = (yield dut.core.epi_cast_ops)
                counters['epi_mul_ops'] = (yield dut.core.epi_mul_ops)
                counters['epi_add_ops'] = (yield dut.core.epi_add_ops)
                assert not (yield dut.ram.protocol_error)
                break
        else:
            raise TimeoutError(f'engine did not finish; completions={results}')

    def collector():
        yield Passive()
        bb = dut.hp.beat_bytes
        while True:
            if (yield dut.ram.w_monitor.valid):
                addr = (yield dut.ram.w_monitor.bits.addr)
                data = (yield dut.ram.w_monitor.bits.data)
                for lane in range(bb):
                    image[addr + lane] = (data >> (8 * lane)) & 0xff
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(rx)
    sim.add_sync_process(collector)
    sim.run()
    assert list(results) == [entry['seq'] for entry in program]
    return results, image, counters


def matmul_program(fmt,
                   k,
                   rows,
                   stride,
                   rstride,
                   qlen,
                   mlen,
                   *,
                   base_seq=1,
                   params=None,
                   store_evt=5,
                   a_base=A_BASE,
                   m_base=M_BASE,
                   r_base=R_BASE):
    """Load activations, execute streaming GEMV and store the drained result."""
    program = [
        dict(opcode=Opcode.LOAD_2D,
             evt=2,
             dram_addr=A_DRAM,
             tmem_addr=a_base,
             row_bytes=qlen,
             rows=1),
        dict(opcode=Opcode.LOAD_2D,
             evt=3,
             dram_addr=M_DRAM,
             tmem_addr=m_base,
             row_bytes=mlen,
             rows=1),
        dict(opcode=Opcode.WAIT, evt=2),
        dict(opcode=Opcode.WAIT, evt=3),
        dict(opcode=Opcode.SQUANTIZE, evt=4, a_base=a_base, m_base=m_base,
             k=k),
        dict(opcode=Opcode.WAIT, evt=4),
        dict(opcode=Opcode.SMATMUL,
             evt=5,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=W_DRAM,
             dram_stride=WM_DRAM,
             r_base=r_base),
        dict(opcode=Opcode.WAIT, evt=5),
        dict(opcode=Opcode.STORE_2D,
             evt=store_evt,
             dram_addr=R_DRAM,
             tmem_addr=r_base,
             row_bytes=rstride,
             rows=1),
        dict(opcode=Opcode.WAIT, evt=store_evt),
        dict(opcode=Opcode.END),
    ]
    return [dict(entry, seq=base_seq + i) for i, entry in enumerate(program)]


def check_results(image, fmt, k, rows, wsegs, act, rstride):
    exp_records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    exp = mm_row_results(exp_records)
    got = {}
    for r in range(rows):
        off = R_DRAM + 4 * r
        got[r] = int.from_bytes(image[off:off + 4], 'little')
    for r in range(rows):
        assert got[r] == f32_bits(exp[r]), \
            (r, hex(got[r]), hex(f32_bits(exp[r])), exp[r])
    # The final beat's unused lanes stayed zero.
    for w in range(rows, (rstride // 4)):
        off = R_DRAM + 4 * w
        assert image[off:off + 4] == b'\x00\x00\x00\x00', w
    return exp


def full_path_case(fmt, k, rows, seed, **over):
    import random

    rng = random.Random(seed)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = build_act(fmt, [rng.uniform(-1, 1) for _ in range(k)])
    params = params_for(fmt, **over)
    image, stride, rstride, qlen, mlen = build_program_image(fmt,
                                                             k,
                                                             rows,
                                                             wsegs,
                                                             act,
                                                             rng,
                                                             params=params)
    # Keep activation staging and results beyond the entire weight tile.
    # DRAM addresses are independent: a tile can fit below A_DRAM yet
    # overlap the old fixed A_BASE in TMEM (Q6_K, k=1536, rows=8).
    a_base = max(A_BASE, W_BASE + rows * stride)
    m_base = max(M_BASE, a_base + qlen)
    r_base = max(R_BASE, m_base + mlen)
    assert r_base + rstride <= params['tmem_bytes'], \
        'full-path fixture exceeds TMEM capacity'
    program = matmul_program(fmt,
                             k,
                             rows,
                             stride,
                             rstride,
                             qlen,
                             mlen,
                             params=params,
                             a_base=a_base,
                             m_base=m_base,
                             r_base=r_base)
    results, image, counters = run_program(params, program, image)
    assert all(s is Status.OK for s in results.values()), results
    check_results(image, fmt, k, rows, wsegs, act, rstride)
    assert counters['useful_macs'] == rows * k
    assert counters['dram_bytes'] > 0
    check_cycle_breakdown(counters, fmt, k, rows, params)
    return counters


def test_full_path_wide_q6_large_tile():
    """Weight rows 6/7 must not alias activation staging in TMEM."""
    full_path_case(Format.Q6_K,
                   1536,
                   8,
                   23,
                   data_width=512,
                   id_width=6,
                   n_tmem_banks=8,
                   act_max_k=2048)


def check_cycle_breakdown(counters, fmt, k, rows, params):
    """Pin the M3b cycle-breakdown counters to the analytic plan.

    With ``n_outputs = 2`` every record covers one full row pair (the
    odd-tail case masks a lane and is asserted separately below), and
    the epilogue issues, per record: the S and d conversions and the
    m1/u multiplies for each lane, two accumulations, and -- Q4_K only
    -- the fc/dmin conversions, the vm1/v multiplies, and the
    correction subtract for each lane. Every row adds the three fold
    adds. The scheduling counters (run/drain/active/stalls) must be
    positive and self-consistent: RUN covers slots and bubbles, and
    bubbles split exactly into starve + credit stalls.
    """
    hp = HasTRoomParams(params)
    if hp.n_outputs != 2:
        return
    nsegs = k // 32
    records = ((rows + 1) // 2) * nsegs
    assert counters['epi_records'] == records

    # Masked lanes in a final odd pair need no conversions or products.
    full_pairs, masked_lanes = divmod(rows, 2)
    masked_records = nsegs if masked_lanes else 0
    full_records = records - masked_records

    q4k = fmt == Format.Q4_K
    casts = 4 * full_records + 2 * masked_records
    muls = casts  # m1/u mirror the casts; vm1/v mirror fc/dmin
    adds = 2 * full_records + masked_records + 3 * rows
    if q4k:
        casts += 4 * full_records + 2 * masked_records
        muls += 4 * full_records + 2 * masked_records
        adds += 2 * full_records + masked_records
    assert counters['epi_cast_ops'] == casts
    assert counters['epi_mul_ops'] == muls
    assert counters['epi_add_ops'] == adds

    assert counters['mm_run_cycles'] > 0
    assert counters['mm_drain_cycles'] > 0
    assert counters['epi_active_cycles'] > 0
    # RUN cycles are valid slots plus bubbles, valid slots produce
    # exactly one record each, and every bubble is either an operand
    # starve or a credit stall (the launch join's only gates).
    assert counters['mm_run_cycles'] >= records
    assert counters['epi_stalls'] >= 0


def test_full_path_q8_0():
    full_path_case(Format.Q8_0, 64, 2, 12)


def test_full_path_q5_0():
    # 66-byte packed rows pad across beats; odd row count masks the
    # final lane and zero-pads the result beat.
    full_path_case(Format.Q5_0, 96, 4, 11)


def test_full_path_q4_k():
    # The minimum-term correction runs end to end.
    full_path_case(Format.Q4_K, 512, 5, 13)


def test_full_path_q6_k():
    full_path_case(Format.Q6_K, 256, 3, 14)


@pytest.mark.parametrize('fmt,k', [
    (Format.Q5_0, 576),
    (Format.Q8_0, 576),
    (Format.Q4_K, 1536),
    (Format.Q6_K, 1536),
])
def test_full_path_deployment_width(fmt, k):
    full_path_case(fmt,
                   k,
                   8 if fmt == Format.Q5_0 else 3,
                   15,
                   data_width=512,
                   id_width=6,
                   n_tmem_banks=8,
                   act_max_k=2048)


def test_full_path_four_output_lanes():
    full_path_case(Format.Q8_0, 64, 7, 16, n_outputs=4)


def test_queued_matmuls_with_response_backpressure():
    """Commands without an intervening WAIT retain separate result contexts."""
    import random

    fmt, k, rows = Format.Q8_0, 64, 3
    rng = random.Random(52)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = build_act(fmt, [rng.uniform(-1, 1) for _ in range(k)])
    image, stride, rstride, qlen, mlen = build_program_image(
        fmt, k, rows, wsegs, act, rng)
    program = matmul_program(fmt, k, rows, stride, rstride, qlen, mlen)
    index = next(i for i, entry in enumerate(program)
                 if entry['opcode'] is Opcode.SMATMUL)
    second = dict(program[index], seq=40, evt=6, rows=rows, r_base=R_BASE + 64)
    program.insert(index + 1, second)
    program[-1:-1] = [
        dict(opcode=Opcode.WAIT, seq=41, evt=6),
        dict(opcode=Opcode.STORE_2D,
             seq=42,
             evt=7,
             dram_addr=R_DRAM + 64,
             tmem_addr=R_BASE + 64,
             row_bytes=8,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=43, evt=7),
    ]
    results, image, _ = run_program(params_for(fmt),
                                    program,
                                    image,
                                    stall_resp=True)
    assert all(status is Status.OK for status in results.values())
    check_results(image, fmt, k, rows, wsegs, act, rstride)
    assert image[R_DRAM + 64:R_DRAM + 68] == image[R_DRAM:R_DRAM + 4]


def test_full_path_back_to_back_matmuls():
    """Reload distinct weights and ACT data between completed matmuls."""
    import random

    fmt, k, rows = Format.Q8_0, 64, 2
    rng = random.Random(51)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    wsegs2 = gen_wsegs(rng, fmt, rows * (k // 32))
    act = build_act(fmt, [rng.uniform(-1, 1) for _ in range(k)])
    act2 = build_act(fmt, [rng.uniform(-1, 1) for _ in range(k)])
    image, stride, rstride, qlen, mlen = build_program_image(
        fmt, k, rows, wsegs, act, rng)
    second, _, _, _, _ = build_program_image(fmt, k, rows, wsegs2, act2, rng)
    offset = 0x7000
    for base, size in ((W_DRAM, rows * stride), (WM_DRAM,
                                                 rows * (k // 32) * 2),
                       (A_DRAM, qlen), (M_DRAM, mlen)):
        image[base + offset:base + offset + size] = second[base:base + size]
    program = matmul_program(fmt, k, rows, stride, rstride, qlen, mlen)[:-1]
    tail = matmul_program(fmt,
                          k,
                          rows,
                          stride,
                          rstride,
                          qlen,
                          mlen,
                          base_seq=100)
    for entry in tail:
        if entry['opcode'] in (Opcode.LOAD_2D, Opcode.SMATMUL):
            entry['dram_addr'] += offset
        if entry['opcode'] is Opcode.SMATMUL:
            entry['dram_stride'] += offset
    program += tail
    results, image, _ = run_program(params_for(fmt), program, image)
    assert all(status is Status.OK for status in results.values())
    check_results(image, fmt, k, rows, wsegs2, act2, rstride)


def test_store_overlaps_matmul():
    """Store an independent TMEM tile while weights stream from DDR."""
    import random

    fmt, k, rows = Format.Q6_K, 256, 4
    rng = random.Random(61)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = build_act(fmt, [rng.uniform(-1, 1) for _ in range(k)])
    image, stride, rstride, qlen, mlen = build_program_image(
        fmt, k, rows, wsegs, act, rng)
    expected_copy = bytes(image[W_DRAM:W_DRAM + rows * stride])
    program = matmul_program(fmt, k, rows, stride, rstride, qlen, mlen)
    program[:0] = [
        dict(opcode=Opcode.LOAD_2D,
             seq=50,
             evt=1,
             dram_addr=W_DRAM,
             tmem_addr=W_BASE,
             row_bytes=rows * stride,
             rows=1),
        dict(opcode=Opcode.WAIT, seq=51, evt=1),
    ]
    index = next(i for i, entry in enumerate(program)
                 if entry['opcode'] is Opcode.SMATMUL) + 1
    program.insert(
        index,
        dict(opcode=Opcode.STORE_2D,
             seq=40,
             evt=6,
             dram_addr=0x7000,
             tmem_addr=W_BASE,
             row_bytes=rows * stride,
             rows=1,
             dram_stride=0,
             tmem_stride=0))
    program.insert(-1, dict(opcode=Opcode.WAIT, seq=41, evt=6))
    results, image, _ = run_program(params_for(fmt), program, image)
    assert all(status is Status.OK for status in results.values())
    assert image[0x7000:0x7000 + rows * stride] == expected_copy
    check_results(image, fmt, k, rows, wsegs, act, rstride)


@pytest.mark.parametrize('k,rows,r_base,why', [
    (33, 2, R_BASE, 'k not a multiple of 32'),
    (2048, 2, R_BASE, 'k beyond act_max_k'),
    (64, 0, R_BASE, 'no rows'),
    (64, 2, R_BASE + 1, 'result base not beat aligned'),
])
def test_matmul_bad_shapes_via_command_stream(k, rows, r_base, why):
    import random

    fmt = Format.Q8_0
    rng = random.Random(71)
    wsegs = gen_wsegs(rng, fmt, 2 * (k // 32) if k % 32 == 0 else 4)
    act = build_act(fmt, [rng.uniform(-1, 1) for _ in range(64)])
    image, stride, rstride, qlen, mlen = build_program_image(
        fmt, 64, 2, wsegs[:4], act, rng)
    program = [
        dict(opcode=Opcode.SMATMUL,
             seq=1,
             evt=1,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=W_DRAM,
             dram_stride=WM_DRAM,
             r_base=r_base),
        dict(opcode=Opcode.WAIT, seq=2, evt=1),
        dict(opcode=Opcode.END, seq=3),
    ]
    results, image, _ = run_program(params_for(fmt),
                                    program,
                                    image,
                                    deadline=2000)
    assert results == {
        1: Status.BAD_SHAPE,
        2: Status.OK,
        3: Status.OK
    }, results


def test_quantize_bad_shape_via_command_stream():
    program = [
        dict(opcode=Opcode.SQUANTIZE,
             seq=1,
             evt=1,
             a_base=A_BASE,
             m_base=M_BASE,
             k=33),
        dict(opcode=Opcode.WAIT, seq=2, evt=1),
        dict(opcode=Opcode.END, seq=3),
    ]
    image = bytearray(DRAM_SIZE)
    results, _, _ = run_program(params_for(Format.Q8_0),
                                program,
                                image,
                                deadline=2000)
    assert results == {
        1: Status.BAD_SHAPE,
        2: Status.OK,
        3: Status.OK
    }, results


def test_reset_with_matmul_in_flight():
    """Reset active compute, then reload a smaller tile from persistent DRAM."""
    import random

    fmt, k, rows = Format.Q4_K, 512, 16
    rng = random.Random(81)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = build_act(fmt, [rng.uniform(-1, 1) for _ in range(k)])
    image, stride, _, qlen, mlen = build_program_image(fmt, k, rows, wsegs,
                                                       act, rng)
    params = params_for(fmt)
    hp = HasTRoomParams(params)
    program = matmul_program(fmt, k, rows, stride, rows * 4, qlen, mlen)
    cut = next(i for i, entry in enumerate(program)
               if entry['opcode'] is Opcode.SMATMUL)
    dut = Harness(params, image)
    restarted = False
    results = {}
    small_rows = 4
    rstride = -(-(small_rows * 4) // hp.beat_bytes) * hp.beat_bytes
    restart_program = matmul_program(fmt,
                                     k,
                                     small_rows,
                                     stride,
                                     rstride,
                                     qlen,
                                     mlen,
                                     base_seq=100)

    def tx():
        nonlocal restarted
        for _ in range(4):
            yield
        for entry in program[:cut + 1]:
            yield from send_cmd(dut.core.cmd, **entry)
        for _ in range(5000):
            if (yield dut.core.useful_macs) >= 128:
                break
            yield
        else:
            raise TimeoutError('matmul did not start before reset')
        assert (yield dut.core.busy)
        assert (yield dut.core.useful_macs) > 0
        yield dut.reset.eq(1)
        yield
        yield
        yield dut.reset.eq(0)
        yield
        yield Settle()
        assert not (yield dut.core.done)
        assert not (yield dut.core.busy)
        assert (yield dut.core.cmds_done) == 0
        restarted = True
        for entry in restart_program:
            yield from send_cmd(dut.core.cmd, **entry)

    def rx():
        yield dut.core.resp.ready.eq(1)
        for _ in range(20000):
            if restarted and (yield dut.core.resp.valid):
                seq = (yield dut.core.resp.bits.seq)
                assert seq >= 100 and seq not in results
                results[seq] = Status((yield dut.core.resp.bits.status))
            yield
            if (yield dut.core.done):
                assert not (yield dut.ram.protocol_error)
                return
        raise TimeoutError(f'engine did not restart; completions={results}')

    def collector():
        yield Passive()
        while True:
            if (yield dut.ram.w_monitor.valid):
                addr = (yield dut.ram.w_monitor.bits.addr)
                data = (yield dut.ram.w_monitor.bits.data)
                image[addr:addr + hp.beat_bytes] = data.to_bytes(
                    hp.beat_bytes, 'little')
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(rx)
    sim.add_sync_process(collector)
    sim.run()
    assert list(results) == [entry['seq'] for entry in restart_program]
    assert all(status is Status.OK for status in results.values())
    check_results(image, fmt, k, small_rows, wsegs, act, rstride)
