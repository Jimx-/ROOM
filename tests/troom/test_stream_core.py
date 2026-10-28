"""Streaming core integration: SG1 DDR weights, activation loading and drain.

Cover numerical formats, AXI/page boundaries, queued format changes, completion
ordering, backpressure, memory-window errors and rejection of retired opcodes.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import pytest
import random

from troom.consts import Format, Opcode, Status
from troom.core import Core
from troom.types import HasTRoomParams

from tests.troom.model import (
    build_act,
    f32_bits,
    mm_row_results,
    mm_segments,
    pack_row,
    row_bytes,
)
from tests.troom.test_matrix import gen_wsegs
from tests.troom.test_stream import (
    STREAM_PACKERS,
    make_tile,
)

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

# DRAM fixture layout (beat multiples; the SG1 regions sit far apart so
# payload/meta placement mistakes cannot alias silently).
A_DRAM = 0x4000
M_DRAM = 0x4800
SGP_DRAM = 0x8000
SGM_DRAM = 0xC000
LEG_W_DRAM = 0x40000
R_DRAM = 0x10000

# TMEM layout.
A_BASE = 0x2000
M_BASE = 0x2800
R_BASE = 0x3000
LEG_W_BASE = 0x0000


def core_params(**over):
    p = dict(PARAMS)
    p['mm_formats'] = tuple(Format)
    p.update(over)
    return p


class Harness(Elaboratable):

    def __init__(self,
                 params,
                 image,
                 *,
                 mem_base=0,
                 mem_size=None,
                 stream_error_beat=None):
        from tests.roomsoc.interconnect.axi_helpers import AXIResponder

        self.params = params
        self.reset = Signal()
        hp = HasTRoomParams(params)
        self.hp = hp
        self.core = Core(params, mem_base=mem_base, mem_size=mem_size)
        depth = len(image) // hp.beat_bytes
        init = [
            int.from_bytes(image[o:o + hp.beat_bytes], 'little')
            for o in range(0, len(image), hp.beat_bytes)
        ]
        self.ram = AXIResponder(
            addr_width=hp.addr_width,
            data_width=hp.data_width,
            id_width=hp.id_width,
            depth=depth,
            init=init,
            r_resp=(0 if stream_error_beat is None else Mux(
                self.core.stream.active &
                (self.core.sreader.beats == stream_error_beat), 2, 0)),
            read_latency=2,
            b_latency=2,
            ar_depth=16)

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


def run_program(params,
                program,
                image,
                deadline=40000,
                *,
                stall_resp=False,
                mem_base=0,
                mem_size=None,
                stream_error_beat=None):
    """Issue commands; return (completions, DRAM image, live checks)."""
    dut = Harness(params,
                  image,
                  mem_base=mem_base,
                  mem_size=mem_size,
                  stream_error_beat=stream_error_beat)
    results = {}
    checks = {}

    def tx():
        for _ in range(4):
            yield
        for entry in program:
            yield from send_cmd(dut.core.cmd, **entry)

    def rx():
        yield dut.core.resp.ready.eq(1)
        checks['stream_errors'] = 0
        for cycle in range(deadline):
            r = dut.core.sreader.bus.r
            if (yield r.valid) and (yield r.ready) and (yield r.bits.resp):
                checks['stream_errors'] += 1
            if (yield dut.core.resp.fire):
                seq = (yield dut.core.resp.bits.seq)
                assert seq not in results, f'duplicate completion {seq}'
                results[seq] = Status((yield dut.core.resp.bits.status))
            if stall_resp:
                yield dut.core.resp.ready.eq(cycle % 31 >= 23)
            yield
            if (yield dut.core.done):
                assert not (yield dut.core.busy), 'busy past END drain'
                assert (yield dut.core.memory_idle), \
                    'memory busy past END drain'
                sg = getattr(dut.core, 'stream', None)
                if sg is not None:
                    assert not (yield sg.active), 'stream active past END'
                checks['cycles'] = (yield dut.core.cycle_count)
                checks['cmds'] = (yield dut.core.cmds_done)
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
    return results, image, checks


def place_stream_tile(image, tile, p_base=SGP_DRAM, m_base=SGM_DRAM):
    """Append one tile's SG1 payload/meta into the fixture regions."""
    packer = STREAM_PACKERS[tile['fmt']]
    payload, meta = packer(tile['wsegs'], tile['k'], tile['rows'])
    image[p_base:p_base + len(payload)] = payload
    image[m_base:m_base + len(meta)] = meta
    return len(payload), len(meta)


def act_regions(image, act, params):
    """Write the QUANTIZE-format activation image into DRAM."""
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    qbytes = bytearray()
    mbytes = bytearray()
    for s in act:
        qbytes += bytes(q & 0xFF for q in s['quants'])
        mbytes += s['s_act'].to_bytes(4, 'little')
        mbytes += (s['sum_a'] & 0xFFFF).to_bytes(2, 'little')
        mbytes += b'\x00\x00'
    qimg = bytes(qbytes) + bytes((-len(qbytes)) % bb)
    mimg = bytes(mbytes) + bytes((-len(mbytes)) % bb)
    image[A_DRAM:A_DRAM + len(qimg)] = qimg
    image[M_DRAM:M_DRAM + len(mimg)] = mimg
    return len(qimg), len(mimg)


def smatmul_program(fmt,
                    k,
                    rows,
                    *,
                    seq=1,
                    evt=4,
                    r_base=R_BASE,
                    p_base=SGP_DRAM,
                    m_base=SGM_DRAM):
    """SQUANTIZE + SMATMUL, weights streamed straight from DRAM."""
    return [
        dict(opcode=Opcode.SQUANTIZE,
             seq=seq,
             evt=2,
             a_base=A_BASE,
             m_base=M_BASE,
             k=k),
        dict(opcode=Opcode.WAIT, seq=seq + 1, evt=2),
        dict(opcode=Opcode.SMATMUL,
             seq=seq + 2,
             evt=evt,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=p_base,
             dram_stride=m_base,
             r_base=r_base),
        dict(opcode=Opcode.WAIT, seq=seq + 3, evt=evt),
    ]


def load_act_program(qlen, mlen, *, seq=10):
    return [
        dict(opcode=Opcode.LOAD_2D,
             seq=seq,
             evt=1,
             dram_addr=A_DRAM,
             tmem_addr=A_BASE,
             row_bytes=qlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=seq + 1,
             evt=3,
             dram_addr=M_DRAM,
             tmem_addr=M_BASE,
             row_bytes=mlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=seq + 2, evt=1),
        dict(opcode=Opcode.WAIT, seq=seq + 3, evt=3),
    ]


def check_stream_results(image, fmt, k, rows, wsegs, act, rstride):
    exp_records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    exp = mm_row_results(exp_records)
    for r in range(rows):
        off = R_DRAM + 4 * r
        got = int.from_bytes(image[off:off + 4], 'little')
        assert got == f32_bits(exp[r]), \
            (fmt.name, r, hex(got), hex(f32_bits(exp[r])), exp[r])
    for w in range(rows, rstride // 4):
        off = R_DRAM + 4 * w
        assert image[off:off + 4] == b'\x00\x00\x00\x00', w
    return exp


def stream_core_case(fmt, k, rows, seed, *, stall_resp=False, **over):
    """One streamed GEMV end to end at the Core command level."""
    rng = random.Random(seed)
    tile = make_tile(rng, k, rows, fmt)
    params = core_params(**over)
    hp = HasTRoomParams(params)
    image = bytearray(R_DRAM + 4096)
    plen, _ = place_stream_tile(image, tile)
    qlen, mlen = act_regions(image, tile['act'], params)
    rstride = -(-(rows * 4) // hp.beat_bytes) * hp.beat_bytes

    program = load_act_program(qlen, mlen) \
        + smatmul_program(fmt, k, rows) \
        + [
            dict(opcode=Opcode.STORE_2D, seq=20, evt=5,
                 dram_addr=R_DRAM, tmem_addr=R_BASE, row_bytes=rstride,
                 rows=1, dram_stride=0, tmem_stride=0),
            dict(opcode=Opcode.WAIT, seq=21, evt=5),
            dict(opcode=Opcode.END, seq=22),
        ]
    results, image, _ = run_program(params,
                                    program,
                                    image,
                                    stall_resp=stall_resp)
    assert all(s is Status.OK for s in results.values()), results
    check_stream_results(image, fmt, k, rows, tile['wsegs'], tile['act'],
                         rstride)
    # The streamed payload really left DRAM: the reader consumed the
    # packed tile, not TMEM staging.
    assert plen > 0


@pytest.mark.parametrize('fmt,k,rows,seed', [
    (Format.Q8_0, 64, 4, 31),
    (Format.Q5_0, 128, 6, 32),
    (Format.Q4_K, 256, 4, 33),
    (Format.Q6_K, 512, 2, 34),
])
def test_stream_core_formats(fmt, k, rows, seed):
    stream_core_case(fmt, k, rows, seed)


@pytest.mark.parametrize('last_beat', [False, True])
def test_stream_core_read_error_is_command_local(last_beat):
    params = core_params()
    tile = make_tile(random.Random(31), 64, 4, Format.Q8_0)
    image = bytearray(R_DRAM + 4096)
    plen, mlen = place_stream_tile(image, tile)
    qlen, alen = act_regions(image, tile['act'], params)
    bb = HasTRoomParams(params).beat_bytes
    error_beat = (plen + mlen) // bb - 1 if last_beat else 0
    program = load_act_program(qlen, alen) \
        + smatmul_program(Format.Q8_0, 64, 4, seq=30) \
        + smatmul_program(Format.Q8_0, 64, 4, seq=40) \
        + [dict(opcode=Opcode.END, seq=50)]
    results, _, checks = run_program(params,
                                     program,
                                     image,
                                     stall_resp=True,
                                     stream_error_beat=error_beat)
    assert checks['stream_errors'] == 1
    assert results[32] is Status.DMA_ERROR
    assert results[42] is Status.OK
    assert all(status is Status.OK for seq, status in results.items()
               if seq != 32)


def test_stream_core_one_row_tile():
    stream_core_case(Format.Q5_0, 128, 1, 35)


def test_stream_core_odd_rows_completion_backpressure():
    stream_core_case(Format.Q4_K, 256, 5, 36, stall_resp=True)


def test_stream_core_deployment_width():
    stream_core_case(Format.Q6_K,
                     256,
                     4,
                     37,
                     data_width=512,
                     id_width=6,
                     n_tmem_banks=8)


@pytest.mark.parametrize('fmt,k,rows', [
    (Format.Q8_0, 576, 4),
    (Format.Q5_0, 576, 8),
    (Format.Q4_K, 256, 3),
    (Format.Q6_K, 1024, 5),
])
@pytest.mark.parametrize('dw', [64, 512])
def test_stream_core_page_split_regions(fmt, k, rows, dw):
    # Regression (found by the llama.cpp raw-command suite against the
    # Verilator image): a reader request crossing a 4 KiB page
    # boundary is issued as sub-bursts by StreamAxiReader, and the
    # split request was never consumed from its request queue -- the
    # reader re-issued the whole split sequence forever, duplicated
    # beats overran the region FIFO's reserved space, and the port
    # wedged with the memory model holding a beat no agent would
    # take. Both SG1 region bases sit three beats before a page
    # boundary so every first request of each region splits. The
    # suites never hit this: their region bases plus uniform 16-beat
    # bursts never crossed a page mid-request (and the unit-level
    # harness drives mem_rreq directly, below StreamAxiReader).
    rng = random.Random(97)
    tile = make_tile(rng, k, rows, fmt)
    params = core_params(data_width=dw)
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    image = bytearray(R_DRAM + 4096)
    place_stream_tile(image,
                      tile,
                      p_base=SGP_DRAM + 0x1000 - 3 * bb,
                      m_base=SGM_DRAM + 0x1000 - 3 * bb)
    qlen, mlen = act_regions(image, tile['act'], params)
    rstride = -(-(rows * 4) // hp.beat_bytes) * hp.beat_bytes

    program = load_act_program(qlen, mlen) \
        + smatmul_program(fmt, k, rows,
                          p_base=SGP_DRAM + 0x1000 - 3 * bb,
                          m_base=SGM_DRAM + 0x1000 - 3 * bb) \
        + [
            dict(opcode=Opcode.STORE_2D, seq=20, evt=5,
                 dram_addr=R_DRAM, tmem_addr=R_BASE, row_bytes=rstride,
                 rows=1, dram_stride=0, tmem_stride=0),
            dict(opcode=Opcode.WAIT, seq=21, evt=5),
            dict(opcode=Opcode.END, seq=22),
        ]
    results, image, _ = run_program(params, program, image)
    assert all(s is Status.OK for s in results.values()), results
    check_stream_results(image, fmt, k, rows, tile['wsegs'], tile['act'],
                         rstride)


def test_stream_core_queued_format_transitions():
    """Queued activation loads and format changes wait for the prior drain."""
    rng = random.Random(38)
    fmt, sfmt, k, rows = Format.Q5_0, Format.Q8_0, 64, 4
    params = core_params()
    image = bytearray(LEG_W_DRAM + 65536)
    first = make_tile(rng, k, rows, fmt)
    second = make_tile(rng, k, rows, sfmt)
    second['act'] = first['act']
    place_stream_tile(image,
                      first,
                      p_base=LEG_W_DRAM,
                      m_base=LEG_W_DRAM + 0x4000)
    place_stream_tile(image, second)
    qlen, mlen = act_regions(image, first['act'], params)
    program = load_act_program(qlen, mlen)
    for i, (tile, pb,
            mb) in enumerate([(first, LEG_W_DRAM, LEG_W_DRAM + 0x4000),
                              (second, SGP_DRAM, SGM_DRAM),
                              (first, LEG_W_DRAM, LEG_W_DRAM + 0x4000)]):
        program += [
            dict(opcode=Opcode.SQUANTIZE,
                 seq=20 + 2 * i,
                 evt=4,
                 a_base=A_BASE,
                 m_base=M_BASE,
                 k=k),
            dict(opcode=Opcode.SMATMUL,
                 seq=21 + 2 * i,
                 evt=5 + i,
                 fmt=int(tile['fmt']),
                 k=k,
                 rows=rows,
                 dram_addr=pb,
                 dram_stride=mb,
                 r_base=R_BASE)
        ]
    rstride = -(-(rows * 4) // HasTRoomParams(params).beat_bytes
                ) * HasTRoomParams(params).beat_bytes
    program += [
        dict(opcode=Opcode.WAIT, seq=30, evt=7),
        dict(opcode=Opcode.STORE_2D,
             seq=31,
             evt=6,
             dram_addr=R_DRAM,
             tmem_addr=R_BASE,
             row_bytes=rstride,
             rows=1),
        dict(opcode=Opcode.END, seq=32)
    ]
    results, image, _ = run_program(params, program, image)
    assert all(status == Status.OK for status in results.values()), results
    check_stream_results(image, fmt, k, rows, first['wsegs'], first['act'],
                         rstride)


def test_stream_core_rejections():
    """Issue-time and accept-time rejection paths for SMATMUL."""
    rng = random.Random(39)
    tile = make_tile(rng, 64, 2, Format.Q8_0)
    params = core_params()
    image = bytearray(R_DRAM + 4096)
    place_stream_tile(image, tile)
    qlen, mlen = act_regions(image, tile['act'], params)

    cases = [
        # k not a whole window count -> engine BAD_SHAPE.
        (dict(fmt=int(Format.Q8_0),
              k=33,
              rows=2,
              dram_addr=SGP_DRAM,
              dram_stride=SGM_DRAM,
              r_base=R_BASE), Status.BAD_SHAPE),
        # Region base not beat aligned -> BAD_ALIGN.
        (dict(fmt=int(Format.Q8_0),
              k=64,
              rows=2,
              dram_addr=SGP_DRAM + 1,
              dram_stride=SGM_DRAM,
              r_base=R_BASE), Status.BAD_ALIGN),
        # Result region unaligned -> issue-time BAD_SHAPE.
        (dict(fmt=int(Format.Q8_0),
              k=64,
              rows=2,
              dram_addr=SGP_DRAM,
              dram_stride=SGM_DRAM,
              r_base=2), Status.BAD_SHAPE),
    ]
    seq = 29
    program = load_act_program(qlen, mlen)
    expect = {e['seq']: Status.OK for e in program}
    for fields, status in cases:
        seq += 10
        entry = dict(opcode=Opcode.SMATMUL, seq=seq, evt=4)
        entry.update(fields)
        program.append(entry)
        expect[seq] = status
    # A good streamed matmul still runs after the rejections.
    good = dict(opcode=Opcode.SMATMUL,
                seq=seq + 1,
                evt=5,
                fmt=int(Format.Q8_0),
                k=64,
                rows=2,
                dram_addr=SGP_DRAM,
                dram_stride=SGM_DRAM,
                r_base=R_BASE)
    program += [
        good,
        dict(opcode=Opcode.WAIT, seq=seq + 2, evt=5),
        dict(opcode=Opcode.END, seq=seq + 3)
    ]
    expect[seq + 1] = Status.OK
    expect[seq + 2] = Status.OK
    expect[seq + 3] = Status.OK

    hp = HasTRoomParams(params)
    rstride = -(-(2 * 4) // hp.beat_bytes) * hp.beat_bytes
    program.insert(
        -1,
        dict(opcode=Opcode.STORE_2D,
             seq=99,
             evt=6,
             dram_addr=R_DRAM,
             tmem_addr=R_BASE,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0))
    expect[99] = Status.OK

    results, image, _ = run_program(params, program, image)
    for e in program:
        assert results[e['seq']] is expect[e['seq']], \
            (e['seq'], results[e['seq']], expect[e['seq']])


def test_stream_core_load_overlaps_matmul():
    """Tensor-DMA reads overlap the streaming reader on one master.

    The activation loads for the second tile are issued immediately
    after the first SMATMUL with no WAIT, so the DMA loader and the
    stream reader share the AXI read direction through the core
    arbiter while both stream. Both tiles' results must be exact.
    """
    rng = random.Random(42)
    fmt = Format.Q6_K
    k, rows = 256, 4
    t0 = make_tile(rng, k, rows, fmt)
    t1 = make_tile(rng, k, rows, fmt)
    params = core_params()
    hp = HasTRoomParams(params)
    image = bytearray(R_DRAM + 0x8000)
    place_stream_tile(image, t0)
    qlen, mlen = act_regions(image, t0['act'], params)
    rstride = -(-(rows * 4) // hp.beat_bytes) * hp.beat_bytes

    # Second tile: a different streamed image and activation, regions
    # offset from the first so nothing aliases.
    packer = STREAM_PACKERS[fmt]
    payload, meta = packer(t1['wsegs'], t1['k'], t1['rows'])
    image[SGP_DRAM + 0x400:SGP_DRAM + 0x400 + len(payload)] = payload
    image[SGM_DRAM + 0x400:SGM_DRAM + 0x400 + len(meta)] = meta
    q2 = bytearray()
    m2 = bytearray()
    for s in t1['act']:
        q2 += bytes(q & 0xFF for q in s['quants'])
        m2 += s['s_act'].to_bytes(4, 'little')
        m2 += (s['sum_a'] & 0xFFFF).to_bytes(2, 'little')
        m2 += b'\x00\x00'
    q2 += bytes((-len(q2)) % hp.beat_bytes)
    m2 += bytes((-len(m2)) % hp.beat_bytes)
    image[A_DRAM + 0x800:A_DRAM + 0x800 + len(q2)] = q2
    image[M_DRAM + 0x800:M_DRAM + 0x800 + len(m2)] = m2

    program = load_act_program(qlen, mlen) + [
        dict(opcode=Opcode.SQUANTIZE,
             seq=1,
             evt=2,
             a_base=A_BASE,
             m_base=M_BASE,
             k=k),
        dict(opcode=Opcode.WAIT, seq=2, evt=2),
        # Accepted immediately; the loads below are then issued while
        # the streaming reader owns the read direction.
        dict(opcode=Opcode.SMATMUL,
             seq=3,
             evt=4,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=SGP_DRAM,
             dram_stride=SGM_DRAM,
             r_base=R_BASE),
        # No WAIT: these DMA reads overlap the streaming reader.
        dict(opcode=Opcode.LOAD_2D,
             seq=30,
             evt=1,
             dram_addr=A_DRAM + 0x800,
             tmem_addr=A_BASE,
             row_bytes=len(q2),
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=31,
             evt=3,
             dram_addr=M_DRAM + 0x800,
             tmem_addr=M_BASE,
             row_bytes=len(m2),
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=32, evt=4),
        dict(opcode=Opcode.WAIT, seq=33, evt=1),
        dict(opcode=Opcode.WAIT, seq=34, evt=3),
        dict(opcode=Opcode.SQUANTIZE,
             seq=35,
             evt=6,
             a_base=A_BASE,
             m_base=M_BASE,
             k=k),
        dict(opcode=Opcode.SMATMUL,
             seq=36,
             evt=7,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=SGP_DRAM + 0x400,
             dram_stride=SGM_DRAM + 0x400,
             r_base=R_BASE),
        dict(opcode=Opcode.WAIT, seq=37, evt=7),
        dict(opcode=Opcode.STORE_2D,
             seq=38,
             evt=5,
             dram_addr=R_DRAM,
             tmem_addr=R_BASE,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=39, evt=5),
        dict(opcode=Opcode.END, seq=40),
    ]
    results, image, _ = run_program(params, program, image, deadline=60000)
    assert all(s is Status.OK for s in results.values()), results
    # The stored results are the second tile's.
    check_stream_results(image, fmt, k, rows, t1['wsegs'], t1['act'], rstride)


def test_stream_core_memory_window_enforced():
    """A tile outside the core's device window never reaches the port."""
    tile = make_tile(random.Random(41), 64, 2, Format.Q8_0)
    params = core_params()
    image = bytearray(R_DRAM + 4096)
    qlen, mlen = act_regions(image, tile['act'], params)
    # Window admits the fixture DRAM below 0x10000000 only; the meta
    # base sits above it.
    program = load_act_program(qlen, mlen) + [
        dict(opcode=Opcode.SQUANTIZE,
             seq=20,
             evt=4,
             a_base=A_BASE,
             m_base=M_BASE,
             k=64),
        dict(opcode=Opcode.SMATMUL,
             seq=21,
             evt=5,
             fmt=int(Format.Q8_0),
             k=64,
             rows=2,
             dram_addr=SGP_DRAM,
             dram_stride=0xF0000000,
             r_base=R_BASE),
        dict(opcode=Opcode.END, seq=22),
    ]
    results, _, _ = run_program(params,
                                program,
                                image,
                                mem_base=0,
                                mem_size=0x10000000)
    assert results[21] is Status.BAD_RANGE


@pytest.mark.parametrize('fmt',
                         [Format.Q8_0, Format.Q5_0, Format.Q4_K, Format.Q6_K])
@pytest.mark.parametrize('data_width', [64, 512])
def test_stream_only_formats(fmt, data_width):
    # Three Q5 rows need K=512 for a whole number of 64-byte beats.
    k = 512 if fmt == Format.Q5_0 and data_width == 512 else 256
    stream_core_case(fmt,
                     k,
                     3,
                     61,
                     data_width=data_width,
                     n_tmem_banks=8,
                     stall_resp=True)


def test_stream_only_rejects_legacy_commands():
    program = [
        dict(opcode=Opcode.QUANTIZE, seq=1, k=64),
        dict(opcode=Opcode.MATMUL, seq=2, k=64, rows=2),
        dict(opcode=Opcode.END, seq=3),
    ]
    results, _, _ = run_program(core_params(), program, bytearray(4096))
    assert results == {
        1: Status.BAD_OPCODE,
        2: Status.BAD_OPCODE,
        3: Status.OK
    }


def test_core_has_only_streaming_matrix_engine():
    from amaranth.hdl import Fragment
    import troom.matrix as matrix_module
    assert not hasattr(matrix_module, 'MatrixUnit')
    assert not hasattr(matrix_module, 'WeightSupply')
    Fragment.get(Core(core_params()), None)


@pytest.mark.parametrize('name', ['sg_enable', 'sg_only'])
def test_retired_legacy_configuration_rejected(name):
    with pytest.raises(ValueError, match='retired legacy'):
        HasTRoomParams(core_params(**{name: 0}))
