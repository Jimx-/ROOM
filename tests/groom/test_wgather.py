import pytest

from amaranth import *
from amaranth.sim import Settle, Simulator

from room.consts import (FUType, IssueQueueType, RegisterType, UOpCode)
from room.exc import Cause
from room.types import HasCoreParams

import groom.csrnames as gpucsrnames
from groom.id_stage import DecodeUnit
from groom.fu import WGatherUnit
from groom.regfile import RegisterFile, RegisterRead

from tests.sim import run_test
from tests.groom.encoding import addi, csrrs, gpu_tmc, jal, nop, slli, wgather
from tests.groom.sim import GroomCoreSim, run_core_sim, wb_monitor

TEST_PARAMS = {
    'is_groom': True,
    'xlen': 32,
    'flen': 32,
    'use_fpu': True,
    'vaddr_bits': 32,
    'io_regions': {},
    'fma_latency': 4,
    'n_cores': 1,
    'n_warps': 4,
    'n_threads': 4,
    'n_barriers': 4,
    'use_raster': False,
    'issue_params': {
        'queue_depth': 4,
    },
}

RS1 = [0xA0, 0xA1, 0xA2, 0xA3]
RS2 = [0xB0, 0xB1, 0xB2, 0xB3]
RS3 = [0xC0, 0xC1, 0xC2, 0xC3]

SRC_LANE_MASK = {0: 0b1110, 1: 0b1101, 2: 0b1011, 3: 0b0111}


def wgather_inst(rd, rs1, rs2, rs3, src_lane, funct3=0):
    return ((rs3 & 0x1f) << 27) | ((src_lane & 0x3) << 25) | (
        (rs2 & 0x1f) << 20) | ((rs1 & 0x1f) << 15) | ((funct3 & 0x7) << 12) | (
            (rd & 0x1f) << 7) | 0b0101011


# (src_lane, request tmask, expected data, expected response tmask)
GATHER_CASES = [
    # Full warps: every lane gathers from the nominal source lane (= lane).
    (0, 0b1111, [0, RS1[0], RS2[0], RS3[0]], SRC_LANE_MASK[0]),
    (1, 0b1111, [RS3[1], 0, RS1[1], RS2[1]], SRC_LANE_MASK[1]),
    (2, 0b1111, [RS2[2], RS3[2], 0, RS1[2]], SRC_LANE_MASK[2]),
    (3, 0b1111, [RS1[3], RS2[3], RS3[3], 0], SRC_LANE_MASK[3]),
    # Partial warps: nominal source lane inactive -> warp's last active lane.
    # Non-source inactive lanes are force-written (resp tmask ignores req
    # tmask outside the source-lane bit).
    (2, 0b0011, [RS2[1], RS3[1], 0, RS1[1]], SRC_LANE_MASK[2]),
    (1, 0b1000, [RS3[3], 0, RS1[3], RS2[3]], SRC_LANE_MASK[1]),
    (0, 0b0100, [0, RS1[2], RS2[2], RS3[2]], SRC_LANE_MASK[0]),
    (3, 0b0111, [RS1[2], RS2[2], RS3[2], 0], SRC_LANE_MASK[3]),
]


@pytest.mark.parametrize('src_lane,tmask,exp_data,exp_tmask', GATHER_CASES)
def test_wgather_unit(src_lane, tmask, exp_data, exp_tmask):
    dut = WGatherUnit(TEST_PARAMS)

    def proc():
        yield dut.req.bits.uop.imm_packed[13:15].eq(src_lane)
        yield dut.req.bits.uop.tmask.eq(tmask)
        for t in range(4):
            yield dut.req.bits.rs1_data[t].eq(RS1[t])
            yield dut.req.bits.rs2_data[t].eq(RS2[t])
            yield dut.req.bits.rs3_data[t].eq(RS3[t])
        yield dut.req.valid.eq(1)
        yield
        yield dut.req.valid.eq(0)
        yield

        assert (yield dut.resp.valid)
        got_tmask = (yield dut.resp.bits.uop.tmask)
        assert got_tmask == exp_tmask, \
            f'lane={src_lane} req tmask={tmask:#06b}: ' \
            f'resp tmask {got_tmask:#06b} != {exp_tmask:#06b}'
        for t in range(4):
            got = (yield dut.resp.bits.data[t])
            assert got == exp_data[t], \
                f'lane={src_lane} req tmask={tmask:#06b}: ' \
                f'data[{t}] {got:#x} != {exp_data[t]:#x}'

    run_test(dut, proc, sync=True)


def test_wgather_unit_pipelined():
    dut = WGatherUnit(TEST_PARAMS)

    def send_reqs():
        for lane in (0, 2):
            yield dut.req.bits.uop.imm_packed[13:15].eq(lane)
            yield dut.req.bits.uop.tmask.eq(0b1111)
            for t in range(4):
                yield dut.req.bits.rs1_data[t].eq(RS1[t])
                yield dut.req.bits.rs2_data[t].eq(RS2[t])
                yield dut.req.bits.rs3_data[t].eq(RS3[t])
            yield dut.req.valid.eq(1)
            yield
        yield dut.req.valid.eq(0)

    def collect():
        exp = [
            ([0, RS1[0], RS2[0], RS3[0]], SRC_LANE_MASK[0]),
            ([RS2[2], RS3[2], 0, RS1[2]], SRC_LANE_MASK[2]),
        ]
        for want_data, want_tmask in exp:
            for _ in range(8):
                if (yield dut.resp.valid):
                    break
                yield
            else:
                assert False, 'missing response'

            assert (yield dut.resp.bits.uop.tmask) == want_tmask
            for t in range(4):
                got = (yield dut.resp.bits.data[t])
                assert got == want_data[t], \
                    f'data[{t}] {got:#x} != {want_data[t]:#x}'
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(send_reqs)
    sim.add_sync_process(collect)
    sim.run()


def test_regfile_third_read_port():
    dut = RegisterFile(rports=12,
                       wports=4,
                       num_regs=32 * TEST_PARAMS['n_warps'],
                       data_width=32)

    def proc():
        # Each write port owns a private replica set serving its trio of
        # read ports (wport i -> read ports 3i..3i+2), as warp-wide
        # writeback drives every write port.
        for wport, addr, data in [(0, 5, 0x555), (0, 6, 0x666), (0, 7, 0x777),
                                  (1, 9 + 32 * 2, 0x999)]:
            yield dut.write_ports[wport].valid.eq(1)
            yield dut.write_ports[wport].bits.addr.eq(addr)
            yield dut.write_ports[wport].bits.data.eq(data)
            yield
        yield dut.write_ports[0].valid.eq(0)
        yield dut.write_ports[1].valid.eq(0)

        # rs1/rs2/rs3 of thread 0 read distinct registers.
        for port, addr in [(0, 5), (1, 6), (2, 7)]:
            yield dut.read_ports[port].addr.eq(addr)
        # The third port replicates the file: all three ports of a trio
        # return the same register, and thread 1's trio sees its own port's
        # write.
        for port, addr in [(3, 9 + 32 * 2), (4, 9 + 32 * 2), (5, 9 + 32 * 2)]:
            yield dut.read_ports[port].addr.eq(addr)
        yield

        for port, want in [(0, 0x555), (1, 0x666), (2, 0x777), (3, 0x999),
                           (4, 0x999), (5, 0x999)]:
            got = (yield dut.read_ports[port].data)
            assert got == want, \
                f'read port {port}: {got:#x} != {want:#x}'

        for port in range(3):
            yield dut.read_ports[port].addr.eq(5)
        yield
        for port in range(3):
            got = (yield dut.read_ports[port].data)
            assert got == 0x555, \
                f'replica coherence port {port}: {got:#x} != 0x555'

    run_test(dut, proc, sync=True)


class IntRegRead(HasCoreParams, Elaboratable):
    """Integer register file plus register read, wired as in groom.core."""

    def __init__(self, params):
        super().__init__(params)
        self.params = params

        self.regfile = RegisterFile(rports=self.n_threads * 3,
                                    wports=self.n_threads,
                                    num_regs=32 * self.n_warps,
                                    data_width=self.xlen)
        self.regread = RegisterRead(num_rports=self.n_threads * 3,
                                    rports_array=[3] * self.n_threads,
                                    reg_width=self.xlen,
                                    params=params)

    def elaborate(self, platform):
        m = Module()
        m.submodules.regfile = self.regfile
        m.submodules.regread = self.regread
        for irr_rp, rp in zip(self.regread.read_ports,
                              self.regfile.read_ports):
            m.d.comb += irr_rp.connect(rp)
        return m


def test_regread_rs3_port_and_x0():
    dut = IntRegRead(TEST_PARAMS)

    def proc():
        regfile, regread = dut.regfile, dut.regread

        # Physically write x0 (the decode path never does, so this proves
        # the rs3 mux, not memory zero-init) and x1, through every write
        # port so all read-port trios see them.
        for addr, data in [(0, 0xDEAD), (1, 0x1234)]:
            for wport in regfile.write_ports:
                yield wport.valid.eq(1)
                yield wport.bits.addr.eq(addr)
                yield wport.bits.data.eq(data)
            yield
        for wport in regfile.write_ports:
            yield wport.valid.eq(0)

        yield regread.dis_wid.eq(0)
        yield regread.dis_uop.lrs1.eq(0)
        yield regread.dis_uop.lrs1_rtype.eq(RegisterType.FIX)
        yield regread.dis_uop.lrs2.eq(1)
        yield regread.dis_uop.lrs2_rtype.eq(RegisterType.FIX)
        yield regread.dis_uop.lrs3.eq(0)
        yield regread.dis_uop.lrs3_rtype.eq(RegisterType.FIX)
        yield regread.dis_valid.eq(1)
        yield
        yield regread.dis_valid.eq(0)

        for _ in range(8):
            if (yield regread.exec_req.valid):
                break
            yield
        else:
            assert False, 'register read produced no request'

        for t in range(4):
            assert (yield regread.exec_req.bits.rs1_data[t]) == 0, 'x0 rs1'
            assert (yield regread.exec_req.bits.rs2_data[t]) == 0x1234
            assert (yield regread.exec_req.bits.rs3_data[t]) == 0, \
                'x0 as rs3 must read as zero'

    run_test(dut, proc, sync=True)


@pytest.mark.parametrize('src_lane', range(4))
def test_decode_wgather(src_lane):
    dut = DecodeUnit(TEST_PARAMS)
    rd, rs1, rs2, rs3 = 3, 4, 5, 6

    def proc():
        yield dut.in_uop.inst.eq(wgather_inst(rd, rs1, rs2, rs3, src_lane))
        yield Settle()

        assert (yield dut.out_uop.opcode) == UOpCode.GPU_WGATHER
        assert (yield dut.out_uop.iq_type) == IssueQueueType.INT
        assert (yield dut.out_uop.fu_type) == FUType.WGATHER
        assert (yield dut.out_uop.ldst) == rd
        assert (yield dut.out_uop.lrs1) == rs1
        assert (yield dut.out_uop.lrs2) == rs2
        assert (yield dut.out_uop.lrs3) == rs3
        assert (yield dut.out_uop.ldst_valid)
        assert (yield dut.out_uop.dst_rtype) == RegisterType.FIX
        assert (yield dut.out_uop.lrs1_rtype) == RegisterType.FIX
        assert (yield dut.out_uop.lrs2_rtype) == RegisterType.FIX
        assert (yield dut.out_uop.lrs3_rtype) == RegisterType.FIX
        assert (yield dut.out_uop.imm_packed[13:15]) == src_lane
        assert not (yield dut.out_uop.stall_warp)
        assert not (yield dut.out_uop.exception)

    run_test(dut, proc)


@pytest.mark.parametrize('funct3', range(1, 8))
def test_decode_wgather_illegal_funct3(funct3):
    dut = DecodeUnit(TEST_PARAMS)

    def proc():
        yield dut.in_uop.inst.eq(wgather_inst(3, 4, 5, 6, 0, funct3))
        yield Settle()

        assert (yield dut.out_uop.exception)
        assert (yield dut.out_uop.exc_cause) == Cause.ILLEGAL_INSTRUCTION

    run_test(dut, proc)


CORE_PARAMS = {
    **TEST_PARAMS,
    'use_fpu':
    False,
    'use_async_copy':
    False,
    'icache_params':
    dict(n_sets=8, n_ways=2, block_bytes=64),
    'dcache_params':
    dict(n_sets=8,
         n_ways=2,
         block_bytes=64,
         row_bits=64,
         n_mshrs=2,
         n_iomshrs=2,
         sdq_size=4,
         rpq_size=4,
         n_banks=2),
    'smem_params':
    dict(base=0x20000, size=0x4000, n_banks=4),
}

# Warp 0 boots with only lane 0 active (WarpScheduler thread_masks reset),
# so the program first activates the full warp with gpu_tmc and then builds
# per-lane-distinct operands from wtid. Gather destinations are initialized
# with nonzero sentinels; the first gather runs under the full mask, the
# second under tmask=0b1011 (source lane inactive: last-active-lane
# fallback), the third under tmask=0b0111 (inactive non-source lane 3 must
# be force-written). Reactivating and copying x7..x9 through addi reads the
# register file back through real instructions, so write gating (source
# lane retention) is verified, not just the writeback stream. The NOP run
# pushes the gathers and readbacks past the first 64-byte icache line,
# exercising multi-line fetch.
CORE_PROGRAM = [
    addi(1, 0, 0xf),
    gpu_tmc(rs2=1),
    csrrs(2, gpucsrnames.wtid),
    slli(3, 2, 8),
    addi(4, 3, 0x21),
    addi(5, 3, 0x32),
    addi(6, 3, 0x55),
    addi(7, 3, 0x77),
    addi(8, 3, 0x88),
    addi(9, 3, 0x99),
] + [nop()] * 8 + [
    wgather(7, 4, 5, 6, 2),
    addi(1, 0, 0xb),
    gpu_tmc(rs2=1),
    wgather(8, 4, 5, 6, 2),
    addi(1, 0, 0x7),
    gpu_tmc(rs2=1),
    wgather(9, 4, 5, 6, 2),
    addi(1, 0, 0xf),
    gpu_tmc(rs2=1),
    addi(10, 7, 0),
    addi(11, 8, 0),
    addi(12, 9, 0),
    jal(0),
]

# (ldst, expected per-lane data, expected writeback tmask)
CORE_EXPECTED = [
    (1, [0xf] * 4, 0b0001),
    (2, [0, 1, 2, 3], 0b1111),
    (3, [0x000, 0x100, 0x200, 0x300], 0b1111),
    (4, [0x21, 0x121, 0x221, 0x321], 0b1111),
    (5, [0x32, 0x132, 0x232, 0x332], 0b1111),
    (6, [0x55, 0x155, 0x255, 0x355], 0b1111),
    (7, [0x77, 0x177, 0x277, 0x377], 0b1111),
    (8, [0x88, 0x188, 0x288, 0x388], 0b1111),
    (9, [0x99, 0x199, 0x299, 0x399], 0b1111),
    # full tmask, src lane 2: [rs2[2], rs3[2], suppressed, rs1[2]]
    (7, [0x232, 0x255, 0, 0x221], 0b1011),
    # tmask 0b1011, src lane 2 inactive -> gathers from lane 3
    (8, [0x332, 0x355, 0, 0x321], 0b1011),
    # tmask 0b0111, lane 3 inactive but force-written
    (9, [0x232, 0x255, 0, 0x221], 0b1011),
    # readbacks: register-file contents must keep the source lane's
    # sentinel and hold the forced write on previously inactive lane 3
    (10, [0x232, 0x255, 0x277, 0x221], 0b1111),
    (11, [0x332, 0x355, 0x288, 0x321], 0b1111),
    (12, [0x232, 0x255, 0x299, 0x221], 0b1111),
]


def test_wgather_core():
    tb = GroomCoreSim(CORE_PARAMS, CORE_PROGRAM)
    events = []

    def script():
        for _ in range(3000):
            yield

    run_core_sim(tb, script, wb_monitor(tb.core, events))

    by_ldst = {}
    for pos, event in enumerate(events):
        by_ldst.setdefault(event['ldst'], []).append((pos, event))

    seen = {}
    for ldst, want_data, want_tmask in CORE_EXPECTED:
        writes = by_ldst.get(ldst, [])
        idx = seen.get(ldst, 0)
        seen[ldst] = idx + 1
        assert len(writes) > idx, \
            f'no writeback #{idx} for x{ldst}: {events}'
        _, event = writes[idx]
        assert event['data'] == want_data, \
            f'x{ldst}#{idx}: {event["data"]!r} != {want_data!r}'
        assert event['tmask'] == want_tmask, \
            f'x{ldst}#{idx}: tmask {event["tmask"]:#06b} != {want_tmask:#06b}'

    # Observed writeback order (event positions, not uop identity). The
    # gathers are the *second* write to x7..x9 — the first is the sentinel
    # addi — so compare the gather entries ([1], whose existence the data
    # loop above already asserts) against the readbacks' single events.
    gather_pos = [by_ldst[l][1][0] for l in (7, 8, 9)]
    assert gather_pos == sorted(gather_pos)
    for (gather, readback), pos in zip(((7, 10), (8, 11), (9, 12)),
                                       gather_pos):
        assert pos < by_ldst[readback][0][0], \
            f'x{readback} readback must retire after the x{gather} gather'
