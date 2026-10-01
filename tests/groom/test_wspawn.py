import pytest

from amaranth import *
from amaranth.sim import Simulator

from room.types import HasCoreParams

from groom.id_stage import DecodeUnit
from groom.fu import GPUControlUnit

from tests.groom.encoding import gpu_wspawn

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


class WspawnDecodeExec(HasCoreParams, Elaboratable):
    """gpu_wspawn decode feeding GPUControlUnit, as in the core."""

    def __init__(self, params):
        super().__init__(params)
        self.decoder = DecodeUnit(params)
        self.fu = GPUControlUnit(params)

        self.inst = Signal(32)
        self.rs1_data = [
            Signal(self.xlen, name=f'rs1_data{t}')
            for t in range(self.n_threads)
        ]
        self.rs2_data = [
            Signal(self.xlen, name=f'rs2_data{t}')
            for t in range(self.n_threads)
        ]

    def elaborate(self, platform):
        m = Module()
        m.submodules.decoder = self.decoder
        m.submodules.fu = self.fu

        m.d.comb += [
            self.decoder.in_uop.inst.eq(self.inst),
            self.decoder.in_uop.tmask.eq((1 << self.n_threads) - 1),
            self.fu.req.valid.eq(1),
            self.fu.req.bits.wid.eq(0),
            self.fu.req.bits.uop.eq(self.decoder.out_uop),
        ]
        for fu_rs1, rs1 in zip(self.fu.req.bits.rs1_data, self.rs1_data):
            m.d.comb += fu_rs1.eq(rs1)
        for fu_rs2, rs2 in zip(self.fu.req.bits.rs2_data, self.rs2_data):
            m.d.comb += fu_rs2.eq(rs2)

        return m


# (rs1 reg, rs2 reg, imm, rs1 value, rs2 value, expected pc, expected mask)
# The zero-offset case mirrors the gpu_wspawn intrinsic exactly: with rs2
# in x11 the spawn pc must stay rs1, not rs1 + 11.
WSPAWN_CASES = [
    (5, 11, 0x000, 0x1000, 2, 0x1000, 0b0011),
    (5, 11, 0x123, 0x1000, 3, 0x1123, 0b0111),
    (6, 30, -0x4, 0x2000, 4, 0x1ffc, 0b1111),
]


@pytest.mark.parametrize('rs1,rs2,imm,rs1_val,rs2_val,want_pc,want_mask',
                         WSPAWN_CASES)
def test_wspawn_decode_to_exec(rs1, rs2, imm, rs1_val, rs2_val, want_pc,
                               want_mask):
    dut = WspawnDecodeExec(TEST_PARAMS)

    def proc():
        yield dut.inst.eq(gpu_wspawn(rs1, rs2, imm))
        for t in range(dut.n_threads):
            yield dut.rs1_data[t].eq(rs1_val)
            yield dut.rs2_data[t].eq(rs2_val)
        yield
        yield

        warp_ctrl = dut.fu.warp_ctrl
        assert (yield warp_ctrl.valid)
        assert (yield warp_ctrl.bits.wspawn.valid)
        assert (yield warp_ctrl.bits.wspawn.pc) == want_pc, \
            f'spawn pc must be rs1 + imm ({want_pc:#x})'
        assert (yield warp_ctrl.bits.wspawn.mask) == want_mask

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    sim.run()
