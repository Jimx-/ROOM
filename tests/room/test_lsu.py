from amaranth.sim import Settle
from amaranth.utils import log2_int
import pytest

from room.lsu import LoadGen, StoreGen
from room.test import run_test


def load_gen_unittest(dut, typ, addr, data_in, data_expected):

    def proc():
        yield dut.typ.eq(typ)
        yield dut.addr.eq(addr)
        yield dut.data_in.eq(data_in)
        yield Settle()

        data_out = yield dut.data_out
        assert data_out == data_expected

    return proc


@pytest.mark.parametrize("xlen", [32, 64])
def test_load_gen(xlen):
    dut = LoadGen(xlen // 8)

    log_max_size = log2_int(xlen // 8)
    data = [0x5a, 0x55aa, 0x55aaaa55, 0x55aaaa5555aaaa55]

    for i in range(log_max_size + 1):
        size = 2**i

        for addr in range(0, xlen // 8, size):
            run_test(
                dut,
                load_gen_unittest(dut, i, addr, data[i] << (addr * 8),
                                  data[i]))


def store_gen_unittest(dut, typ, addr, data_in, shift):

    def proc():
        yield dut.typ.eq(typ)
        yield dut.addr.eq(addr)
        yield dut.data_in.eq(data_in)
        yield Settle()

        data_out = yield dut.data_out
        mask = yield dut.mask

        bit_mask = 0
        for i in range(len(dut.mask)):
            if mask & (1 << i):
                bit_mask |= 0xff << (i << 3)

        assert (data_out & bit_mask) >> shift == data_in

    return proc


@pytest.mark.parametrize("xlen", [32, 64])
def test_store_gen(xlen):
    dut = StoreGen(xlen // 8)

    log_max_size = log2_int(xlen // 8)
    data = [0x5a, 0x55aa, 0x55aaaa55, 0x55aaaa5555aaaa55]

    for i in range(log_max_size + 1):
        size = 2**i

        for addr in range(0, xlen // 8, size):
            run_test(dut, store_gen_unittest(dut, i, addr, data[i], addr * 8))
