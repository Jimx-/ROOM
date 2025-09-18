from amaranth.sim import Settle
import pytest

from room.utils import PopCount, FindFirstSet
from room.test import run_test


@pytest.mark.parametrize("n", [0, 1, 7, 8])
def test_popcount(n):
    dut = PopCount(n)

    def proc():
        xs = range(2**n)

        for x in xs:
            yield dut.inp.eq(x)
            yield Settle()
            out = yield dut.out
            assert out == x.bit_count()

    run_test(dut, proc)


def ffs(x):
    return (x & -x).bit_length() - 1


@pytest.mark.parametrize("n", [1, 2, 4, 8])
def test_ffs(n):
    dut = FindFirstSet(n)

    def proc():
        xs = range(1, 2**n)

        for x in xs:
            yield dut.inp.eq(x)
            yield Settle()
            out = yield dut.out
            assert out == ffs(x)

    run_test(dut, proc)
