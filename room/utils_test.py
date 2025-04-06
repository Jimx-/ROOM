from amaranth.sim import Settle
import pytest

from room.utils import PopCount
from room.test import run_test


@pytest.mark.parametrize("n", [0, 1, 7, 8])
def test_popcount(n):
    dut = PopCount(n)

    def proc():
        xs = range(2**n)

        for x in xs:
            print(hex(x))
            yield dut.inp.eq(x)
            yield Settle()
            out = yield dut.out
            assert out == x.bit_count()

    run_test(dut, proc)
