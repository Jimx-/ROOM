from amaranth import *
from amaranth.hdl.rec import Direction


def make_data_layout(data_width, user_width=1):
    return [
        ("data", data_width, Direction.FANOUT),
        ("keep", data_width // 8, Direction.FANOUT),
        ("user", user_width, Direction.FANOUT),
        ("last", 1, Direction.FANOUT),
    ]
