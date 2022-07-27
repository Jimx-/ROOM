from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT


def make_obi_layout(data_width=32, addr_width=32):
    be_width = data_width // 8
    return [
        ("req", 1, DIR_FANOUT),
        ("gnt", 1, DIR_FANIN),
        ("addr", addr_width, DIR_FANOUT),
        ("we", 1, DIR_FANOUT),
        ("be", be_width, DIR_FANOUT),
        ("wdata", data_width, DIR_FANOUT),
        ("rvalid", 1, DIR_FANIN),
        ("rdata", data_width, DIR_FANIN),
    ]


class OBI(Record):

    def __init__(self, data_width=32, addr_width=32, name=None):
        self.addr_width = addr_width
        self.data_width = data_width
        super().__init__(make_obi_layout(data_width=data_width,
                                         addr_width=addr_width),
                         name=name)


def make_axi_layout(data_width=32, addr_width=32, id_width=12):
    wstrb_width = data_width // 8

    return [
        # write address channel signals
        (
            "aw",
            [
                ("id", id_width, DIR_FANOUT),  # write address ID
                ("addr", addr_width, DIR_FANOUT),  # write address
                ("len", 8, DIR_FANOUT),  # burst length
                ("size", 3, DIR_FANOUT),  # burst size
                ("burst", 2, DIR_FANOUT),  # burst type
                ("lock", 2, DIR_FANOUT),  # lock type
                ("cache", 4, DIR_FANOUT),  # memory type
                ("prot", 3, DIR_FANOUT),  # protection type
                ("qos", 4, DIR_FANOUT),  # QoS
                ("valid", 1, DIR_FANOUT),  # write address valid
                ("ready", 1, DIR_FANIN),  # write address ready
            ]),
        # write data channel signals
        (
            "w",
            [
                ("id", id_width, DIR_FANOUT),  # write ID tag
                ("data", data_width, DIR_FANOUT),  # write data
                ("strb", wstrb_width, DIR_FANOUT),  # write strobes
                ("last", 1, DIR_FANOUT),  # write last
                ("valid", 1, DIR_FANOUT),  # write valid
                ("ready", 1, DIR_FANIN),  # write ready
            ]),
        # write response channel signals
        (
            "b",
            [
                ("id", id_width, DIR_FANIN),  # response ID tag
                ("resp", 2, DIR_FANIN),  # write response
                ("valid", 1, DIR_FANIN),  # write response valid
                ("ready", 1, DIR_FANOUT),  # response ready
            ]),
        # read address channel signals
        (
            "ar",
            [
                ("id", id_width, DIR_FANOUT),  # read address ID
                ("addr", addr_width, DIR_FANOUT),  # read address
                ("len", 8, DIR_FANOUT),  # burst length
                ("size", 3, DIR_FANOUT),  # burst size
                ("burst", 2, DIR_FANOUT),  # burst type
                ("lock", 2, DIR_FANOUT),  # lock type
                ("cache", 4, DIR_FANOUT),  # memory type
                ("prot", 3, DIR_FANOUT),  # protection type
                ("qos", 4, DIR_FANOUT),  # QoS
                ("valid", 1, DIR_FANOUT),  # read address valid
                ("ready", 1, DIR_FANIN),  # read address ready
            ]),
        # read data channel signals
        (
            "r",
            [
                ("id", id_width, DIR_FANIN),  # read ID tag
                ("data", data_width, DIR_FANIN),  # read data
                ("resp", 2, DIR_FANIN),  # read response
                ("last", 1, DIR_FANIN),  # read last
                ("valid", 1, DIR_FANIN),  # read valid
                ("ready", 1, DIR_FANOUT),  # read ready
            ]),
    ]


class AXI(Record):

    def __init__(self, data_width=32, addr_width=32, id_width=12, name=None):
        self.addr_width = addr_width
        self.data_width = data_width
        self.id_width = id_width
        super().__init__(make_axi_layout(data_width=data_width,
                                         addr_width=addr_width,
                                         id_width=id_width),
                         name=name)
