from enum import IntEnum
from amaranth import *
from amaranth.hdl.rec import Direction

from room.utils import Arbiter

from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer, AXIStreamPacketizer
from roomsoc.interconnect.stream import Decoupled, Valid, Queue

_transports = {
    'RC': 0x00,
    'UC': 0x20,
    'RD': 0x40,
    'UD': 0x60,
}

_opcodes = {
    'SEND_FIRST': 0x00,
    'SEND_MIDDLE': 0x01,
    'SEND_LAST': 0x02,
    'SEND_LAST_WITH_IMMEDIATE': 0x03,
    'SEND_ONLY': 0x04,
    'SEND_ONLY_WITH_IMMEDIATE': 0x05,
    'RDMA_WRITE_FIRST': 0x06,
    'RDMA_WRITE_MIDDLE': 0x07,
    'RDMA_WRITE_LAST': 0x08,
    'RDMA_WRITE_LAST_WITH_IMMEDIATE': 0x09,
    'RDMA_WRITE_ONLY': 0x0a,
    'RDMA_WRITE_ONLY_WITH_IMMEDIATE': 0x0b,
    'RDMA_READ_REQUEST': 0x0c,
    'RDMA_READ_RESPONSE_FIRST': 0x0d,
    'RDMA_READ_RESPONSE_MIDDLE': 0x0e,
    'RDMA_READ_RESPONSE_LAST': 0x0f,
    'RDMA_READ_RESPONSE_ONLY': 0x10,
    'ACKNOWLEDGE': 0x11,
    'ATOMIC_ACKNOWLEDGE': 0x12,
    'COMPARE_SWAP': 0x13,
    'FETCH_ADD': 0x14,
}


def opcode(transport, op):
    return (
        '{}_{}'.format(transport, op),
        _transports[transport] + _opcodes[op],
    )


_bth_opcodes = dict([
    opcode('RC', 'SEND_FIRST'),
    opcode('RC', 'SEND_MIDDLE'),
    opcode('RC', 'SEND_LAST'),
    opcode('RC', 'SEND_LAST_WITH_IMMEDIATE'),
    opcode('RC', 'SEND_ONLY'),
    opcode('RC', 'SEND_ONLY_WITH_IMMEDIATE'),
    opcode('RC', 'RDMA_WRITE_FIRST'),
    opcode('RC', 'RDMA_WRITE_MIDDLE'),
    opcode('RC', 'RDMA_WRITE_LAST'),
    opcode('RC', 'RDMA_WRITE_LAST_WITH_IMMEDIATE'),
    opcode('RC', 'RDMA_WRITE_ONLY'),
    opcode('RC', 'RDMA_WRITE_ONLY_WITH_IMMEDIATE'),
    opcode('RC', 'RDMA_READ_REQUEST'),
    opcode('RC', 'RDMA_READ_RESPONSE_FIRST'),
    opcode('RC', 'RDMA_READ_RESPONSE_MIDDLE'),
    opcode('RC', 'RDMA_READ_RESPONSE_LAST'),
    opcode('RC', 'RDMA_READ_RESPONSE_ONLY'),
    opcode('RC', 'ACKNOWLEDGE'),
    opcode('RC', 'ATOMIC_ACKNOWLEDGE'),
    opcode('RC', 'COMPARE_SWAP'),
    opcode('RC', 'FETCH_ADD'),
    ###
    opcode('UC', 'SEND_FIRST'),
    opcode('UC', 'SEND_MIDDLE'),
    opcode('UC', 'SEND_LAST'),
    opcode('UC', 'SEND_LAST_WITH_IMMEDIATE'),
    opcode('UC', 'SEND_ONLY'),
    opcode('UC', 'SEND_ONLY_WITH_IMMEDIATE'),
    opcode('UC', 'RDMA_WRITE_FIRST'),
    opcode('UC', 'RDMA_WRITE_MIDDLE'),
    opcode('UC', 'RDMA_WRITE_LAST'),
    opcode('UC', 'RDMA_WRITE_LAST_WITH_IMMEDIATE'),
    opcode('UC', 'RDMA_WRITE_ONLY'),
    opcode('UC', 'RDMA_WRITE_ONLY_WITH_IMMEDIATE'),
    ###
    opcode('RD', 'SEND_FIRST'),
    opcode('RD', 'SEND_MIDDLE'),
    opcode('RD', 'SEND_LAST'),
    opcode('RD', 'SEND_LAST_WITH_IMMEDIATE'),
    opcode('RD', 'SEND_ONLY'),
    opcode('RD', 'SEND_ONLY_WITH_IMMEDIATE'),
    opcode('RD', 'RDMA_WRITE_FIRST'),
    opcode('RD', 'RDMA_WRITE_MIDDLE'),
    opcode('RD', 'RDMA_WRITE_LAST'),
    opcode('RD', 'RDMA_WRITE_LAST_WITH_IMMEDIATE'),
    opcode('RD', 'RDMA_WRITE_ONLY'),
    opcode('RD', 'RDMA_WRITE_ONLY_WITH_IMMEDIATE'),
    opcode('RD', 'RDMA_READ_REQUEST'),
    opcode('RD', 'RDMA_READ_RESPONSE_FIRST'),
    opcode('RD', 'RDMA_READ_RESPONSE_MIDDLE'),
    opcode('RD', 'RDMA_READ_RESPONSE_LAST'),
    opcode('RD', 'RDMA_READ_RESPONSE_ONLY'),
    opcode('RD', 'ACKNOWLEDGE'),
    opcode('RD', 'ATOMIC_ACKNOWLEDGE'),
    opcode('RD', 'COMPARE_SWAP'),
    opcode('RD', 'FETCH_ADD'),
    ###
    opcode('UD', 'SEND_ONLY'),
    opcode('UD', 'SEND_ONLY_WITH_IMMEDIATE'),
])

BthOpcode = IntEnum("BthOpcode", _bth_opcodes)


def _opcode_is_reth(opcode):
    return (opcode == BthOpcode.RC_RDMA_READ_REQUEST) | (
        opcode == BthOpcode.RC_RDMA_WRITE_FIRST) | (
            opcode == BthOpcode.RC_RDMA_WRITE_ONLY)


def _opcode_is_aeth(opcode):
    return (opcode == BthOpcode.RC_RDMA_READ_RESPONSE_ONLY) | (
        opcode == BthOpcode.RC_RDMA_READ_RESPONSE_FIRST) | (
            opcode == BthOpcode.RC_RDMA_READ_RESPONSE_LAST) | (
                opcode == BthOpcode.RC_ACKNOWLEDGE)


def _opcode_no_data(opcode):
    return (opcode == BthOpcode.RC_RDMA_READ_REQUEST) | (
        opcode == BthOpcode.RC_ACKNOWLEDGE)


IB_BTH_LAYOUT = [
    ("opcode", 8, Direction.FANOUT),
    ("se", 1, Direction.FANOUT),
    ("m", 1, Direction.FANOUT),
    ("pad_cnt", 2, Direction.FANOUT),
    ("version", 4, Direction.FANOUT),
    ("partition", 16, Direction.FANOUT),
    ("_rsvd0", 8, Direction.FANOUT),
    ("dest_qp", 24, Direction.FANOUT),
    ("ack_req", 1, Direction.FANOUT),
    ("_rsvd1", 7, Direction.FANOUT),
    ("psn", 24, Direction.FANOUT),
]

IB_RETH_LAYOUT = [
    ("vaddr", 64, Direction.FANOUT),
    ("r_key", 32, Direction.FANOUT),
    ("dmalen", 32, Direction.FANOUT),
]

IB_AETH_LAYOUT = [
    ("syndrome", 8, Direction.FANOUT),
    ("msn", 24, Direction.FANOUT),
]


class _MemoryCommandInternal(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('opcode', 8, Direction.FANOUT),
            ('qpn', 24, Direction.FANOUT),
            ('addr', 64, Direction.FANOUT),
            ('len', 32, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class InfiniBandMetadata(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('opcode', 8, Direction.FANOUT),
            ('partition', 16, Direction.FANOUT),
            ('dest_qp', 24, Direction.FANOUT),
            ('psn', 24, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class InfiniBandDepacketizer(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.meta = Decoupled(InfiniBandMetadata)

        self.aeth = Decoupled(Record, IB_AETH_LAYOUT)
        self.reth = Decoupled(Record, IB_RETH_LAYOUT)

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        bth_depacketizer = m.submodules.bth_depacketizer = AXIStreamDepacketizer(
            Record, IB_BTH_LAYOUT, data_width=self.data_width)
        m.d.comb += self.data_in.connect(bth_depacketizer.sink)

        reth_depacketizer = m.submodules.reth_depacketizer = AXIStreamDepacketizer(
            Record, IB_RETH_LAYOUT, data_width=self.data_width)
        aeth_depacketizer = m.submodules.aeth_depacketizer = AXIStreamDepacketizer(
            Record, IB_AETH_LAYOUT, data_width=self.data_width)

        bth_header = bth_depacketizer.header

        meta_q = m.submodules.meta_q = Queue(4, InfiniBandMetadata)
        m.d.comb += [
            meta_q.enq.bits.opcode.eq(bth_header.opcode),
            meta_q.enq.bits.partition.eq(
                Cat(bth_header.partition[i * 8:(i + 1) * 8]
                    for i in range(len(bth_header.partition) // 8, -1, -1))),
            meta_q.enq.bits.dest_qp.eq(
                Cat(bth_header.dest_qp[i * 8:(i + 1) * 8]
                    for i in range(len(bth_header.dest_qp) // 8, -1, -1))),
            meta_q.enq.bits.psn.eq(
                Cat(bth_header.psn[i * 8:(i + 1) * 8]
                    for i in range(len(bth_header.psn) // 8, -1, -1))),
            meta_q.deq.connect(self.meta),
        ]

        eth_q = m.submodules.eth_q = Queue(
            4, Signal,
            max(len(self.aeth.bits), len(self.reth.bits)) + 1)

        with m.FSM():
            with m.State('IDLE'):
                with m.If(bth_depacketizer.source.valid):
                    m.d.comb += meta_q.enq.valid.eq(1)

                    with m.If(meta_q.enq.fire):
                        with m.If(_opcode_is_reth(bth_header.opcode)):
                            m.next = 'RETH_HEADER'
                        with m.Elif(_opcode_is_aeth(bth_header.opcode)):
                            m.next = 'AETH_HEADER'
                        with m.Else():
                            m.next = 'DROP'

            with m.State('RETH_HEADER'):
                m.d.comb += [
                    bth_depacketizer.source.connect(reth_depacketizer.sink),
                    eth_q.enq.bits.eq(
                        Cat(Const(0, 1), reth_depacketizer.header)),
                    eth_q.enq.valid.eq(reth_depacketizer.source.valid),
                ]

                with m.If(eth_q.enq.fire):
                    m.next = 'RETH_DATA'

            with m.State('RETH_DATA'):
                m.d.comb += bth_depacketizer.source.connect(
                    reth_depacketizer.sink)

                with m.If(bth_header.opcode == BthOpcode.RC_RDMA_READ_REQUEST):
                    m.d.comb += reth_depacketizer.source.ready.eq(1)
                with m.Else():
                    m.d.comb += reth_depacketizer.source.connect(self.data_out)

                with m.If(reth_depacketizer.source.fire
                          & reth_depacketizer.source.bits.last):
                    m.next = 'IDLE'

            with m.State('AETH_HEADER'):
                m.d.comb += [
                    bth_depacketizer.source.connect(aeth_depacketizer.sink),
                    eth_q.enq.bits.eq(
                        Cat(Const(1, 1), aeth_depacketizer.header)),
                    eth_q.enq.valid.eq(aeth_depacketizer.source.valid),
                ]

                with m.If(eth_q.enq.fire):
                    m.next = 'AETH_DATA'

            with m.State('AETH_DATA'):
                m.d.comb += aeth_depacketizer.source.ready.eq(1)

                with m.If(aeth_depacketizer.source.fire
                          & aeth_depacketizer.source.bits.last):
                    m.next = 'IDLE'

            with m.State('DROP'):
                m.d.comb += bth_depacketizer.source.ready.eq(1)

                with m.If(bth_depacketizer.source.fire
                          & bth_depacketizer.source.bits.last):
                    m.next = 'IDLE'

        reth = Record(IB_RETH_LAYOUT)
        aeth = Record(IB_AETH_LAYOUT)

        m.d.comb += [
            reth.eq(eth_q.deq.bits[1:]),
            aeth.eq(eth_q.deq.bits[1:]),
            self.aeth.bits.syndrome.eq(aeth.syndrome),
            self.aeth.bits.msn.eq(
                Cat(aeth.msn[i * 8:(i + 1) * 8]
                    for i in range(len(aeth.msn) // 8 - 1, -1, -1))),
            self.aeth.valid.eq(eth_q.deq.valid
                               & (eth_q.deq.bits[0] == 1)),
            self.reth.bits.vaddr.eq(
                Cat(reth.vaddr[i * 8:(i + 1) * 8]
                    for i in range(len(reth.vaddr) // 8 - 1, -1, -1))),
            self.reth.bits.r_key.eq(
                Cat(reth.r_key[i * 8:(i + 1) * 8]
                    for i in range(len(reth.r_key) // 8 - 1, -1, -1))),
            self.reth.bits.dmalen.eq(
                Cat(reth.dmalen[i * 8:(i + 1) * 8]
                    for i in range(len(reth.dmalen) // 8 - 1, -1, -1))),
            self.reth.valid.eq(eth_q.deq.valid
                               & (eth_q.deq.bits[0] == 0)),
            eth_q.deq.ready.eq(self.aeth.fire | self.reth.fire),
        ]

        return m


class StateTable(Elaboratable):

    class Entry(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('epsn', 24, Direction.FANOUT),
            ],
                             name=name,
                             src_loc_at=1 + src_loc_at)

    def __init__(self, max_qps):
        self.max_qps = max_qps

        self.read = [Decoupled(Signal, 16, name=f'read{i}') for i in range(1)]
        self.resp = [
            Valid(StateTable.Entry, name=f'resp{i}') for i in range(1)
        ]

    def elaborate(self, platform):
        m = Module()

        mem = Memory(width=len(self.resp[0].bits), depth=self.max_qps)
        mem_rport = m.submodules.mem_rport = mem.read_port(transparent=False)

        read_arbiter = m.submodules.read_arbiter = Arbiter(
            len(self.read), Signal, 16)
        for r, rr in zip(self.read, read_arbiter.inp):
            m.d.comb += r.connect(rr)

        m.d.comb += [
            mem_rport.addr.eq(read_arbiter.out.bits),
            read_arbiter.out.ready.eq(1),
        ]

        for r in self.resp:
            m.d.comb += r.bits.eq(mem_rport.data)
            m.d.sync += r.valid.eq(0)

        for r, rs in zip(self.read, self.resp):
            with m.If(r.fire):
                m.d.sync += rs.valid.eq(1)

        return m


class InfiniBandDropper(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.meta_in = Decoupled(InfiniBandMetadata)
        self.meta_out = Decoupled(InfiniBandMetadata)

        self.aeth_in = Decoupled(Record, IB_AETH_LAYOUT)
        self.reth_in = Decoupled(Record, IB_RETH_LAYOUT)
        self.aeth_out = Decoupled(Record, IB_AETH_LAYOUT)
        self.reth_out = Decoupled(Record, IB_RETH_LAYOUT)

        self.state_read = Decoupled(Signal, 16)
        self.state_resp = Valid(StateTable.Entry)

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        meta = InfiniBandMetadata()

        meta_q = m.submodules.meta_q = Queue(4, InfiniBandMetadata)
        m.d.comb += [
            meta_q.enq.bits.eq(meta),
            meta_q.deq.connect(self.meta_out),
        ]

        is_aeth = Signal()
        eth_q = m.submodules.eth_q = Queue(
            4, Signal,
            max(len(self.aeth_in.bits), len(self.reth_in.bits)) + 1)
        with m.If(is_aeth):
            m.d.comb += eth_q.enq.bits.eq(Cat(is_aeth, self.aeth_in.bits))
        with m.Else():
            m.d.comb += eth_q.enq.bits.eq(Cat(is_aeth, self.reth_in.bits))

        with m.FSM():
            with m.State('IDLE'):
                with m.If(self.meta_in.valid):
                    with m.If((_opcode_is_aeth(self.meta_in.bits.opcode)
                               & self.aeth_in.valid)
                              | (_opcode_is_reth(self.meta_in.bits.opcode)
                                 & self.reth_in.valid)):
                        m.d.comb += [
                            self.state_read.bits.eq(self.meta_in.bits.dest_qp),
                            self.state_read.valid.eq(1),
                        ]

                        with m.If(self.state_read.fire):
                            m.d.comb += self.meta_in.ready.eq(1)
                            m.d.sync += [
                                meta.eq(self.meta_in.bits),
                                is_aeth.eq(
                                    _opcode_is_aeth(self.meta_in.bits.opcode)),
                            ]

                            m.next = 'CHECK'

            with m.State('CHECK'):
                with m.If(self.state_resp.valid):
                    with m.If(self.state_resp.bits.epsn == meta.psn):
                        m.next = 'FORWARD_META'
                    with m.Else():
                        m.next = 'DROP_META'

            with m.State('FORWARD_META'):
                m.d.comb += [
                    meta_q.enq.valid.eq(eth_q.enq.ready),
                    eth_q.enq.valid.eq(meta_q.enq.ready),
                ]

                with m.If(meta_q.enq.fire & eth_q.enq.fire):
                    with m.If(is_aeth):
                        m.d.comb += self.aeth_in.ready.eq(1)
                    with m.Else():
                        m.d.comb += self.reth_in.ready.eq(1)

                    with m.If(_opcode_no_data(meta.opcode)):
                        m.next = 'IDLE'
                    with m.Else():
                        m.next = 'FORWARD_DATA'

            with m.State('FORWARD_DATA'):
                m.d.comb += self.data_in.connect(self.data_out)

                with m.If(self.data_out.fire & self.data_out.bits.last):
                    m.next = 'IDLE'

            with m.State('DROP_META'):
                with m.If(is_aeth):
                    m.d.comb += self.aeth_in.ready.eq(1)
                with m.Else():
                    m.d.comb += self.reth_in.ready.eq(1)

                with m.If(_opcode_no_data(meta.opcode)):
                    m.next = 'IDLE'
                with m.Else():
                    m.next = 'DROP_DATA'

            with m.State('DROP_DATA'):
                m.d.comb += self.data_in.ready.eq(1)

                with m.If(self.data_in.fire & self.data_in.bits.last):
                    m.next = 'IDLE'

        m.d.comb += [
            self.aeth_out.bits.eq(eth_q.deq.bits[1:]),
            self.reth_out.bits.eq(eth_q.deq.bits[1:]),
            self.aeth_out.valid.eq(eth_q.deq.valid & eth_q.deq.bits[0]),
            self.reth_out.valid.eq(eth_q.deq.valid & ~eth_q.deq.bits[0]),
            eth_q.deq.ready.eq(self.aeth_out.fire | self.reth_out.fire),
        ]

        return m


class RxHandler(Elaboratable):

    class ReadRequest(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('qpn', 24, Direction.FANOUT),
                ('vaddr', 64, Direction.FANOUT),
                ('dmalen', 32, Direction.FANOUT),
                ('psn', 24, Direction.FANOUT),
            ],
                             name=name,
                             src_loc_at=1 + src_loc_at)

    def __init__(self, data_width):
        self.data_width = data_width

        self.meta_in = Decoupled(InfiniBandMetadata)

        self.aeth_in = Decoupled(Record, IB_AETH_LAYOUT)
        self.reth_in = Decoupled(Record, IB_RETH_LAYOUT)

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

        self.read_req = Decoupled(RxHandler.ReadRequest)

    def elaborate(self, platform):
        m = Module()

        meta = InfiniBandMetadata()
        aeth = Record(IB_AETH_LAYOUT)
        reth = Record(IB_RETH_LAYOUT)

        with m.FSM():
            with m.State('IDLE'):
                with m.If(self.meta_in.valid):
                    with m.If((_opcode_is_aeth(self.meta_in.bits.opcode)
                               & self.aeth_in.valid)
                              | (_opcode_is_reth(self.meta_in.bits.opcode)
                                 & self.reth_in.valid)):
                        m.d.sync += meta.eq(self.meta_in.bits)
                        m.d.comb += self.meta_in.ready.eq(1)

                        with m.If(_opcode_is_aeth(self.meta_in.bits.opcode)):
                            m.d.sync += aeth.eq(self.aeth_in.bits)
                            m.d.comb += self.aeth_in.ready.eq(1)

                        with m.If(_opcode_is_reth(self.meta_in.bits.opcode)):
                            m.d.sync += reth.eq(self.reth_in.bits)
                            m.d.comb += self.reth_in.ready.eq(1)

                        m.next = 'PROCESS'

            with m.State('PROCESS'):
                with m.Switch(meta.opcode):
                    with m.Case(BthOpcode.RC_RDMA_READ_REQUEST):
                        m.d.comb += [
                            self.read_req.bits.qpn.eq(meta.dest_qp),
                            self.read_req.bits.vaddr.eq(reth.vaddr),
                            self.read_req.bits.dmalen.eq(reth.dmalen),
                            self.read_req.bits.psn.eq(meta.psn),
                            self.read_req.valid.eq(1),
                        ]

                        with m.If(self.read_req.fire):
                            m.next = 'IDLE'

                    with m.Default():
                        m.next = 'IDLE'

        return m


class ReadFragmenter(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.req = Decoupled(RxHandler.ReadRequest)
        self.mem_cmd = Decoupled(_MemoryCommandInternal)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += [
            self.mem_cmd.bits.opcode.eq(BthOpcode.RC_RDMA_READ_RESPONSE_ONLY),
            self.mem_cmd.bits.qpn.eq(self.req.bits.qpn),
            self.mem_cmd.bits.addr.eq(self.req.bits.vaddr),
            self.mem_cmd.bits.len.eq(self.req.bits.dmalen),
            self.mem_cmd.valid.eq(self.req.valid),
            self.req.ready.eq(self.mem_cmd.ready),
        ]

        return m


class InfiniBandTransportProtocol(Elaboratable):

    class MemoryCommand(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('addr', 64, Direction.FANOUT),
                ('len', 32, Direction.FANOUT),
            ],
                             name=name,
                             src_loc_at=1 + src_loc_at)

    def __init__(self, data_width, max_qps=64):
        self.data_width = data_width
        self.max_qps = max_qps

        self.rx_data_in = AXIStreamInterface(data_width=data_width)

        self.mem_read_cmd = Decoupled(
            InfiniBandTransportProtocol.MemoryCommand)
        self.mem_read_data = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        state_table = m.submodules.state_table = StateTable(
            max_qps=self.max_qps)

        depacketizer = m.submodules.depacketizer = InfiniBandDepacketizer(
            data_width=self.data_width)
        m.d.comb += self.rx_data_in.connect(depacketizer.data_in)

        dropper = m.submodules.dropper = InfiniBandDropper(
            data_width=self.data_width)
        m.d.comb += [
            depacketizer.meta.connect(dropper.meta_in),
            depacketizer.reth.connect(dropper.reth_in),
            depacketizer.aeth.connect(dropper.aeth_in),
            depacketizer.data_out.connect(dropper.data_in),
            dropper.state_read.connect(state_table.read[0]),
            dropper.state_resp.eq(state_table.resp[0]),
        ]

        rx_handler = m.submodules.rx_handler = RxHandler(
            data_width=self.data_width)
        m.d.comb += [
            dropper.meta_out.connect(rx_handler.meta_in),
            dropper.aeth_out.connect(rx_handler.aeth_in),
            dropper.reth_out.connect(rx_handler.reth_in),
            dropper.data_out.connect(rx_handler.data_in),
        ]

        read_fragmenter = m.submodules.read_fragmenter = ReadFragmenter(
            data_width=self.data_width)
        m.d.comb += [
            rx_handler.read_req.connect(read_fragmenter.req),
            self.mem_read_cmd.bits.addr.eq(read_fragmenter.mem_cmd.bits.addr),
            self.mem_read_cmd.bits.len.eq(read_fragmenter.mem_cmd.bits.len),
            self.mem_read_cmd.valid.eq(read_fragmenter.mem_cmd.valid),
            read_fragmenter.mem_cmd.ready.eq(self.mem_read_cmd.ready),
        ]

        m.d.comb += rx_handler.data_out.ready.eq(1)

        return m
