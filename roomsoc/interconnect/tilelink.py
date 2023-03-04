from enum import Enum
from amaranth import *
from amaranth import tracer
from amaranth.hdl.rec import Direction
from amaranth.utils import log2_int
from amaranth_soc.memory import MemoryMap

from room.utils import Decoupled


class ChannelAOpcode(Enum):
    PutFullData = 0
    PutPartialData = 1
    ArithmeticData = 2
    LogicalData = 3
    Get = 4
    Intent = 5
    Acquire = 6


class ChannelBOpcode(Enum):
    PutFullData = 0
    PutPartialData = 1
    ArithmeticData = 2
    LogicalData = 3
    Get = 4
    Intent = 5
    Probe = 6


class ChannelCOpcode(Enum):
    AccessAck = 0
    AccessAckData = 1
    HintAck = 2
    ProbeAck = 4
    ProbeAckData = 5
    Release = 6
    ReleaseData = 7


class ChannelDOpcode(Enum):
    AccessAck = 0
    AccessAckData = 1
    HintAck = 2
    Grant = 4
    GrantData = 5
    ReleaseAck = 6


class ChannelA(Record):

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 size_width,
                 source_id_width=1,
                 name=None):

        layout = [
            ('opcode', ChannelAOpcode, Direction.FANOUT),
            ('param', 3, Direction.FANOUT),
            ('size', size_width, Direction.FANOUT),
            ('source', source_id_width, Direction.FANOUT),
            ('address', addr_width, Direction.FANOUT),
            ('mask', data_width // 8, Direction.FANOUT),
            ('data', data_width, Direction.FANOUT),
            ('corrupt', 1, Direction.FANOUT),
        ]

        super().__init__(layout, name=name, src_loc_at=1)


class ChannelB(Record):

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 size_width,
                 source_id_width=1,
                 name=None):

        layout = [
            ('opcode', ChannelBOpcode, Direction.FANOUT),
            ('param', 3, Direction.FANOUT),
            ('size', size_width, Direction.FANOUT),
            ('source', source_id_width, Direction.FANOUT),
            ('address', addr_width, Direction.FANOUT),
            ('mask', data_width // 8, Direction.FANOUT),
            ('data', data_width, Direction.FANOUT),
        ]

        super().__init__(layout, name=name, src_loc_at=1)


class ChannelC(Record):

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 size_width,
                 source_id_width=1,
                 name=None):

        layout = [
            ('opcode', ChannelCOpcode, Direction.FANOUT),
            ('param', 3, Direction.FANOUT),
            ('size', size_width, Direction.FANOUT),
            ('source', source_id_width, Direction.FANOUT),
            ('address', addr_width, Direction.FANOUT),
            ('data', data_width, Direction.FANOUT),
            ('error', 1, Direction.FANOUT),
        ]

        super().__init__(layout, name=name, src_loc_at=1)


class ChannelD(Record):

    def __init__(self,
                 *,
                 data_width,
                 size_width,
                 source_id_width=1,
                 sink_id_width=1,
                 name=None):

        layout = [
            ('opcode', ChannelDOpcode, Direction.FANOUT),
            ('param', 2, Direction.FANOUT),
            ('size', size_width, Direction.FANOUT),
            ('source', source_id_width, Direction.FANOUT),
            ('sink', sink_id_width, Direction.FANOUT),
            ('denied', 1, Direction.FANOUT),
            ('corrupt', 1, Direction.FANOUT),
            ('data', data_width, Direction.FANOUT),
        ]

        super().__init__(layout, name=name, src_loc_at=1)


class ChannelE(Record):

    def __init__(self, *, sink_id_width=1, name=None):

        layout = [
            ('sink', sink_id_width, Direction.FANOUT),
        ]

        super().__init__(layout, name=name, src_loc_at=1)


class Interface:

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 size_width=1,
                 source_id_width=1,
                 sink_id_width=1,
                 has_bce=False,
                 name=None,
                 src_loc_at=0):
        if name is not None:
            self.name = str(name)
        else:
            self.name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.addr_width = addr_width
        self.data_width = data_width
        self.source_id_width = source_id_width
        self.sink_id_width = sink_id_width
        self.size_bits = size_width
        self.has_bce = has_bce

        self.a = Decoupled(ChannelA,
                           addr_width=addr_width,
                           data_width=data_width,
                           size_width=size_width,
                           source_id_width=source_id_width,
                           name=f'{self.name}_a')

        self.d = Decoupled(ChannelD,
                           data_width=data_width,
                           size_width=size_width,
                           source_id_width=source_id_width,
                           sink_id_width=sink_id_width,
                           name=f'{self.name}_d')

        if has_bce:
            self.b = Decoupled(ChannelB,
                               addr_width=addr_width,
                               data_width=data_width,
                               size_width=size_width,
                               source_id_width=source_id_width,
                               name=f'{self.name}_b')

            self.c = Decoupled(ChannelC,
                               addr_width=addr_width,
                               data_width=data_width,
                               size_width=size_width,
                               source_id_width=source_id_width,
                               name=f'{self.name}_c')

            self.e = Decoupled(ChannelE,
                               sink_id_width=sink_id_width,
                               name=f'{self.name}_e')

    def connect(self, subord):
        return [
            self.a.connect(subord.a),
            subord.d.connect(self.d),
        ] + ([
            subord.b.connect(self.b),
            self.c.connect(subord.c),
            self.e.connect(subord.e)
        ] if self.has_bce and subord.has_bce else [])

    @staticmethod
    def has_data(bits):
        if isinstance(bits, ChannelA):
            return (bits.opcode == ChannelAOpcode.PutFullData) | (
                bits.opcode == ChannelAOpcode.PutPartialData)
        elif isinstance(bits, ChannelD):
            return bits.opcode == ChannelDOpcode.AccessAckData

        return False

    @property
    def memory_map(self):
        if self._map is None:
            raise NotImplementedError(
                "Bus interface {!r} does not have a memory map".format(self))
        return self._map

    @memory_map.setter
    def memory_map(self, memory_map):
        if not isinstance(memory_map, MemoryMap):
            raise TypeError(
                "Memory map must be an instance of MemoryMap, not {!r}".format(
                    memory_map))
        if memory_map.data_width != 8:
            raise ValueError(
                "Memory map has data width {}, which is not the same as bus "
                "interface granularity {}".format(memory_map.data_width, 8))
        if memory_map.addr_width != max(1, self.addr_width):
            raise ValueError(
                "Memory map has address width {}, which is not the same as bus "
                "interface address width {}".format(memory_map.addr_width,
                                                    self.addr_width))
        memory_map.freeze()
        self._map = memory_map

    def tilelink_get(self, address, size, mask, source=0):
        return [
            self.a.bits.opcode.eq(ChannelAOpcode.Get),
            self.a.bits.param.eq(0),
            self.a.bits.size.eq(size),
            self.a.bits.source.eq(source),
            self.a.bits.address.eq(address),
            self.a.bits.mask.eq(mask),
            self.a.bits.corrupt.eq(0),
        ]

    def tilelink_put_full_data(self, address, data, size, mask, source=0):
        return [
            self.a.bits.opcode.eq(ChannelAOpcode.PutFullData),
            self.a.bits.param.eq(0),
            self.a.bits.size.eq(size),
            self.a.bits.source.eq(source),
            self.a.bits.address.eq(address),
            self.a.bits.mask.eq(mask),
            self.a.bits.data.eq(data),
            self.a.bits.corrupt.eq(0),
        ]

    def tilelink_access_ack(self, size, source=0, sink=0):
        return [
            self.d.bits.opcode.eq(ChannelDOpcode.AccessAck),
            self.d.bits.param.eq(0),
            self.d.bits.size.eq(size),
            self.d.bits.source.eq(source),
            self.d.bits.sink.eq(sink),
            self.d.bits.denied.eq(0),
            self.d.bits.corrupt.eq(0),
        ]

    def tilelink_access_ack_data(self,
                                 data,
                                 size,
                                 source=0,
                                 sink=0,
                                 corrupt=0):
        return [
            self.d.bits.opcode.eq(ChannelDOpcode.AccessAckData),
            self.d.bits.param.eq(0),
            self.d.bits.size.eq(size),
            self.d.bits.source.eq(source),
            self.d.bits.sink.eq(sink),
            self.d.bits.denied.eq(0),
            self.d.bits.corrupt.eq(corrupt),
            self.d.bits.data.eq(data),
        ]


class TileLink2Wishbone(Elaboratable):

    def __init__(self, tl, wishbone, base_addr=0x00000000):
        self.base_addr = base_addr

        self.tl = tl
        self.wishbone = wishbone

    def elaborate(self, platform):
        m = Module()

        tl = self.tl
        wb = self.wishbone

        wb_adr_shift = log2_int(tl.data_width // 8)

        wb_addr = Signal(wb.addr_width)
        burst_len = Signal(2**tl.size_bits + 1)
        mask = Signal.like(tl.a.bits.mask)
        wen = Signal()
        rdata = Signal.like(wb.dat_r)

        with m.FSM():
            with m.State('IDLE'):
                with m.If(tl.a.valid):
                    is_write = Interface.has_data(tl.a.bits)

                    m.d.sync += [
                        wb_addr.eq((tl.a.bits.address -
                                    self.base_addr)[wb_adr_shift:]),
                        burst_len.eq(1 << tl.a.bits.size),
                        mask.eq(tl.a.bits.mask),
                        wen.eq(is_write),
                        tl.d.bits.size.eq(tl.a.bits.size),
                        tl.d.bits.source.eq(tl.a.bits.source),
                    ]

                    m.d.comb += tl.a.ready.eq(~is_write)

                    m.next = 'ACT'

            with m.State('ACT'):
                m.d.comb += [
                    wb.stb.eq(~wen | tl.a.valid),
                    wb.cyc.eq(~wen | tl.a.valid),
                    wb.adr.eq(wb_addr),
                    wb.we.eq(wen),
                    wb.sel.eq(mask),
                ]

                with m.If(wb.we):
                    m.d.comb += [
                        wb.dat_w.eq(tl.a.bits.data),
                        tl.a.ready.eq(wb.ack),
                    ]

                with m.If(wb.ack):
                    m.d.sync += wb_addr.eq(wb_addr + 1)

                    with m.If(~wb.we):
                        m.d.comb += [
                            tl.d.bits.opcode.eq(ChannelDOpcode.AccessAckData),
                            tl.d.bits.data.eq(wb.dat_r),
                            tl.d.valid.eq(1),
                        ]

                        with m.If(tl.d.ready):
                            with m.If(burst_len > tl.data_width // 8):
                                m.d.sync += burst_len.eq(burst_len -
                                                         tl.data_width // 8)
                            with m.Else():
                                m.d.sync += burst_len.eq(0)

                                m.next = 'IDLE'
                        with m.Else():
                            m.d.sync += rdata.eq(wb.dat_r)

                            m.next = 'WAIT_ACK'

                    with m.Else():
                        with m.If(burst_len > tl.data_width // 8):
                            m.d.sync += burst_len.eq(burst_len -
                                                     tl.data_width // 8)
                        with m.Else():
                            m.d.sync += burst_len.eq(0)

                            m.next = 'WRITE_ACK'

            with m.State('WAIT_ACK'):
                m.d.comb += [
                    tl.d.bits.opcode.eq(ChannelDOpcode.AccessAckData),
                    tl.d.bits.data.eq(rdata),
                    tl.d.valid.eq(1),
                ]

                with m.If(tl.d.ready):
                    with m.If(burst_len > tl.data_width // 8):
                        m.d.sync += burst_len.eq(burst_len -
                                                 tl.data_width // 8)
                    with m.Else():
                        m.d.sync += burst_len.eq(0)

                        m.next = 'IDLE'

            with m.State('WRITE_ACK'):
                m.d.comb += [
                    tl.d.bits.opcode.eq(ChannelDOpcode.AccessAck),
                    tl.d.bits.data.eq(rdata),
                    tl.d.valid.eq(1),
                ]

                with m.If(tl.d.ready):
                    m.next = 'IDLE'

        return m


class Arbiter(Elaboratable):

    def __init__(self, cls, *args, data_width, size_width, **kwargs):
        self.data_width = data_width
        self.size_width = size_width

        self.bus = Decoupled(cls,
                             data_width=data_width,
                             size_width=size_width,
                             *args,
                             **kwargs)
        self._intrs = []

    def add(self, intr_bus):
        self._intrs.append(intr_bus)

    def elaborate(self, platform):
        m = Module()

        requests = Signal(len(self._intrs))
        grant = Signal(range(len(self._intrs)))
        m.d.comb += requests.eq(Cat(intr_bus.valid
                                    for intr_bus in self._intrs))

        beats_left = Signal(2**self.size_width + 1)
        lg_beat_bytes = log2_int(self.data_width // 8)

        bus_busy = beats_left != 0

        def num_beats(bits):
            if isinstance(bits, ChannelE):
                return 1
            return Mux(Interface.has_data(bits),
                       ((1 << bits.size) >> lg_beat_bytes) |
                       (bits.size < lg_beat_bytes), 1)

        with m.If(~bus_busy):
            with m.Switch(grant):
                for i in range(len(requests)):
                    with m.Case(i):
                        for pred in reversed(range(i)):
                            with m.If(requests[pred]):
                                m.d.sync += [
                                    grant.eq(pred),
                                    beats_left.eq(
                                        num_beats(self._intrs[pred].bits)),
                                ]
                        for succ in reversed(range(i + 1, len(requests))):
                            with m.If(requests[succ]):
                                m.d.sync += [
                                    grant.eq(succ),
                                    beats_left.eq(
                                        num_beats(self._intrs[succ].bits)),
                                ]

                        with m.If(requests[i]):
                            m.d.sync += beats_left.eq(
                                num_beats(self._intrs[i].bits))

        with m.If(bus_busy):
            m.d.sync += beats_left.eq(beats_left - self.bus.fire)

            with m.Switch(grant):
                for i, intr_bus in enumerate(self._intrs):
                    with m.Case(i):
                        m.d.comb += [
                            self.bus.bits.eq(intr_bus.bits),
                            self.bus.valid.eq(intr_bus.valid),
                            intr_bus.ready.eq(self.bus.ready),
                        ]

        return m
