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
    AcquireBlock = 6
    AcquirePerm = 7


class CapParam(Enum):
    toT = 0
    toB = 1
    toN = 2


class GrowParam(Enum):
    NtoB = 0
    NtoT = 1
    BtoT = 2


class ShrinkReportParam(Enum):
    TtoB = 0
    TtoN = 1
    BtoN = 2
    TtoT = 3
    BtoB = 4
    NtoN = 5


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
                 name=None,
                 src_loc_at=1):

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

        super().__init__(layout, name=name, src_loc_at=src_loc_at)


class ChannelB(Record):

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 size_width,
                 source_id_width=1,
                 name=None,
                 src_loc_at=1):

        layout = [
            ('opcode', ChannelBOpcode, Direction.FANOUT),
            ('param', 3, Direction.FANOUT),
            ('size', size_width, Direction.FANOUT),
            ('source', source_id_width, Direction.FANOUT),
            ('address', addr_width, Direction.FANOUT),
            ('mask', data_width // 8, Direction.FANOUT),
            ('data', data_width, Direction.FANOUT),
        ]

        super().__init__(layout, name=name, src_loc_at=src_loc_at)


class ChannelC(Record):

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 size_width,
                 source_id_width=1,
                 name=None,
                 src_loc_at=1):

        layout = [
            ('opcode', ChannelCOpcode, Direction.FANOUT),
            ('param', 3, Direction.FANOUT),
            ('size', size_width, Direction.FANOUT),
            ('source', source_id_width, Direction.FANOUT),
            ('address', addr_width, Direction.FANOUT),
            ('data', data_width, Direction.FANOUT),
            ('corrupt', 1, Direction.FANOUT),
        ]

        super().__init__(layout, name=name, src_loc_at=src_loc_at)


class ChannelD(Record):

    def __init__(self,
                 *,
                 data_width,
                 size_width,
                 source_id_width=1,
                 sink_id_width=1,
                 name=None,
                 src_loc_at=1):

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

        super().__init__(layout, name=name, src_loc_at=src_loc_at)


class ChannelE(Record):

    def __init__(self, *, sink_id_width=1, name=None, src_loc_at=1):

        layout = [
            ('sink', sink_id_width, Direction.FANOUT),
        ]

        super().__init__(layout, name=name, src_loc_at=src_loc_at)


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
        self.size_width = size_width
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
        elif isinstance(bits, ChannelC):
            return (bits.opcode == ChannelCOpcode.ReleaseData) | (
                bits.opcode == ChannelCOpcode.ProbeAckData)
        elif isinstance(bits, ChannelD):
            return (bits.opcode == ChannelDOpcode.AccessAckData) | (
                bits.opcode == ChannelDOpcode.GrantData)

        return False

    @staticmethod
    def is_request(bits):
        if isinstance(bits, ChannelA):
            return True
        elif isinstance(bits, ChannelB):
            return True
        elif isinstance(bits, ChannelC):
            return bits.opcode[2] & bits.opcode[1]
        elif isinstance(bits, ChannelD):
            return bits.opcode[2] & ~bits.opcode[1]

        return False

    @staticmethod
    def num_beats0(bits):
        data_width = len(bits.data) if hasattr(bits, 'data') else 0
        beat_bytes = data_width // 8

        if isinstance(bits, ChannelE):
            return 0
        elif not hasattr(bits, 'size') or len(bits.size) == 0:
            return 0
        else:
            raw_size = 1 << bits.size
            return Mux(
                Interface.has_data(bits),
                Mux(raw_size < beat_bytes, 0,
                    (raw_size >> log2_int(beat_bytes)) - 1), 0)

    @staticmethod
    def count(m, bits, fire, name=None):
        if name is None:
            prefix = ''
        else:
            prefix = f'{name}_'

        data_width = len(bits.data) if hasattr(bits, 'data') else 0
        beat_bytes = data_width // 8

        beats = Interface.num_beats0(bits)
        counter = Signal(range(4**len(bits.size) //
                               beat_bytes) if hasattr(bits, 'size') else 1,
                         name=f'{prefix}counter')
        counter_next = counter - 1
        first = Signal(name=f'{prefix}first')
        last = Signal(name=f'{prefix}last')
        done = Signal(name=f'{prefix}done')
        count = Signal.like(counter, name=f'{prefix}count')

        m.d.comb += [
            first.eq(counter == 0),
            last.eq((counter == 1) | (beats == 0)),
            done.eq(last & fire),
            count.eq(beats & ~counter_next),
        ]

        with m.If(fire):
            m.d.sync += counter.eq(Mux(first, beats, counter_next))

        return first, last, done, count

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

        if tl.has_bce:
            tl_adapted = Interface(addr_width=tl.addr_width,
                                   data_width=tl.data_width,
                                   size_width=tl.size_width,
                                   source_id_width=tl.source_id_width,
                                   sink_id_width=tl.sink_id_width)
            m.submodules.cache_adapter = CacheCork(tl, tl_adapted)
            tl = tl_adapted

        wb_adr_shift = log2_int(tl.data_width // 8)

        wb_addr = Signal(wb.addr_width)
        burst_len = Signal(2**tl.size_width + 1)
        mask = Signal.like(tl.a.bits.mask)
        wen = Signal()
        rdata = Signal.like(wb.dat_r)

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += tl.a.ready.eq(1)

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
                    wb.cyc.eq(~wen | tl.a.valid),
                    wb.stb.eq(wb.cyc),
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
                    tl.d.bits.param.eq(CapParam.toT),
                    tl.d.valid.eq(1),
                ]

                with m.If(tl.d.ready):
                    with m.If(burst_len > tl.data_width // 8):
                        m.d.sync += burst_len.eq(burst_len -
                                                 tl.data_width // 8)
                        m.next = 'ACT'
                    with m.Else():
                        m.d.sync += burst_len.eq(0)
                        m.next = 'IDLE'

            with m.State('WRITE_ACK'):
                m.d.comb += [
                    tl.d.bits.opcode.eq(ChannelDOpcode.AccessAck),
                    tl.d.valid.eq(1),
                ]

                with m.If(tl.d.ready):
                    m.next = 'IDLE'

        return m


class CacheCork(Elaboratable):

    def __init__(self, in_bus, out_bus):
        self.in_bus = in_bus
        self.out_bus = out_bus

    def elaborate(self, platform):
        m = Module()

        in_bus = self.in_bus
        out_bus = self.out_bus

        if not self.in_bus.has_bce:
            m.d.comb += in_bus.connect(out_bus)
        else:
            busy = Signal(reset=1)
            is_release = Signal()

            req_opcode = Signal(ChannelAOpcode)
            req_param = Signal(GrowParam)
            req_source = Signal.like(in_bus.a.bits.source)

            resp_opcode = Signal(ChannelDOpcode)
            with m.Switch(req_opcode):
                with m.Case(ChannelAOpcode.PutFullData):
                    m.d.comb += resp_opcode.eq(ChannelDOpcode.AccessAck)
                with m.Case(ChannelAOpcode.PutPartialData):
                    m.d.comb += resp_opcode.eq(ChannelDOpcode.AccessAck)
                with m.Case(ChannelAOpcode.Get):
                    m.d.comb += resp_opcode.eq(ChannelDOpcode.AccessAckData)
                with m.Case(ChannelAOpcode.AcquireBlock):
                    m.d.comb += resp_opcode.eq(
                        Mux(req_param == GrowParam.BtoT, ChannelDOpcode.Grant,
                            ChannelDOpcode.GrantData))
                with m.Case(ChannelAOpcode.AcquirePerm):
                    m.d.comb += resp_opcode.eq(ChannelDOpcode.Grant)

            with m.If(is_release):
                m.d.comb += resp_opcode.eq(ChannelDOpcode.ReleaseAck)

            with m.If(is_release | (in_bus.c.valid & ~busy)):
                m.d.comb += [
                    out_bus.a.bits.opcode.eq(ChannelAOpcode.PutFullData),
                    out_bus.a.bits.param.eq(0),
                    out_bus.a.bits.size.eq(in_bus.c.bits.size),
                    out_bus.a.bits.source.eq(in_bus.c.bits.source),
                    out_bus.a.bits.address.eq(in_bus.c.bits.address),
                    out_bus.a.bits.mask.eq(~0),
                    out_bus.a.bits.data.eq(in_bus.c.bits.data),
                    out_bus.a.bits.corrupt.eq(in_bus.c.bits.corrupt),
                    out_bus.a.valid.eq(in_bus.c.valid & (
                        in_bus.c.bits.opcode != ChannelCOpcode.Release)),
                    in_bus.c.ready.eq(out_bus.a.ready),
                ]

            with m.Else():
                m.d.comb += in_bus.a.connect(out_bus.a)

                with m.If(in_bus.a.bits.opcode == ChannelAOpcode.AcquireBlock):
                    m.d.comb += out_bus.a.bits.opcode.eq(ChannelAOpcode.Get)

            with m.FSM():
                with m.State('IDLE'):
                    m.d.comb += [
                        busy.eq(0),
                        in_bus.a.ready.eq(1),
                        in_bus.c.ready.eq(1),
                    ]

                    m.d.sync += is_release.eq(0)

                    with m.If(in_bus.a.valid):
                        to_d = ((in_bus.a.bits.opcode
                                 == ChannelAOpcode.AcquireBlock)
                                & (in_bus.a.bits.param == GrowParam.BtoT)) | (
                                    in_bus.a.bits.opcode
                                    == ChannelAOpcode.AcquirePerm)

                        m.d.comb += in_bus.a.ready.eq(out_bus.a.ready)

                        m.d.sync += [
                            req_opcode.eq(in_bus.a.bits.opcode),
                            req_param.eq(in_bus.a.bits.param),
                            req_source.eq(in_bus.a.bits.source),
                        ]

                        with m.If(in_bus.a.fire):
                            with m.If(to_d):
                                m.next = 'ACK'
                            with m.Else():
                                m.next = 'ACT'

                    with m.If(in_bus.c.valid):
                        m.d.comb += [
                            in_bus.a.ready.eq(0),
                            in_bus.c.ready.eq((
                                in_bus.c.bits.opcode == ChannelCOpcode.Release)
                                              | out_bus.a.ready),
                        ]

                        m.d.sync += [
                            is_release.eq(1),
                            req_source.eq(in_bus.c.bits.source),
                        ]

                        with m.If(in_bus.c.fire):
                            with m.If(in_bus.c.bits.opcode ==
                                      ChannelCOpcode.Release):
                                m.next = 'ACK'
                            with m.Else():
                                m.next = 'ACT'

                with m.State('ACT'):
                    _, _, done, _ = Interface.count(m, out_bus.d.bits,
                                                    out_bus.d.fire)

                    m.d.comb += [
                        out_bus.d.connect(in_bus.d),
                        in_bus.d.bits.opcode.eq(resp_opcode),
                        in_bus.d.bits.param.eq(CapParam.toT),
                    ]

                    with m.If(done):
                        m.d.sync += is_release.eq(0)
                        m.next = 'IDLE'

                with m.State('ACK'):
                    m.d.comb += [
                        in_bus.d.valid.eq(1),
                        in_bus.d.bits.opcode.eq(resp_opcode),
                        in_bus.d.bits.source.eq(req_source),
                    ]

                    with m.If(in_bus.d.ready):
                        m.d.sync += is_release.eq(0)
                        m.next = 'IDLE'

            m.d.comb += in_bus.e.ready.eq(1)

        return m


class Arbiter(Elaboratable):

    def __init__(self, cls, *args, data_width=None, size_width=None, **kwargs):
        self.data_width = data_width
        self.size_width = size_width

        if data_width is not None:
            self.bus = Decoupled(cls,
                                 data_width=data_width,
                                 size_width=size_width,
                                 *args,
                                 **kwargs)
        else:
            self.bus = Decoupled(cls, *args, **kwargs)

        self._intrs = []

    def add(self, intr_bus):
        self._intrs.append(intr_bus)

    def elaborate(self, platform):
        m = Module()

        requests = Signal(len(self._intrs))
        grant = Signal(range(len(self._intrs)))
        m.d.comb += requests.eq(Cat(intr_bus.valid
                                    for intr_bus in self._intrs))

        if self.data_width is not None:
            beats_left = Signal(2**self.size_width + 1)
            lg_beat_bytes = log2_int(self.data_width // 8)
        else:
            beats_left = Signal()
            lg_beat_bytes = 0

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
