from enum import Enum
from amaranth import *
from amaranth import tracer
from amaranth.hdl.rec import Direction
from amaranth.utils import log2_int
from amaranth_soc.memory import MemoryMap

from roomsoc.interconnect.stream import Decoupled, Repeater, Queue


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

    def tilelink_access_ack(self, size, source=0, sink=0, denied=0):
        return [
            self.d.bits.opcode.eq(ChannelDOpcode.AccessAck),
            self.d.bits.param.eq(0),
            self.d.bits.size.eq(size),
            self.d.bits.source.eq(source),
            self.d.bits.sink.eq(sink),
            self.d.bits.denied.eq(denied),
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


class CacheCork(Elaboratable):

    def __init__(self, in_bus, out_bus):
        self.in_bus = in_bus
        self.out_bus = out_bus

        if self.out_bus.source_id_width < self.in_bus.source_id_width + 1:
            raise ValueError(
                "Subordinate bus has source ID width {}, which is smaller than required ({})"
                .format(self.out_bus.source_id_width,
                        self.in_bus.source_id_width + 1))

    def elaborate(self, platform):
        m = Module()

        in_bus = self.in_bus
        out_bus = self.out_bus

        if not self.in_bus.has_bce:
            m.d.comb += in_bus.connect(out_bus)
        else:
            a_a = Decoupled(ChannelA,
                            addr_width=out_bus.addr_width,
                            data_width=out_bus.data_width,
                            size_width=out_bus.size_width,
                            source_id_width=out_bus.source_id_width)
            a_d = Decoupled(ChannelD,
                            data_width=in_bus.data_width,
                            size_width=in_bus.size_width,
                            source_id_width=in_bus.source_id_width,
                            sink_id_width=in_bus.sink_id_width)
            a_write = (in_bus.a.bits.opcode == ChannelAOpcode.PutFullData) | (
                in_bus.a.bits.opcode == ChannelAOpcode.PutPartialData)
            to_d = ((in_bus.a.bits.opcode == ChannelAOpcode.AcquireBlock) &
                    (in_bus.a.bits.param == GrowParam.BtoT)) | (
                        in_bus.a.bits.opcode == ChannelAOpcode.AcquirePerm)
            m.d.comb += in_bus.a.ready.eq(Mux(to_d, a_d.ready, a_a.ready))

            m.d.comb += [
                a_a.valid.eq(in_bus.a.valid & ~to_d),
                in_bus.a.bits.connect(a_a.bits),
                a_a.bits.source.eq((in_bus.a.bits.source << 1) | a_write),
            ]
            with m.If((in_bus.a.bits.opcode == ChannelAOpcode.AcquireBlock)
                      | (in_bus.a.bits.opcode == ChannelAOpcode.AcquirePerm)):
                m.d.comb += [
                    a_a.bits.opcode.eq(ChannelAOpcode.Get),
                    a_a.bits.param.eq(0),
                    a_a.bits.source.eq((in_bus.a.bits.source << 1) | 1),
                ]

            m.d.comb += [
                a_d.valid.eq(in_bus.a.valid & to_d),
                a_d.bits.opcode.eq(ChannelDOpcode.Grant),
                a_d.bits.source.eq(in_bus.a.bits.source),
                a_d.bits.size.eq(in_bus.a.bits.size),
            ]

            c_a = Decoupled(ChannelA,
                            addr_width=out_bus.addr_width,
                            data_width=out_bus.data_width,
                            size_width=out_bus.size_width,
                            source_id_width=out_bus.source_id_width)
            m.d.comb += [
                c_a.valid.eq(in_bus.c.valid & (
                    in_bus.c.bits.opcode == ChannelCOpcode.ReleaseData)),
                c_a.bits.opcode.eq(ChannelAOpcode.PutFullData),
                c_a.bits.param.eq(0),
                c_a.bits.size.eq(in_bus.c.bits.size),
                c_a.bits.source.eq(in_bus.c.bits.source << 1),
                c_a.bits.address.eq(in_bus.c.bits.address),
                c_a.bits.mask.eq(~0),
                c_a.bits.data.eq(in_bus.c.bits.data),
                c_a.bits.corrupt.eq(in_bus.c.bits.corrupt),
            ]

            c_d = Decoupled(ChannelD,
                            data_width=in_bus.data_width,
                            size_width=in_bus.size_width,
                            source_id_width=in_bus.source_id_width,
                            sink_id_width=in_bus.sink_id_width)
            m.d.comb += [
                c_d.valid.eq(in_bus.c.valid &
                             (in_bus.c.bits.opcode == ChannelCOpcode.Release)),
                c_d.bits.opcode.eq(ChannelDOpcode.ReleaseAck),
                c_d.bits.size.eq(in_bus.c.bits.size),
                c_d.bits.source.eq(in_bus.c.bits.source),
            ]

            m.d.comb += in_bus.c.ready.eq(
                Mux(in_bus.c.bits.opcode == ChannelCOpcode.Release, c_d.ready,
                    c_a.ready))

            m.d.comb += in_bus.e.ready.eq(1)

            d_d = Decoupled(ChannelD,
                            data_width=in_bus.data_width,
                            size_width=in_bus.size_width,
                            source_id_width=in_bus.source_id_width,
                            sink_id_width=in_bus.sink_id_width)
            m.d.comb += [
                out_bus.d.connect(d_d),
                d_d.bits.source.eq(out_bus.d.bits.source >> 1),
            ]

            with m.If((out_bus.d.bits.opcode == ChannelDOpcode.AccessAckData)
                      & out_bus.d.bits.source[0]):
                m.d.comb += [
                    d_d.bits.opcode.eq(ChannelDOpcode.GrantData),
                    d_d.bits.param.eq(CapParam.toT),
                ]
            with m.If((out_bus.d.bits.opcode == ChannelDOpcode.AccessAck)
                      & ~out_bus.d.bits.source[0]):
                m.d.comb += d_d.bits.opcode.eq(ChannelDOpcode.ReleaseAck)

            a_arbiter = m.submodules.a_arbiter = Arbiter(
                ChannelA,
                addr_width=out_bus.addr_width,
                data_width=out_bus.data_width,
                size_width=out_bus.size_width,
                source_id_width=out_bus.source_id_width,
                policy='lowest')
            a_arbiter.add(c_a)
            a_arbiter.add(a_a)
            m.d.comb += a_arbiter.bus.connect(out_bus.a)

            c_d_queue = m.submodules.c_d_queue = Queue(
                2,
                ChannelD,
                data_width=in_bus.data_width,
                size_width=in_bus.size_width,
                source_id_width=in_bus.source_id_width,
                sink_id_width=in_bus.sink_id_width,
                flow=False)
            m.d.comb += c_d.connect(c_d_queue.enq)
            a_d_queue = m.submodules.a_d_queue = Queue(
                2,
                ChannelD,
                data_width=in_bus.data_width,
                size_width=in_bus.size_width,
                source_id_width=in_bus.source_id_width,
                sink_id_width=in_bus.sink_id_width,
                flow=False)
            m.d.comb += a_d.connect(a_d_queue.enq)
            d_arbiter = m.submodules.d_arbiter = Arbiter(
                ChannelD,
                data_width=in_bus.data_width,
                size_width=in_bus.size_width,
                source_id_width=in_bus.source_id_width,
                sink_id_width=in_bus.sink_id_width,
                policy='lowest')
            d_arbiter.add(d_d)
            d_arbiter.add(c_d_queue.deq)
            d_arbiter.add(a_d_queue.deq)
            m.d.comb += d_arbiter.bus.connect(in_bus.d)

        return m


class Arbiter(Elaboratable):

    @staticmethod
    def _round_robin(m, requests, grant, early_grant):
        with m.Switch(grant):
            for i in range(len(requests)):
                with m.Case(i):
                    for pred in reversed(range(i)):
                        with m.If(requests[pred]):
                            m.d.comb += early_grant.eq(pred)

                    for succ in reversed(range(i + 1, len(requests))):
                        with m.If(requests[succ]):
                            m.d.comb += early_grant.eq(succ)

    @staticmethod
    def _lowest(m, requests, grant, early_grant):
        for i in reversed(range(len(requests))):
            with m.If(requests[i]):
                m.d.comb += early_grant.eq(i)

    def __init__(self, cls, *args, policy='rr', **kwargs):
        self.bus = Decoupled(cls, *args, **kwargs)

        self.policy = dict(
            rr=Arbiter._round_robin,
            lowest=Arbiter._lowest,
        )[policy]

        self._intrs = []

    def add(self, intr_bus):
        self._intrs.append(intr_bus)

    def elaborate(self, platform):
        m = Module()

        requests = Signal(len(self._intrs))
        grant = Signal(range(len(self._intrs)))
        early_grant = Signal.like(grant)
        m.d.comb += [
            requests.eq(Cat(intr_bus.valid for intr_bus in self._intrs)),
            early_grant.eq(grant),
        ]

        if hasattr(self.bus.bits, 'size'):
            beats_left = Signal(2**len(self.bus.bits.size) + 1)
        else:
            beats_left = Signal()

        bus_busy = beats_left != 0

        with m.If(~bus_busy):
            self.policy(m, requests, grant, early_grant)

            with m.Switch(early_grant):
                for i in range(len(requests)):
                    with m.Case(i):
                        with m.If(requests[i] & self.bus.fire):
                            m.d.sync += [
                                grant.eq(i),
                                beats_left.eq(
                                    Interface.num_beats0(self._intrs[i].bits)),
                            ]

        with m.If(bus_busy):
            m.d.sync += beats_left.eq(beats_left - self.bus.fire)

        with m.Switch(early_grant):
            for i, intr_bus in enumerate(self._intrs):
                with m.Case(i):
                    m.d.comb += intr_bus.connect(self.bus)

        return m


class Fragmenter(Elaboratable):

    def __init__(self, max_size, min_size, in_bus):
        self.max_size = max_size
        self.min_size = min_size

        added_bits = log2_int(max_size // min_size) + 1

        self.in_bus = in_bus
        self.out_bus = Interface(addr_width=in_bus.addr_width,
                                 data_width=in_bus.data_width,
                                 size_width=in_bus.size_width,
                                 source_id_width=in_bus.source_id_width +
                                 added_bits)

    def elaborate(self, platform):
        m = Module()

        in_bus = self.in_bus
        out_bus = self.out_bus

        if self.min_size == self.max_size:
            m.d.comb += in_bus.connect(out_bus)
        else:
            beat_bytes = out_bus.data_width // 8
            counter_bits = log2_int(self.max_size // beat_bytes)

            acknum = Signal(counter_bits)
            d_orig = Signal.like(in_bus.d.bits.size)
            d_toggle = Signal()
            d_fragnum = out_bus.d.bits.source[:log2_int(self.max_size //
                                                        self.min_size)]
            d_first = acknum == 0
            d_last = d_fragnum == 0
            dsize_bytes = (1 << out_bus.d.bits.size)[:log2_int(self.min_size) +
                                                     1]
            dsize_mask = (dsize_bytes - 1)[:log2_int(self.min_size)]
            d_has_data = Interface.has_data(out_bus.d.bits)

            acknum_fragment = d_fragnum << log2_int(
                self.min_size // beat_bytes)
            acknum_size = dsize_mask >> log2_int(beat_bytes)
            d_first_acknum = acknum_fragment | Mux(d_has_data, acknum_size, 0)
            ack_decrement = Mux(d_has_data, 1,
                                dsize_bytes >> log2_int(beat_bytes))
            d_first_size_mask = Signal(log2_int(self.max_size))
            d_first_size = Signal.like(in_bus.d.bits.size)

            m.d.comb += d_first_size_mask.eq(
                d_fragnum << log2_int(self.min_size)
                | dsize_mask)
            for i in range(len(d_first_size_mask)):
                with m.If(d_first_size_mask[i]):
                    m.d.comb += d_first_size.eq(i + 1)

            with m.If(out_bus.d.fire):
                m.d.sync += acknum.eq(
                    Mux(d_first, d_first_acknum, acknum - ack_decrement))
                with m.If(d_first):
                    m.d.sync += [
                        d_orig.eq(d_first_size),
                        d_toggle.eq(out_bus.d.bits.source[log2_int(
                            self.max_size // self.min_size)]),
                    ]

            drop = ~(d_has_data | d_last)
            m.d.comb += [
                out_bus.d.ready.eq(in_bus.d.ready | drop),
                in_bus.d.valid.eq(out_bus.d.valid & ~drop),
                out_bus.d.bits.connect(in_bus.d.bits),
                in_bus.d.bits.source.eq(
                    out_bus.d.bits.source[log2_int(self.max_size //
                                                   self.min_size) + 1:]),
                in_bus.d.bits.size.eq(Mux(d_first, d_first_size, d_orig)),
            ]

            repeater = m.submodules.repeater = Repeater(
                ChannelA,
                addr_width=in_bus.addr_width,
                data_width=in_bus.data_width,
                size_width=in_bus.size_width,
                source_id_width=in_bus.source_id_width,
            )
            m.d.comb += in_bus.a.connect(repeater.enq)
            in_a = repeater.deq

            a_limit = log2_int(self.min_size)
            a_orig = in_a.bits.size
            a_frag = Mux(a_orig > a_limit, a_limit, a_orig)
            a_orig_mask = ((1 << a_orig) - 1)[:log2_int(self.max_size)]
            a_frag_mask = ((1 << a_frag) - 1)[:log2_int(self.min_size)]
            a_has_data = Interface.has_data(in_a.bits)
            a_mask = Mux(a_has_data, 0, a_frag_mask)

            gennum = Signal(counter_bits)
            a_first = gennum == 0
            old_gennum = Mux(a_first, a_orig_mask >> log2_int(beat_bytes),
                             gennum - 1)[:counter_bits]
            new_gennum = ~(~old_gennum | (a_mask >> log2_int(beat_bytes)))
            a_fragnum = Signal(counter_bits)
            m.d.comb += a_fragnum.eq(
                ~(~(old_gennum >> log2_int(self.min_size // beat_bytes)))
                | (a_frag_mask >> log2_int(self.min_size)))
            d_toggle_last = Signal()
            a_toggle = ~Mux(a_first, d_toggle, d_toggle_last)

            with m.If(a_first):
                m.d.sync += d_toggle_last.eq(d_toggle)

            with m.If(out_bus.a.fire):
                m.d.sync += gennum.eq(new_gennum)

            m.d.comb += [
                repeater.repeat.eq(~a_has_data & (a_fragnum != 0)),
                in_a.connect(out_bus.a),
                out_bus.a.bits.address.eq(in_a.bits.address | ~(
                    (old_gennum << log2_int(beat_bytes)) | ~a_orig_mask
                    | a_frag_mask | (self.min_size - 1))),
                out_bus.a.bits.source.eq(
                    Cat(a_fragnum, a_toggle, in_a.bits.source)),
                out_bus.a.bits.size.eq(a_frag),
                out_bus.a.bits.data.eq(in_bus.a.bits.data),
                out_bus.a.bits.mask.eq(
                    Mux(repeater.full, Repl(1, beat_bytes),
                        in_bus.a.bits.mask)),
            ]

            if in_bus.has_bce:
                m.d.comb += [
                    in_bus.c.ready.eq(1),
                    in_bus.e.ready.eq(1),
                ]

            if out_bus.has_bce:
                m.d.comb += out_bus.b.ready.eq(1)

        return m


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
                                   source_id_width=tl.source_id_width + 1,
                                   sink_id_width=tl.sink_id_width)
            m.submodules.cache_adapter = CacheCork(tl, tl_adapted)
            tl = tl_adapted

        fragmenter = m.submodules.fragmenter = Fragmenter(
            max_size=2**(1 << tl.size_width),
            min_size=wb.data_width // 8,
            in_bus=tl,
        )
        tl = fragmenter.out_bus

        wb_adr_shift = log2_int(tl.data_width // 8)

        d = Decoupled(
            ChannelD,
            data_width=tl.data_width,
            size_width=tl.size_width,
            source_id_width=tl.source_id_width,
            sink_id_width=tl.sink_id_width,
        )
        d_queue = m.submodules.d_queue = Queue(
            1,
            ChannelD,
            data_width=tl.data_width,
            size_width=tl.size_width,
            source_id_width=tl.source_id_width,
            sink_id_width=tl.sink_id_width,
            flow=True,
        )
        m.d.comb += [
            d.connect(d_queue.enq),
            d_queue.deq.connect(tl.d),
        ]

        a = m.submodules.a_queue = Queue(
            1,
            ChannelA,
            addr_width=tl.addr_width,
            data_width=tl.data_width,
            size_width=tl.size_width,
            source_id_width=tl.source_id_width,
            flow=True,
        )
        m.d.comb += tl.a.connect(a.enq)

        d_stall = Signal()
        m.d.sync += d_stall.eq(tl.d.valid & ~tl.d.ready)

        a_cyc = a.deq.valid & ~d_stall
        a_enable = Signal()
        a_write = Interface.has_data(a.deq.bits)

        with m.If(a_cyc):
            m.d.sync += a_enable.eq(1)
        with m.If(d.fire):
            m.d.sync += a_enable.eq(0)

        d_write = Signal()
        d_source = Signal.like(a.deq.bits.source)
        d_size = Signal.like(a.deq.bits.size)
        with m.If(a_cyc & ~a_enable):
            m.d.sync += [
                d_write.eq(a_write),
                d_source.eq(a.deq.bits.source),
                d_size.eq(a.deq.bits.size),
            ]

        m.d.comb += [
            wb.cyc.eq(a_cyc),
            wb.stb.eq(a_cyc),
            wb.adr.eq((a.deq.bits.address - self.base_addr)[wb_adr_shift:]),
            wb.dat_w.eq(a.deq.bits.data),
            wb.we.eq(a_write),
            wb.sel.eq(a.deq.bits.mask),
        ]

        m.d.comb += [
            a.deq.ready.eq(a_enable & wb.ack),
            d.valid.eq(a_enable & wb.ack),
        ]

        m.d.comb += [
            d.bits.opcode.eq(
                Mux(d_write, ChannelDOpcode.AccessAck,
                    ChannelDOpcode.AccessAckData)),
            d.bits.size.eq(d_size),
            d.bits.source.eq(d_source),
            d.bits.data.eq(wb.dat_r),
        ]

        if hasattr(wb, 'err'):
            m.d.comb += [
                d.bits.denied.eq(d_write & wb.err),
                d.bits.corrupt.eq(~d_write & wb.err),
            ]

        return m


class Serializer(Elaboratable):

    def __init__(self, in_bus):
        self.in_bus = in_bus
        self.out_bus = Interface(addr_width=in_bus.addr_width,
                                 data_width=in_bus.data_width,
                                 size_width=in_bus.size_width,
                                 source_id_width=in_bus.source_id_width,
                                 sink_id_width=in_bus.sink_id_width,
                                 has_bce=in_bus.has_bce)

    def elaborate(self, platform):
        m = Module()

        in_bus = self.in_bus
        out_bus = self.out_bus

        a_id = in_bus.a.bits.source
        flight = Signal()
        flight_id = Signal.like(a_id)

        a_first, _, _, _ = Interface.count(m, in_bus.a.bits, in_bus.a.fire)
        d_first, _, _, _ = Interface.count(m, in_bus.d.bits, in_bus.d.fire)
        d_to_a = out_bus.d.bits.opcode != ChannelDOpcode.ReleaseAck

        with m.If(a_first & in_bus.a.fire):
            m.d.sync += flight.eq(1)
        with m.If(d_first & d_to_a & in_bus.d.fire):
            m.d.sync += flight.eq(0)

        with m.If(in_bus.a.fire):
            m.d.sync += flight_id.eq(a_id)

        stall = a_first & flight & (flight_id != a_id)

        m.d.comb += [
            in_bus.a.connect(out_bus.a),
            out_bus.d.connect(in_bus.d),
            out_bus.a.valid.eq(in_bus.a.valid & ~stall),
            in_bus.a.ready.eq(out_bus.a.ready & ~stall),
        ]

        if in_bus.has_bce:
            m.d.comb += [
                out_bus.b.connect(in_bus.b),
                in_bus.c.connect(out_bus.c),
                in_bus.e.connect(out_bus.e),
            ]

        return m
