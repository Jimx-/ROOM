from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.stream import Decoupled, Valid


class _AXIStreamLayout(Record):

    def __init__(self,
                 data_width,
                 keep_width,
                 id_width,
                 dest_width,
                 user_width,
                 layout=None,
                 name=None,
                 src_loc_at=0):
        if layout is not None:
            payload_layout = layout
        else:
            payload_layout = [
                ('data', max(1, data_width), Direction.FANOUT),
                ('keep', max(1, keep_width), Direction.FANOUT),
            ]

        payload_layout += [
            ('id', max(1, id_width), Direction.FANOUT),
            ('dest', max(1, dest_width), Direction.FANOUT),
            ('user', max(1, user_width), Direction.FANOUT),
            ('last', 1, Direction.FANOUT),
        ]

        super().__init__(payload_layout, name=name, src_loc_at=1 + src_loc_at)


class AXIStreamInterface(Decoupled):

    def __init__(self,
                 data_width=0,
                 keep_width=None,
                 id_width=0,
                 dest_width=0,
                 user_width=0,
                 layout=None,
                 name=None):
        self.data_width = data_width
        self.keep_width = data_width // 8 if keep_width is None else keep_width
        self.id_width = id_width
        self.dest_width = dest_width
        self.user_width = user_width

        super().__init__(_AXIStreamLayout,
                         self.data_width,
                         self.keep_width,
                         self.id_width,
                         self.dest_width,
                         self.user_width,
                         layout,
                         name=name,
                         src_loc_at=1)


class AXIStreamPacketizer(Elaboratable):

    def __init__(self, cls, *args, data_width=0, **kwargs):
        self.data_width = data_width

        self.sink = AXIStreamInterface(data_width=data_width)
        self.source = AXIStreamInterface(data_width=data_width)
        self.header = cls(*args, **kwargs)

    def elaborate(self, platform):
        m = Module()

        data_width = len(self.sink.bits.data)
        beat_bytes = data_width // 8
        header_beats = len(self.header) // data_width
        header_leftover = (len(self.header) // 8) % beat_bytes
        aligned = header_leftover == 0

        sr = Signal(len(self.header), reset_less=True)
        sr_load = Signal()
        sr_shift = Signal()
        count = Signal(range(max(header_beats, 2)))

        with m.If(sr_load):
            m.d.sync += sr.eq(self.header)
        if header_beats > 1:
            with m.If(sr_shift):
                m.d.sync += sr.eq(sr[self.data_width:])

        if not aligned:
            sink_d = _AXIStreamLayout(data_width=data_width,
                                      keep_width=data_width // 8,
                                      id_width=0,
                                      dest_width=0,
                                      user_width=0)

            with m.If(self.sink.fire):
                m.d.sync += sink_d.eq(self.sink.bits)

        from_idle = Signal()

        with m.FSM():
            with m.State("IDLE"):
                m.d.comb += self.sink.ready.eq(1)
                m.d.sync += [
                    count.eq(1),
                    from_idle.eq(1),
                ]

                with m.If(self.sink.valid):
                    m.d.comb += self.sink.ready.eq(0)

                    if header_beats > 0:
                        m.d.comb += [
                            self.source.valid.eq(1),
                            self.source.bits.last.eq(0),
                            self.source.bits.data.eq(
                                self.header[:self.data_width]),
                            self.source.bits.keep.eq(~0),
                        ]

                        with m.If(self.source.fire):
                            m.d.comb += sr_load.eq(1)

                            if header_beats <= 1:
                                m.next = "ALIGNED-DATA-COPY" if aligned else "UNALIGNED-DATA-COPY"
                            else:
                                m.next = "HEADER-SEND"

                    else:
                        m.d.sync += [
                            from_idle.eq(0),
                            sink_d.data[(beat_bytes - header_leftover) *
                                        8:].eq(self.header),
                            sink_d.keep[beat_bytes - header_leftover:].eq(~0),
                        ]

                        m.next = "UNALIGNED-DATA-COPY"

            if header_beats > 1:
                with m.State("HEADER-SEND"):
                    m.d.comb += [
                        self.source.valid.eq(1),
                        self.source.bits.last.eq(0),
                        self.source.bits.data.eq(
                            sr[min(self.data_width,
                                   len(sr) - 1):]),
                        self.source.bits.keep.eq(~0),
                    ]

                    with m.If(self.source.fire):
                        m.d.comb += sr_shift.eq(1)
                        m.d.sync += count.eq(count + 1)

                        with m.If(count == (header_beats - 1)):
                            m.next = "ALIGNED-DATA-COPY" if aligned else "UNALIGNED-DATA-COPY"

            if aligned:
                with m.State("ALIGNED-DATA-COPY"):
                    m.d.comb += [
                        self.source.valid.eq(self.sink.valid),
                        self.source.bits.last.eq(self.sink.bits.last),
                        self.sink.ready.eq(self.source.ready),
                        self.source.bits.data.eq(self.sink.bits.data),
                        self.source.bits.keep.eq(self.sink.bits.keep),
                    ]

                    with m.If(self.source.fire & self.source.bits.last):
                        m.next = "IDLE"

            else:
                with m.State("UNALIGNED-DATA-COPY"):
                    m.d.comb += [
                        self.source.valid.eq(self.sink.valid
                                             | sink_d.last),
                        self.source.bits.last.eq((
                            self.sink.bits.last
                            & ~self.sink.bits.keep[beat_bytes -
                                                   header_leftover:].any())
                                                 | sink_d.last),
                        self.sink.ready.eq(self.source.ready),
                    ]

                    with m.If(from_idle):
                        m.d.comb += [
                            self.source.bits.
                            data[:max(header_leftover * 8, 1)].eq(
                                sr[min(self.data_width,
                                       len(sr) - 1):]),
                            self.source.bits.keep[:header_leftover].eq(~0),
                        ]
                    with m.Else():
                        m.d.comb += [
                            self.source.bits.
                            data[:max(header_leftover * 8, 1)].eq(
                                sink_d.data[(beat_bytes - header_leftover) *
                                            8:]),
                            self.source.bits.keep[:header_leftover].eq(
                                sink_d.keep[beat_bytes - header_leftover:]),
                        ]

                    m.d.comb += [
                        self.source.bits.data[header_leftover * 8:].eq(
                            self.sink.bits.data),
                        self.source.bits.keep[header_leftover:].eq(
                            Mux(self.sink.valid & ~sink_d.last,
                                self.sink.bits.keep, 0)),
                    ]

                    with m.If(self.source.fire):
                        m.d.comb += self.sink.ready.eq(~sink_d.last)
                        m.d.sync += from_idle.eq(0)
                        with m.If(self.source.bits.last):
                            m.d.sync += sink_d.last.eq(0)
                            m.next = "IDLE"

        return m


class AXIStreamDepacketizer(Elaboratable):

    def __init__(self, cls, *args, data_width=0, **kwargs):
        self.data_width = data_width

        self.sink = AXIStreamInterface(data_width=data_width)
        self.source = AXIStreamInterface(data_width=data_width)
        self.header = cls(*args, **kwargs)

    def elaborate(self, platform):
        m = Module()

        data_width = len(self.sink.bits.data)
        beat_bytes = data_width // 8
        header_beats = len(self.header) // data_width
        header_leftover = (len(self.header) // 8) % beat_bytes
        aligned = header_leftover == 0

        sr = Signal(len(self.header), reset_less=True)
        sr_shift = Signal()
        sr_shift_leftover = Signal()
        count = Signal(range(max(header_beats, 2)))

        m.d.comb += self.header.eq(sr)
        if header_beats == 1 and header_leftover == 0:
            with m.If(sr_shift):
                m.d.sync += sr.eq(self.sink.bits.data)
        else:
            with m.If(sr_shift):
                m.d.sync += sr.eq(Cat(sr[beat_bytes * 8:],
                                      self.sink.bits.data))
            with m.If(sr_shift_leftover):
                m.d.sync += sr.eq(
                    Cat(sr[header_leftover * 8:], self.sink.bits.data))

        if not aligned:
            sink_d = _AXIStreamLayout(data_width=data_width,
                                      keep_width=data_width // 8,
                                      id_width=0,
                                      dest_width=0,
                                      user_width=0)

            with m.If(self.sink.fire):
                m.d.sync += sink_d.eq(self.sink.bits)

        from_idle = Signal()
        seen_last = Signal()

        with m.FSM():
            with m.State("IDLE"):
                m.d.comb += self.sink.ready.eq(1)
                m.d.sync += [
                    count.eq(1),
                    from_idle.eq(1),
                    seen_last.eq(0),
                ]

                with m.If(self.sink.fire):
                    m.d.comb += sr_shift.eq(1)
                    m.d.sync += seen_last.eq(self.sink.bits.last)

                    if header_beats <= 1:
                        m.next = "ALIGNED-DATA-COPY" if aligned else "UNALIGNED-DATA-COPY"
                    else:
                        m.next = "HEADER-RECEIVE"

            if header_beats > 1:
                with m.State("HEADER-RECEIVE"):
                    m.d.comb += self.sink.ready.eq(1)

                    with m.If(self.sink.fire):
                        m.d.comb += sr_shift.eq(1)
                        m.d.sync += [
                            count.eq(count + 1),
                            seen_last.eq(self.sink.bits.last),
                        ]

                        with m.If(count == (header_beats - 1)):
                            m.next = "ALIGNED-DATA-COPY" if aligned else "UNALIGNED-DATA-COPY"

            if aligned:
                with m.State("ALIGNED-DATA-COPY"):
                    m.d.comb += [
                        self.source.valid.eq(self.sink.valid | seen_last),
                        self.source.bits.last.eq(self.sink.bits.last
                                                 | seen_last),
                        self.sink.ready.eq(self.source.ready & ~seen_last),
                        self.source.bits.data.eq(self.sink.bits.data),
                        self.source.bits.keep.eq(
                            Mux(seen_last, 0, self.sink.bits.keep)),
                    ]

                    with m.If(self.source.fire & self.source.bits.last):
                        m.next = "IDLE"

            else:
                with m.State("UNALIGNED-DATA-COPY"):
                    m.d.comb += [
                        self.source.valid.eq(self.sink.valid
                                             | sink_d.last),
                        self.source.bits.last.eq((
                            self.sink.bits.last
                            & ~self.sink.bits.keep[header_leftover:].any())
                                                 | sink_d.last),
                        self.sink.ready.eq(self.source.ready & ~sink_d.last),
                        self.source.bits.data.eq(sink_d.data[header_leftover *
                                                             8:]),
                        self.source.bits.data[min((beat_bytes -
                                                   header_leftover) *
                                                  8, data_width - 1):].eq(
                                                      self.sink.bits.data),
                        self.source.bits.keep.eq(
                            sink_d.keep[header_leftover:]),
                        self.source.bits.keep[min((
                            beat_bytes -
                            header_leftover), data_width // 8 - 1):].eq(
                                Mux(self.sink.valid & ~sink_d.last,
                                    self.sink.bits.keep, 0)),
                    ]

                    with m.If(from_idle):
                        m.d.comb += [
                            self.source.valid.eq(0),
                            self.sink.ready.eq(1),
                        ]
                        with m.If(self.sink.fire):
                            m.d.sync += from_idle.eq(0)
                            m.d.comb += sr_shift_leftover.eq(1)

                    with m.If(self.source.fire & self.source.bits.last):
                        m.next = "IDLE"

        return m


class AXIStreamArbiter(Elaboratable):

    def __init__(self,
                 n,
                 *,
                 data_width=0,
                 keep_width=None,
                 id_width=0,
                 dest_width=0,
                 user_width=0,
                 layout=None):
        self.n = n

        self.inp = [
            AXIStreamInterface(data_width=data_width,
                               keep_width=keep_width,
                               id_width=id_width,
                               dest_width=dest_width,
                               user_width=user_width,
                               layout=layout,
                               name=f'in{i}') for i in range(n)
        ]

        self.out = AXIStreamInterface(data_width=data_width,
                                      keep_width=keep_width,
                                      id_width=id_width,
                                      dest_width=dest_width,
                                      user_width=user_width,
                                      layout=layout)

    def elaborate(self, platform):
        m = Module()

        req = Cat(inp.valid for inp in self.inp)

        grant = Valid(Signal, range(self.n))
        grant_next = Signal.like(grant.bits)
        m.d.comb += grant_next.eq(grant.bits)
        with m.Switch(grant.bits):
            for i in range(len(req)):
                with m.Case(i):
                    for pred in reversed(range(i)):
                        with m.If(req[pred]):
                            m.d.comb += grant_next.eq(pred)
                    for succ in reversed(range(i + 1, len(req))):
                        with m.If(req[succ]):
                            m.d.comb += grant_next.eq(succ)

        with m.If(req.any() & ~grant.valid):
            m.d.sync += [
                grant.bits.eq(grant_next),
                grant.valid.eq(1),
            ]
        with m.If(self.out.fire & self.out.bits.last):
            m.d.sync += grant.valid.eq(0)

        sel = Signal.like(grant.bits)
        m.d.comb += sel.eq(Mux(grant.valid, grant.bits, grant_next))

        with m.Switch(sel):
            for i in range(self.n):
                with m.Case(i):
                    m.d.comb += self.inp[i].connect(self.out)

        return m


class _DownConverter(Elaboratable):

    def __init__(self, dw_from, dw_to, ratio, reverse):
        self.dw_from = dw_from
        self.dw_to = dw_to
        self.ratio = ratio
        self.reverse = reverse

        self.sink = AXIStreamInterface(data_width=dw_from)
        self.source = AXIStreamInterface(data_width=dw_to)

    def elaborate(self, platform):
        m = Module()

        bytes_to = self.dw_to // 8
        sel = Signal(range(self.ratio))

        m.d.comb += [
            self.source.valid.eq(self.sink.valid),
            self.source.bits.last.eq(self.sink.bits.last
                                     & (self == (self.ratio - 1))),
        ]

        with m.Switch(sel):
            for i in range(self.ratio):
                with m.Case(i):
                    n = self.ratio - i - 1 if self.reverse else i

                    m.d.comb += [
                        self.source.bits.data.eq(
                            self.sink.bits.data[n * self.dw_to:(n + 1) *
                                                self.dw_to]),
                        self.source.bits.keep.eq(
                            self.sink.bits.keep[n * bytes_to:(n + 1) *
                                                bytes_to]),
                    ]

        with m.If(self.source.fire):
            m.d.sync += sel.eq(sel + 1)

            with m.If(sel == self.ratio - 1):
                m.d.comb += self.sink.ready.eq(1)
                m.d.sync += sel.eq(0)

        return m


class _UpConverter(Elaboratable):

    def __init__(self, dw_from, dw_to, ratio, reverse):
        self.dw_from = dw_from
        self.dw_to = dw_to
        self.ratio = ratio
        self.reverse = reverse

        self.sink = AXIStreamInterface(data_width=dw_from)
        self.source = AXIStreamInterface(data_width=dw_to)

    def elaborate(self, platform):
        m = Module()

        bytes_from = self.dw_from // 8
        sel = Signal(range(self.ratio))

        m.d.comb += self.sink.ready.eq(~self.source.valid | self.source.ready)

        with m.If(self.source.ready):
            m.d.sync += self.source.valid.eq(0)

        with m.If(self.sink.fire):
            with m.If((sel == (self.ratio - 1)) | self.sink.last):
                m.d.sync += [
                    self.source.valid.eq(1),
                    sel.eq(0),
                ]
            with m.Else():
                m.d.sync += sel.eq(sel + 1)

        with m.If(self.source.fire):
            with m.If(self.sink.fire):
                m.d.sync += self.source.bits.last(self.sink.bits.last)
            with m.Else():
                m.d.sync += self.source.bits.last(0)
        with m.If(self.sink.fire):
            m.d.sync += self.source.bits.last.eq(self.source.bits.last
                                                 | self.sink.bits.last)

        with m.Switch(sel):
            for i in range(self.ratio):
                with m.Case(i):
                    n = self.ratio - i - 1 if self.reverse else i

                    with m.If(self.sink.fire):
                        m.d.sync += [
                            self.source.bits.data[n * self.dw_from:(n + 1) *
                                                  self.dw_from].eq(
                                                      self.sink.bits.data),
                            self.source.bits.keep[n * bytes_from:(n + 1) *
                                                  bytes_from].eq(
                                                      self.sink.bits.keep),
                        ]

        return m


class AXIStreamConverter(Elaboratable):

    def __init__(self, dw_from, dw_to, reverse=False):
        self.dw_from = dw_from
        self.dw_to = dw_to
        self.reverse = reverse

        self.sink = AXIStreamInterface(data_width=dw_from)
        self.source = AXIStreamInterface(data_width=dw_to)

    def _get_converter_ratio(self):
        converter_cls = None
        ratio = 1

        if self.dw_from > self.dw_to:
            converter_cls = _DownConverter
            if self.dw_from % self.dw_to:
                raise ValueError("Ratio must be an int")
            ratio = self.dw_from // self.dw_to
        elif self.dw_from < self.dw_to:
            converter_cls = _UpConverter
            if self.dw_to % self.dw_from:
                raise ValueError("Ratio must be an int")
            ratio = self.dw_to // self.dw_from

        return converter_cls, ratio

    def elaborate(self, platform):
        m = Module()

        if self.dw_from == self.dw_to:
            m.d.comb += self.sink.connect(self.source)
        else:
            cls, ratio = self._get_converter_ratio()
            converter = m.submodules.converter = cls(dw_from=self.dw_from,
                                                     dw_to=self.dw_to,
                                                     ratio=ratio,
                                                     reverse=self.reverse)

            m.d.comb += [
                self.sink.connect(converter.sink),
                converter.source.connect(self.source),
            ]

        return m
