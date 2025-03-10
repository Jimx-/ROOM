from amaranth import *
from amaranth import tracer

from amaranth_soc import csr

__all__ = ["EventSource", "IRQLine", "InterruptSource"]


class EventSource:

    def __init__(self, *, mode="level", name=None, src_loc_at=0):
        if name is not None and not isinstance(name, str):
            raise TypeError("Name must be a string, not {!r}".format(name))

        choices = ("level", "rise", "fall")
        if mode not in choices:
            raise ValueError(
                "Invalid trigger mode {!r}; must be one of {}".format(
                    mode, ", ".join(choices)))

        self.name = name or tracer.get_var_name(depth=2 + src_loc_at)
        self.mode = mode
        self.stb = Signal(name=f"{self.name}_stb")


class IRQLine(Signal):

    def __init__(self, shape=None, *, name=None, src_loc_at=0):
        super().__init__(shape=shape, name=name, src_loc_at=1 + src_loc_at)

    __hash__ = object.__hash__


class InterruptSource(Elaboratable):

    def __init__(self, events, *, name=None, src_loc_at=0):
        if name is not None and not isinstance(name, str):
            raise TypeError("Name must be a string, not {!r}".format(name))
        self.name = name or tracer.get_var_name(depth=2 + src_loc_at)

        for event in events:
            if not isinstance(event, EventSource):
                raise TypeError(
                    "Event source must be an instance of EventSource, not {!r}"
                    .format(event))
        self._events = list(events)

        width = len(events)
        self.status = csr.Element(width, "r", name=f"{self.name}_int_status")
        self.pending = csr.Element(width,
                                   "rw",
                                   name=f"{self.name}_int_pending")
        self.enable = csr.Element(width, "rw", name=f"{self.name}_int_enable")

        self.irq = IRQLine(name=f"{self.name}_irq")

    def elaborate(self, platform):
        m = Module()

        with m.If(self.pending.w_stb):
            m.d.sync += self.pending.r_data.eq(self.pending.r_data
                                               & ~self.pending.w_data)

        with m.If(self.enable.w_stb):
            m.d.sync += self.enable.r_data.eq(self.enable.w_data)

        for i, event in enumerate(self._events):
            m.d.sync += self.status.r_data[i].eq(event.stb)

            if event.mode in ("rise", "fall"):
                event_stb_r = Signal.like(event.stb, name_suffix="_r")
                m.d.sync += event_stb_r.eq(event.stb)

            event_trigger = Signal(name=f"{event.name}_trigger")
            if event.mode == "level":
                m.d.comb += event_trigger.eq(event.stb)
            elif event.mode == "rise":
                m.d.comb += event_trigger.eq(~event_stb_r & event.stb)
            elif event.mode == "fall":
                m.d.comb += event_trigger.eq(event_stb_r & ~event.stb)
            else:
                assert False

            if event.mode == "level":
                m.d.sync += self.pending.r_data[i].eq(event_trigger)
            else:
                with m.If(event_trigger):
                    m.d.sync += self.pending.r_data[i].eq(1)

        m.d.comb += self.irq.eq(
            (self.pending.r_data & self.enable.r_data).any())

        return m
