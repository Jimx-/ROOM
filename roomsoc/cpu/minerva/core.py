from amaranth import *

from roomsoc.interconnect import wishbone


class Minerva(Elaboratable):
    io_regions = {0x8000_0000: 0x8000_0000}

    def __init__(self, reset_address):
        self.reset_address = reset_address

        self.ibus = wishbone.Interface(data_width=32,
                                       addr_width=30,
                                       granularity=8,
                                       features=(
                                           "cti",
                                           "bte",
                                           "err",
                                       ))

        self.dbus = wishbone.Interface(data_width=32,
                                       addr_width=30,
                                       granularity=8,
                                       features=(
                                           "cti",
                                           "bte",
                                           "err",
                                       ))

        self.reset = Signal()
        self.interrupt = Signal()

        self.periph_buses = [self.ibus, self.dbus]

    def elaborate(self, platform):
        m = Module()

        cpu_params = dict(
            i_clk=ClockSignal(),
            i_rst=ResetSignal() | self.reset,
            # Interrupt
            i_timer_interrupt=0,
            i_software_interrupt=0,
            i_external_interrupt=self.interrupt,
            # Ibus
            o_ibus__stb=self.ibus.stb,
            o_ibus__cyc=self.ibus.cyc,
            o_ibus__cti=self.ibus.cti,
            o_ibus__bte=self.ibus.bte,
            o_ibus__we=self.ibus.we,
            o_ibus__adr=self.ibus.adr,
            o_ibus__dat_w=self.ibus.dat_w,
            o_ibus__sel=self.ibus.sel,
            i_ibus__ack=self.ibus.ack,
            i_ibus__err=self.ibus.err,
            i_ibus__dat_r=self.ibus.dat_r,
            # Dbus
            o_dbus__stb=self.dbus.stb,
            o_dbus__cyc=self.dbus.cyc,
            o_dbus__cti=self.dbus.cti,
            o_dbus__bte=self.dbus.bte,
            o_dbus__we=self.dbus.we,
            o_dbus__adr=self.dbus.adr,
            o_dbus__dat_w=self.dbus.dat_w,
            o_dbus__sel=self.dbus.sel,
            i_dbus__ack=self.dbus.ack,
            i_dbus__err=self.dbus.err,
            i_dbus__dat_r=self.dbus.dat_r,
        )

        m.submodules.core = Instance(
            "minerva_cpu",
            **cpu_params,
        )

        return m
