from amaranth import *

from roomsoc.interconnect import wishbone


class SERV(Elaboratable):
    io_regions = {0x8000_0000: 0x8000_0000}

    def __init__(self, reset_address):
        self.reset_address = reset_address

        self.ibus = wishbone.Interface(data_width=32,
                                       addr_width=30,
                                       granularity=8)
        self.dbus = wishbone.Interface(data_width=32,
                                       addr_width=30,
                                       granularity=8)
        self.reset = Signal()

        self.periph_buses = [self.ibus, self.dbus]

    def elaborate(self, platform):
        m = Module()

        ibus_adr = Signal(32)
        dbus_adr = Signal(32)

        m.submodules.core = Instance(
            "serv_rf_top",
            p_RESET_PC=self.reset_address,
            i_clk=ClockSignal(),
            i_i_rst=ResetSignal() | self.reset,
            i_i_timer_irq=0,
            # I-bus
            o_o_ibus_adr=ibus_adr,
            o_o_ibus_cyc=self.ibus.cyc,
            i_i_ibus_rdt=self.ibus.dat_r,
            i_i_ibus_ack=self.ibus.ack,
            # D-bus
            o_o_dbus_adr=dbus_adr,
            o_o_dbus_dat=self.dbus.dat_w,
            o_o_dbus_sel=self.dbus.sel,
            o_o_dbus_we=self.dbus.we,
            o_o_dbus_cyc=self.dbus.cyc,
            i_i_dbus_rdt=self.dbus.dat_r,
            i_i_dbus_ack=self.dbus.ack,
        )

        m.d.comb += [
            self.ibus.adr.eq(ibus_adr[2:]),
            self.ibus.stb.eq(self.ibus.cyc),
            self.ibus.sel.eq(0xf),
            self.dbus.adr.eq(dbus_adr[2:]),
            self.dbus.stb.eq(self.dbus.cyc),
        ]

        return m
