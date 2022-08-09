from amaranth import *
from amaranth.sim import Simulator

from room.consts import *
from room import Core

from roomsoc.interconnect import wishbone

core_params = dict(fetch_width=4,
                   fetch_buffer_size=16,
                   core_width=4,
                   num_pregs=96,
                   num_rob_rows=16,
                   max_br_count=4,
                   ldq_size=16,
                   stq_size=16,
                   issue_params={
                       IssueQueueType.MEM:
                       dict(dispatch_width=4, num_entries=16, issue_width=2),
                       IssueQueueType.INT:
                       dict(dispatch_width=4, num_entries=16, issue_width=4),
                   })


class Top(Elaboratable):

    def __init__(self):
        self.rst = Signal()

    def elaborate(self, platform):
        m = Module()

        m.d.comb += ResetSignal().eq(self.rst)

        ibus = wishbone.Interface(data_width=core_params['fetch_width'] * 16,
                                  adr_width=30)
        dbus = wishbone.Interface(data_width=32, adr_width=30)

        # mem_init = [0xffdff06f0f868693] + [0x0f8686930f868693] * 16
        # mem_init = [0x0f8686930f868693] * 16
        # mem_init = [
        #     0x0000011300000093, 0x0000021300000193, 0x0f868693fe0088e3
        # ] + [0x0f8686930f868693] * 16

        # mem_init = [
        #     0x0200059300000513,
        #     0x0015051300000613,
        #     0x00260613feb54ee3,
        #     0x0026061300260613,
        #     0x0000006f00260613,
        # ]

        mem_init = [0x0005a58300800593, 0x0040061300158593, 0x0000006f00b62023]

        m.submodules.sram_i = wishbone.SRAM(
            Memory(width=ibus.data_width,
                   depth=(1 << 10) // (ibus.data_width >> 3),
                   init=mem_init), ibus)

        dmem_init = [0x0, 0x0, 0x1234, 0x0]
        m.submodules.sram_d = wishbone.SRAM(
            Memory(width=dbus.data_width,
                   depth=(1 << 10) // (dbus.data_width >> 3),
                   init=dmem_init), dbus)

        core = m.submodules.core = Core(Core.validate_params(core_params))

        m.d.comb += [
            core.ibus.connect(ibus),
            ibus.adr.eq(
                core.ibus.adr[Shape.cast(range(ibus.data_width >> 3)).width:]),
            core.dbus.connect(dbus),
            dbus.adr.eq(core.dbus.adr[2:]),
        ]

        return m


if __name__ == "__main__":
    dut = Top()

    sim = Simulator(dut)
    sim.add_clock(1e-6)

    def process():
        yield dut.rst.eq(1)
        yield
        yield dut.rst.eq(0)
        for _ in range(1000):
            yield

    sim.add_sync_process(process)
    with sim.write_vcd('room.vcd'):
        sim.run()
