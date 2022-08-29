from amaranth import *
from amaranth.build import *
from amaranth.vendor.xilinx import XilinxPlatform

from room.consts import *
from room.debug import JTAGInterface
from room import Core

from roomsoc.soc import SoC
from roomsoc.interconnect import axi

core_params = dict(fetch_width=2,
                   fetch_buffer_size=16,
                   core_width=2,
                   num_pregs=96,
                   num_rob_rows=16,
                   max_br_count=4,
                   ldq_size=16,
                   stq_size=16,
                   issue_params={
                       IssueQueueType.MEM:
                       dict(dispatch_width=2, num_entries=16, issue_width=2),
                       IssueQueueType.INT:
                       dict(dispatch_width=2, num_entries=16, issue_width=2),
                   })


class Top(Elaboratable):

    mem_map = {
        'rom': 0x00000000,
        'sram': 0x20000000,
    }

    def __init__(self, rom_image=[], ram_image=[]):
        self.rom_image = rom_image
        self.ram_image = ram_image

        self.axil_master = axi.AXILiteInterface(data_width=32,
                                                addr_width=30,
                                                name='axil_master')

        self.jtag = JTAGInterface()

    def elaborate(self, platform):
        m = Module()

        soc = m.submodules.soc = SoC(bus_data_width=32, bus_addr_width=32)

        core = Core(Core.validate_params(core_params))
        m.d.comb += core.jtag.connect(self.jtag)

        soc.bus.add_master(name='axil_master', master=self.axil_master)

        soc.add_rom(name='rom',
                    origin=self.mem_map['rom'],
                    size=0x2000,
                    init=self.rom_image,
                    mode='rw')
        soc.add_ram(name='sram',
                    origin=self.mem_map['sram'],
                    size=0x1000,
                    init=self.ram_image)

        soc.add_controller()

        soc.add_cpu(core)

        for res, start, size in soc.resources():
            print(res.name, hex(start), size)

        return m


if __name__ == "__main__":
    from amaranth.back import verilog
    top = Top()

    with open('/tmp/soc_wrapper.v', 'w') as f:
        f.write(
            verilog.convert(top,
                            ports=[
                                top.axil_master.ar.valid,
                                top.axil_master.ar.addr,
                                top.axil_master.ar.ready,
                                top.axil_master.r.valid,
                                top.axil_master.r.data,
                                top.axil_master.r.resp,
                                top.axil_master.r.ready,
                                top.axil_master.aw.valid,
                                top.axil_master.aw.addr,
                                top.axil_master.aw.ready,
                                top.axil_master.w.valid,
                                top.axil_master.w.data,
                                top.axil_master.w.strb,
                                top.axil_master.w.ready,
                                top.axil_master.b.valid,
                                top.axil_master.b.resp,
                                top.axil_master.b.ready,
                                top.jtag.tck,
                                top.jtag.tdi,
                                top.jtag.tdo,
                                top.jtag.tms,
                            ],
                            name='soc_wrapper'))
