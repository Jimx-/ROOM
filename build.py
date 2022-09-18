from amaranth import *
from amaranth.build import *

from room.consts import *
from room import Core

from roomsoc.soc import SoC
from roomsoc.interconnect import axi
from roomsoc.peripheral.uart import UART
from roomsoc.peripheral.debug import JTAGInterface, DebugModule
from roomsoc.peripheral.sdc import SDController
from roomsoc.platform.kc705 import KC705Platform

import argparse
import struct


def read_mem_image(filename):
    image = []

    with open(filename, 'rb') as f:
        while True:
            w = f.read(4)
            if not w:
                break
            image.append(struct.unpack('I', w)[0])

    return image


core_params = dict(
    xlen=64,
    vaddr_bits=32,
    fetch_width=4,
    fetch_buffer_size=16,
    core_width=2,
    num_pregs=96,
    num_rob_rows=16,
    max_br_count=4,
    ldq_size=16,
    stq_size=16,
    num_breakpoints=1,
    issue_params={
        IssueQueueType.MEM: dict(dispatch_width=2,
                                 num_entries=16,
                                 issue_width=2),
        IssueQueueType.INT: dict(dispatch_width=2,
                                 num_entries=16,
                                 issue_width=2),
    },
    icache_params=dict(
        n_sets=64,
        n_ways=4,
        block_bytes=64,
    ),
)


class Top(Elaboratable):

    mem_map = {
        'rom': 0x00000000,
        'sram': 0x20000000,
    }

    def __init__(self, clk_freq, rom_image, debug_rom_image, ram_image=[]):
        self.rom_image = rom_image
        self.debug_rom_image = debug_rom_image
        self.ram_image = ram_image
        self.clk_freq = clk_freq

        self.axil_master = axi.AXILiteInterface(data_width=32,
                                                addr_width=30,
                                                name='axil_master')

        self.jtag = JTAGInterface()

        self.uart = UART(divisor=int(self.clk_freq // 115200))

        self.sdc = SDController()

    def elaborate(self, platform):
        m = Module()

        soc = m.submodules.soc = SoC(bus_data_width=32, bus_addr_width=32)

        core = Core(Core.validate_params(core_params))

        debug_module = DebugModule(self.debug_rom_image)
        m.d.comb += [
            debug_module.jtag.connect(self.jtag),
            core.interrupts.debug.eq(debug_module.debug_int),
        ]

        soc.bus.add_master(name='axil_master', master=self.axil_master)
        soc.bus.add_master(name='dm_master', master=debug_module.dbus)

        soc.add_rom(name='rom',
                    origin=self.mem_map['rom'],
                    size=0x20000,
                    init=self.rom_image,
                    mode='rw')

        soc.add_peripheral('dm', debug_module)

        soc.add_ram(name='sram',
                    origin=self.mem_map['sram'],
                    size=0x20000,
                    init=self.ram_image)

        soc.add_controller()

        soc.add_cpu(core)

        soc.add_peripheral('uart', self.uart)
        soc.add_peripheral('sdc', self.sdc)

        dm_base = 0

        for res, start, size in soc.resources():
            if res.name == 'debug_module_halted':
                dm_base = start

            print(res.name, hex(start), size)

        m.d.comb += core.debug_entry.eq(dm_base + 0x800)

        with open('include/generated/platform.h', 'w') as f:
            soc.generate_platform_header(file=f)

        return m


if __name__ == "__main__":
    from amaranth.back import verilog

    parser = argparse.ArgumentParser(description='ROOM SoC simulation')
    parser.add_argument('rom', type=str, help='ROM image')
    parser.add_argument('debug_rom', type=str, help='Debug ROM image')
    parser.add_argument('--ram', type=str, help='RAM image', default=None)
    parser.add_argument('--freq',
                        type=int,
                        help='SoC clock frequency',
                        default=50e6)
    args = parser.parse_args()

    rom = read_mem_image(args.rom)
    debug_rom = read_mem_image(args.debug_rom)

    if args.ram is not None:
        ram = read_mem_image(args.ram)
    else:
        ram = []

    top = Top(args.freq, rom_image=rom, debug_rom_image=debug_rom)

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
                                top.uart.tx,
                                top.uart.rx,
                                top.sdc.sdio_clk,
                                top.sdc.sdio_cmd_i,
                                top.sdc.sdio_cmd_o,
                                top.sdc.sdio_cmd_t,
                                top.sdc.sdio_data_i,
                                top.sdc.sdio_data_o,
                                top.sdc.sdio_data_t,
                            ],
                            platform=KC705Platform(),
                            name='soc_wrapper'))
