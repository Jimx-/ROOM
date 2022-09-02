from amaranth import *
from amaranth.sim import Simulator

from room.consts import *
from room import Core
from room.debug import JTAGInterface, DebugUnit

from roomsoc.soc import SoC
from roomsoc.interconnect import axi
from roomsoc.peripheral.uart import UART

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

    mem_map = {
        'rom': 0x00000000,
        'sram': 0x20000000,
    }

    def __init__(self, clk_freq, rom_image, ram_image=[]):
        self.rom_image = rom_image
        self.ram_image = ram_image
        self.clk_freq = clk_freq

        self.axil_master = axi.AXILiteInterface(data_width=32,
                                                addr_width=32,
                                                name='axil_master')

        self.jtag = JTAGInterface()

        self.rst = Signal()

    def elaborate(self, platform):
        m = Module()

        m.d.comb += ResetSignal().eq(self.rst)

        soc = m.submodules.soc = SoC(bus_data_width=32, bus_addr_width=32)

        core = Core(Core.validate_params(core_params))

        soc.bus.add_master(name='axil_master', master=self.axil_master)

        soc.add_rom(name='rom',
                    origin=self.mem_map['rom'],
                    size=0x2000,
                    init=self.rom_image)
        soc.add_ram(name='sram',
                    origin=self.mem_map['sram'],
                    size=0x1000,
                    init=self.ram_image)

        soc.add_controller()

        soc.add_cpu(core)

        uart = UART(divisor=int(self.clk_freq // 115200))
        soc.add_peripheral('uart', uart)

        for res, start, size in soc.resources():
            print(res.name, hex(start), size)

        return m


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='ROOM SoC simulation')
    parser.add_argument('rom', type=str, help='ROM image')
    parser.add_argument('--ram', type=str, help='RAM image', default=None)
    parser.add_argument('--freq',
                        type=int,
                        help='SoC clock frequency',
                        default=1e6)
    args = parser.parse_args()

    rom = read_mem_image(args.rom)

    if args.ram is not None:
        ram = read_mem_image(args.ram)
    else:
        ram = []

    dut = Top(args.freq, rom, ram)

    sim = Simulator(dut)
    sim.add_clock(1.0 / args.freq)

    def process():
        yield dut.rst.eq(1)
        yield
        yield dut.rst.eq(0)
        for _ in range(100):
            yield

    sim.add_sync_process(process)
    with sim.write_vcd('room.vcd'):
        sim.run()
