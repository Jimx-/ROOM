from amaranth import *
from amaranth.sim import Simulator

from room.consts import *
from room import Core

from roomsoc.soc import SoC
from roomsoc.interconnect import wishbone

import argparse
import struct


def read_rom_image(filename):
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

    def __init__(self, rom_image):
        self.rom_image = rom_image

        self.rst = Signal()

    def elaborate(self, platform):
        m = Module()

        m.d.comb += ResetSignal().eq(self.rst)

        ibus = wishbone.Interface(data_width=32, addr_width=30, granularity=8)

        soc = m.submodules.soc = SoC(bus_data_width=32, bus_addr_width=32)

        core = Core(Core.validate_params(core_params))

        soc.add_rom(name='rom',
                    origin=self.mem_map['rom'],
                    size=0x1000,
                    init=self.rom_image)
        soc.add_ram(name='sram', origin=self.mem_map['sram'], size=0x1000)

        soc.add_controller()

        soc.add_cpu(core)

        for res, start, size in soc.resources():
            print(res.name, hex(start), size)

        return m


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='ROOM SoC simulation')
    parser.add_argument('rom', type=str, help='ROM image')
    args = parser.parse_args()

    rom = read_rom_image(args.rom)

    dut = Top(rom)

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
