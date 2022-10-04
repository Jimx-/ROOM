from amaranth import *
from amaranth.sim import Simulator

from room.consts import *
from room.core import Core, CoreDebug

from roomsoc.soc import SoC
from roomsoc.interconnect import axi
from roomsoc.peripheral.uart import UART
from roomsoc.peripheral.sdc import MockSDController
from roomsoc.peripheral.debug import JTAGInterface, DebugModule

import argparse
import struct


def read_mem_image(filename, word_len=32):
    image = []

    with open(filename, 'rb') as f:
        while True:
            w = f.read(4)
            if not w:
                break
            image.append(struct.unpack('I', w)[0])

    if word_len == 64:
        if len(image) & 1:
            image.append(0)
        image = [((hi << 32) | lo) for lo, hi in zip(*(iter(image), ) * 2)]

    return image


core_params = dict(
    xlen=64,
    vaddr_bits=32,
    fetch_width=4,
    fetch_buffer_size=16,
    core_width=4,
    num_int_pregs=96,
    num_fp_pregs=64,
    num_rob_rows=16,
    max_br_count=4,
    ldq_size=16,
    stq_size=16,
    num_breakpoints=1,
    issue_params={
        IssueQueueType.MEM: dict(dispatch_width=4,
                                 num_entries=16,
                                 issue_width=2),
        IssueQueueType.INT: dict(dispatch_width=4,
                                 num_entries=16,
                                 issue_width=4),
        IssueQueueType.FP: dict(dispatch_width=4,
                                num_entries=16,
                                issue_width=2),
    },
    icache_params=dict(
        n_sets=64,
        n_ways=4,
        block_bytes=64,
    ),
    use_fpu=True,
    flen=64,
)


class Top(Elaboratable):

    mem_map = {
        'rom': 0x00000000,
        'sram': 0x20000000,
    }

    def __init__(self,
                 clk_freq,
                 core_params,
                 rom_image,
                 debug_rom_image,
                 ram_image=[],
                 sim_debug=False):
        self.rom_image = rom_image
        self.debug_rom_image = debug_rom_image
        self.ram_image = ram_image
        self.clk_freq = clk_freq
        self.core_params = Core.validate_params(core_params)
        self.sim_debug = sim_debug

        self.axil_master = axi.AXILiteInterface(data_width=32,
                                                addr_width=32,
                                                name='axil_master')

        self.rst = Signal()

        self.jtag = JTAGInterface()

        if sim_debug:
            self.core_debug = CoreDebug(self.core_params)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += [
            ResetSignal().eq(self.rst),
            self.jtag.tck.eq(ClockSignal('debug')),
        ]

        soc = m.submodules.soc = SoC(bus_data_width=32, bus_addr_width=32)

        core = Core(self.core_params, sim_debug=self.sim_debug)

        if self.sim_debug:
            m.d.comb += self.core_debug.eq(core.core_debug)

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
                    init=self.rom_image)

        soc.add_peripheral('dm', debug_module)

        soc.add_ram(name='sram',
                    origin=self.mem_map['sram'],
                    size=0x20000,
                    init=self.ram_image)

        soc.add_controller()

        soc.add_cpu(core)

        uart = UART(divisor=int(self.clk_freq // 115200))
        soc.add_peripheral('uart', uart)

        sdc = MockSDController()
        soc.add_peripheral('sdc', sdc)

        dm_base = 0

        for res, start, size in soc.resources():
            if res.name == 'debug_module_halted':
                dm_base = start

            print(res.name, hex(start), size)

        m.d.comb += core.debug_entry.eq(dm_base + 0x800)

        return m


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='ROOM SoC simulation')
    parser.add_argument('rom', type=str, help='ROM image')
    parser.add_argument('debug_rom', type=str, help='Debug ROM image')
    parser.add_argument('--ram', type=str, help='RAM image', default=None)
    parser.add_argument('--freq',
                        type=int,
                        help='SoC clock frequency',
                        default=1e6)
    args = parser.parse_args()

    rom = read_mem_image(args.rom)
    debug_rom = read_mem_image(args.debug_rom)

    if args.ram is not None:
        ram = read_mem_image(args.ram)
    else:
        ram = []

    dut = Top(args.freq, core_params, rom, debug_rom, ram, sim_debug=True)

    sim = Simulator(dut)
    sim.add_clock(1.0 / args.freq)
    sim.add_clock(2.0 / args.freq, domain='debug')

    def process():
        yield dut.rst.eq(1)
        yield
        yield dut.rst.eq(0)

        for _ in range(100):
            yield

    def process_sim_debug(cycles=100, log_file=None):

        def proc():
            yield dut.rst.eq(1)
            yield
            yield dut.rst.eq(0)

            for _ in range(cycles):
                for if_debug in dut.core_debug.if_debug:
                    valid = yield if_debug.valid

                    if valid:
                        id = yield if_debug.uop_id
                        pc = yield if_debug.pc
                        inst = yield if_debug.inst
                        print(f'I {id} {pc:x} {inst:x}', file=log_file)

                for id_debug in dut.core_debug.id_debug:
                    valid = yield id_debug.valid

                    if valid:
                        id = yield id_debug.uop_id
                        br_mask = yield id_debug.br_mask
                        print(f'ID {id} {br_mask:x}', file=log_file)

                for ex_debug in dut.core_debug.ex_debug:
                    valid = yield ex_debug.valid

                    if valid:
                        id = yield ex_debug.uop_id
                        opcode = yield ex_debug.opcode
                        prs1 = yield ex_debug.prs1
                        rs1_data = yield ex_debug.rs1_data
                        prs2 = yield ex_debug.prs2
                        rs2_data = yield ex_debug.rs2_data

                        print(
                            f'EX {id} {opcode} {prs1} {rs1_data:x} {prs2} {rs2_data:x}',
                            file=log_file)

                for mem_debug in dut.core_debug.mem_debug:
                    valid = yield mem_debug.valid

                    if valid:
                        id = yield mem_debug.uop_id
                        opcode = yield mem_debug.opcode
                        addr = yield mem_debug.addr
                        data = yield mem_debug.data
                        prs1 = yield mem_debug.prs1
                        prs2 = yield mem_debug.prs2

                        print(
                            f'MEM {id} {opcode} {prs1} {prs2} {addr:x} {data:x}',
                            file=log_file)

                for wb_debug in dut.core_debug.wb_debug:
                    valid = yield wb_debug.valid

                    if valid:
                        id = yield wb_debug.uop_id
                        pdst = yield wb_debug.pdst
                        data = yield wb_debug.data
                        print(f'WB {id} {pdst} {data:x}', file=log_file)

                for com_debug in dut.core_debug.commit_debug:
                    valid = yield com_debug.valid

                    if valid:
                        id = yield com_debug.uop_id
                        print(f'C {id}', file=log_file)

                mispredict_mask = yield dut.core_debug.branch_mispredict
                if mispredict_mask != 0:
                    print(f'BRK {mispredict_mask:x}', file=log_file)

                resolve_mask = yield dut.core_debug.branch_resolve
                if resolve_mask != 0:
                    print(f'BRR {resolve_mask:x}', file=log_file)

                if (yield dut.core_debug.flush_pipeline):
                    print(f'X', file=log_file)

                print('+', file=log_file)

                yield

        return proc

    def process_debug():
        for _ in range(10):
            yield
        yield from dut.jtag.write_dmi(0x10, 1)
        r = yield from dut.jtag.read_dmi(0x11)
        print(hex(r))
        yield from dut.jtag.write_dmi(0x10, 0x80000001)
        for _ in range(100):
            yield
        r = yield from dut.jtag.read_dmi(0x11)
        print(hex(r))

        # Write 0x4 to DCSR
        # yield from dut.jtag.write_dmi(0x4, 0x4)
        # for _ in range(100):
        #     yield
        # yield from dut.jtag.write_dmi(0x17, 0x002307b0)
        # for _ in range(200):
        #     yield

        # Write 0x0 to DPC
        yield from dut.jtag.write_dmi(0x4, 0x39b8)
        for _ in range(100):
            yield
        yield from dut.jtag.write_dmi(0x17, 0x002307b1)
        for _ in range(200):
            yield

        yield from dut.jtag.write_dmi(0x10, 0x40000001)
        for _ in range(100):
            yield
        r = yield from dut.jtag.read_dmi(0x11)
        print(hex(r))
        yield from dut.jtag.write_dmi(0x10, 0x00000001)
        for _ in range(100):
            yield
        r = yield from dut.jtag.read_dmi(0x11)
        print(hex(r))

        # yield from dut.jtag.write_dmi(0x17, 0x00221001)
        # for _ in range(200):
        #     yield
        # r = yield from dut.jtag.read_dmi(0x16)
        # print(hex(r))

    f = open('trace.log', 'w')

    sim.add_sync_process(process_sim_debug(cycles=100, log_file=f))
    # sim.add_sync_process(process)
    # sim.add_sync_process(process_debug, domain='debug')
    with sim.write_vcd('room.vcd'):
        sim.run()

    f.close()
