from amaranth import *
from amaranth.sim import Simulator

from room.consts import *
from room.core import Core, CoreDebug

from roomsoc.soc import SoC
from roomsoc.interconnect import axi
from roomsoc.peripheral.l2cache import L2Cache
from roomsoc.peripheral.uart import UART
from roomsoc.peripheral.sdc import MockSDController
from roomsoc.peripheral.debug import JTAGInterface, DebugModule

import argparse
import struct

from room.dcache import MSHRReq


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
    dcache_params=dict(
        n_sets=64,
        n_ways=4,
        block_bytes=64,
        row_bits=64,
        n_mshrs=1,
        n_iomshrs=1,
        sdq_size=17,
        rpq_size=16,
        n_data_banks=1,
    ),
    use_fpu=True,
    flen=64,
    fma_latency=4,
    io_regions={0xC0000000: 0x40000000},
)

l2cache_params = dict(
    capacity_kb=64,
    n_ways=8,
    block_bytes=64,
    in_bus=dict(
        source_id_width=4,
        sink_id_width=2,
        size_width=3,
    ),
    out_bus=dict(source_id_width=4, ),
)


class Top(Elaboratable):

    mem_map = {
        'rom': 0x00010000,
        'sram': 0x80000000,
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
        self.core_params = core_params
        self.sim_debug = sim_debug

        self.axil_master = axi.AXILiteInterface(data_width=64,
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

        soc = m.submodules.soc = SoC(bus_data_width=64, bus_addr_width=32)

        core = Core(self.core_params, sim_debug=self.sim_debug)
        soc.add_cpu(core)
        m.d.comb += core.reset_vector.eq(0x10000)

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
                    size=0x1000,
                    init=self.rom_image)

        soc.add_peripheral('dm', debug_module)

        if l2cache_params is not None:
            l2cache = L2Cache(l2cache_params)
            m.d.comb += core.dbus.connect(l2cache.in_bus)
            soc.bus.add_master(name='l2c_dbus', master=l2cache.out_bus)
            soc.add_peripheral('l2cache', l2cache)
        else:
            soc.bus.add_master(name='cpu_dbus', master=core.dbus)

        soc.add_ram(name='sram',
                    origin=self.mem_map['sram'],
                    size=0x20000,
                    init=self.ram_image)

        soc.add_controller()

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

    rom = read_mem_image(args.rom, word_len=64)
    debug_rom = read_mem_image(args.debug_rom)

    if args.ram is not None:
        ram = read_mem_image(args.ram, word_len=64)
    else:
        ram = [0x6f]  # j 0

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
                        id = yield if_debug.bits.uop_id
                        pc = yield if_debug.bits.pc
                        inst = yield if_debug.bits.inst
                        print(f'I {id} {pc:x} {inst:x}', file=log_file)

                for id_debug in dut.core_debug.id_debug:
                    valid = yield id_debug.valid

                    if valid:
                        id = yield id_debug.bits.uop_id
                        br_mask = yield id_debug.bits.br_mask
                        print(f'ID {id} {br_mask:x}', file=log_file)

                for ex_debug in dut.core_debug.ex_debug:
                    valid = yield ex_debug.valid

                    if valid:
                        id = yield ex_debug.bits.uop_id
                        opcode = yield ex_debug.bits.opcode
                        prs1 = yield ex_debug.bits.prs1
                        rs1_data = yield ex_debug.bits.rs1_data
                        prs2 = yield ex_debug.bits.prs2
                        rs2_data = yield ex_debug.bits.rs2_data

                        print(
                            f'EX {id} {opcode} {prs1} {rs1_data:x} {prs2} {rs2_data:x}',
                            file=log_file)

                for mem_debug in dut.core_debug.mem_debug:
                    valid = yield mem_debug.valid

                    if valid:
                        id = yield mem_debug.bits.uop_id
                        opcode = yield mem_debug.bits.opcode
                        addr = yield mem_debug.bits.addr
                        data = yield mem_debug.bits.data
                        prs1 = yield mem_debug.bits.prs1
                        prs2 = yield mem_debug.bits.prs2

                        print(
                            f'MEM {id} {opcode} {prs1} {prs2} {addr:x} {data:x}',
                            file=log_file)

                for wb_debug in dut.core_debug.wb_debug:
                    valid = yield wb_debug.valid

                    if valid:
                        id = yield wb_debug.bits.uop_id
                        pdst = yield wb_debug.bits.pdst
                        data = yield wb_debug.bits.data
                        print(f'WB {id} {pdst} {data:x}', file=log_file)

                for com_debug in dut.core_debug.commit_debug:
                    valid = yield com_debug.valid

                    if valid:
                        id = yield com_debug.bits.uop_id
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

    sim.add_sync_process(process_sim_debug(cycles=300, log_file=f))
    # sim.add_sync_process(process)
    # sim.add_sync_process(process_debug, domain='debug')
    with sim.write_vcd('room.vcd'):
        sim.run()

    f.close()
