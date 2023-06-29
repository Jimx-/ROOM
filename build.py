from amaranth import *
from amaranth.build import *
from amaranth.utils import log2_int

from amaranth_soc.memory import MemoryMap

from room.consts import *
from room import Core

from roomsoc.soc import SoC
from roomsoc.interconnect import axi, wishbone, tilelink as tl
from roomsoc.peripheral.l2cache import L2Cache
from roomsoc.peripheral.uart import UART
from roomsoc.peripheral.debug import JTAGInterface, DebugModule
from roomsoc.peripheral.sdc import SDController
from roomsoc.platform.kc705 import KC705Platform

import argparse
import struct
import os

from jinja2 import FileSystemLoader, Environment


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
    core_width=2,
    num_int_pregs=96,
    num_fp_pregs=64,
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
        IssueQueueType.FP: dict(dispatch_width=2,
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
        n_banks=4,
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
        sink_id_width=4,
        size_width=3,
    ),
    out_bus=dict(
        source_id_width=4,
        sink_id_width=1,
    ),
    client_source_map={0: (0, 7)},
)


def generate_trace_if(m, core, output_dir):
    signals = dict(
        i_clk=ClockSignal(),
        i_rst=ResetSignal(),
    )

    for i, if_debug in enumerate(core.core_debug.if_debug):
        signals[f'i_if_debug{i}_valid'] = if_debug.valid
        signals[f'i_if_debug{i}_uop_id'] = if_debug.bits.uop_id
        signals[f'i_if_debug{i}_pc'] = if_debug.bits.pc
        signals[f'i_if_debug{i}_inst'] = if_debug.bits.inst

    for i, id_debug in enumerate(core.core_debug.id_debug):
        signals[f'i_id_debug{i}_valid'] = id_debug.valid
        signals[f'i_id_debug{i}_uop_id'] = id_debug.bits.uop_id
        signals[f'i_id_debug{i}_br_mask'] = id_debug.bits.br_mask

    for i, ex_debug in enumerate(core.core_debug.ex_debug):
        signals[f'i_ex_debug{i}_valid'] = ex_debug.valid
        signals[f'i_ex_debug{i}_uop_id'] = ex_debug.bits.uop_id
        signals[f'i_ex_debug{i}_opcode'] = ex_debug.bits.opcode
        signals[f'i_ex_debug{i}_prs1'] = ex_debug.bits.prs1
        signals[f'i_ex_debug{i}_rs1_data'] = ex_debug.bits.rs1_data
        signals[f'i_ex_debug{i}_prs2'] = ex_debug.bits.prs2
        signals[f'i_ex_debug{i}_rs2_data'] = ex_debug.bits.rs2_data

    for i, mem_debug in enumerate(core.core_debug.mem_debug):
        signals[f'i_mem_debug{i}_valid'] = mem_debug.valid
        signals[f'i_mem_debug{i}_uop_id'] = mem_debug.bits.uop_id
        signals[f'i_mem_debug{i}_opcode'] = mem_debug.bits.opcode
        signals[f'i_mem_debug{i}_addr'] = mem_debug.bits.addr
        signals[f'i_mem_debug{i}_data'] = mem_debug.bits.data
        signals[f'i_mem_debug{i}_prs1'] = mem_debug.bits.prs1
        signals[f'i_mem_debug{i}_prs2'] = mem_debug.bits.prs2

    for i, wb_debug in enumerate(core.core_debug.wb_debug):
        signals[f'i_wb_debug{i}_valid'] = wb_debug.valid
        signals[f'i_wb_debug{i}_uop_id'] = wb_debug.bits.uop_id
        signals[f'i_wb_debug{i}_pdst'] = wb_debug.bits.pdst
        signals[f'i_wb_debug{i}_data'] = wb_debug.bits.data

    for i, com_debug in enumerate(core.core_debug.commit_debug):
        signals[f'i_commit_debug{i}_valid'] = com_debug.valid
        signals[f'i_commit_debug{i}_uop_id'] = com_debug.bits.uop_id

    signals['i_branch_resolve'] = core.core_debug.branch_resolve
    signals['i_branch_mispredict'] = core.core_debug.branch_mispredict
    signals['i_flush_pipeline'] = core.core_debug.flush_pipeline

    m.submodules.trace_if = Instance('vl_trace_if', **signals)

    env = Environment(loader=FileSystemLoader(searchpath='rtl'))
    config = dict(
        core=core,
        UOP_ID_WIDTH=10,
    )

    template = env.get_template('vl_trace_if.v.tmpl')
    output = template.render(config)

    with open(os.path.join(output_dir, 'vl_trace_if.v'), 'w') as fout:
        fout.write(output)
        fout.write('\n')


class DromajoRAM(Elaboratable):

    def __init__(self, addr_width, data_width=32, ram_base=0x80000000):
        self.addr_width = addr_width
        self.data_width = data_width
        self.ram_base = ram_base

        self.bus = wishbone.Interface(addr_width=addr_width,
                                      data_width=data_width,
                                      granularity=8)
        self.bus.memory_map = MemoryMap(data_width=8,
                                        addr_width=addr_width +
                                        log2_int(data_width // 8))

    def elaborate(self, platform):
        m = Module()

        m.submodules.ram = Instance(
            'dromajo_ram',
            p_ADDR_WIDTH=self.addr_width,
            p_DATA_WIDTH=self.data_width,
            p_RAM_BASE=self.ram_base,
            #
            i_clk_i=ClockSignal(),
            i_rst_i=ResetSignal(),
            i_adr_i=self.bus.adr,
            i_dat_i=self.bus.dat_w,
            i_sel_i=self.bus.sel,
            i_we_i=self.bus.we,
            i_cyc_i=self.bus.cyc,
            i_stb_i=self.bus.stb,
            #
            o_ack_o=self.bus.ack,
            o_dat_o=self.bus.dat_r,
        )

        return m


class JTAGDPI(Elaboratable):

    def __init__(self):
        self.jtag = JTAGInterface()

    def elaborate(self, platform):
        m = Module()

        jtag_trst_n = Signal()
        jtag_srst_n = Signal()

        m.submodules.jtag_dpi = Instance(
            'jtagdpi',
            #
            i_clk_i=ClockSignal(),
            i_rst_ni=ResetSignal(),
            i_jtag_tdo=self.jtag.tdo,
            #
            o_jtag_tms=self.jtag.tms,
            o_jtag_tck=self.jtag.tck,
            o_jtag_tdi=self.jtag.tdi,
            o_jtag_trst_n=jtag_trst_n,
            o_jtag_srst_n=jtag_srst_n,
        )

        m.d.comb += self.jtag.trst.eq(~jtag_trst_n)

        return m


class Top(Elaboratable):

    mem_map = {
        'rom': 0x00010000,
        'sram': 0x80000000,
    }

    def __init__(self,
                 clk_freq,
                 rom_image,
                 debug_rom_image,
                 ram_image=[],
                 sim=False):
        self.rom_image = rom_image
        self.debug_rom_image = debug_rom_image
        self.ram_image = ram_image
        self.clk_freq = clk_freq
        self.sim = sim

        self.axil_master = axi.AXILiteInterface(data_width=64,
                                                addr_width=29,
                                                name='axil_master')

        self.jtag = JTAGInterface()

        self.uart = UART(divisor=int(self.clk_freq // 115200))

        self.sdc = SDController()

    def elaborate(self, platform):
        m = Module()

        soc = m.submodules.soc = SoC(bus_data_width=64, bus_addr_width=32)

        core = Core(core_params, sim_debug=self.sim)
        soc.add_cpu(core)
        m.d.comb += core.reset_vector.eq(0x10000)

        if self.sim:
            generate_trace_if(m, core, '/tmp')

        debug_module = DebugModule(self.debug_rom_image)
        m.d.comb += core.interrupts.debug.eq(debug_module.debug_int)

        if self.sim:
            jtag_dpi = m.submodules.jtag_dpi = JTAGDPI()
            m.d.comb += debug_module.jtag.connect(jtag_dpi.jtag)
        else:
            m.d.comb += debug_module.jtag.connect(self.jtag)

        soc.bus.add_master(name='axil_master', master=self.axil_master)
        soc.bus.add_master(name='dm_master', master=debug_module.dbus)

        soc.add_rom(name='rom',
                    origin=self.mem_map['rom'],
                    size=0x1000,
                    init=self.rom_image,
                    mode='rw')

        soc.add_peripheral('dm', debug_module)

        if l2cache_params is not None:
            l2c_in_bus = tl.Interface(data_width=core.xlen,
                                      addr_width=32,
                                      size_width=3,
                                      source_id_width=4,
                                      sink_id_width=4,
                                      has_bce=True)

            mmio_bus = tl.Interface(data_width=core.xlen,
                                    addr_width=32,
                                    size_width=3,
                                    source_id_width=4,
                                    sink_id_width=4,
                                    has_bce=True)

            mmio_valid = Signal()
            for origin, size in core.io_regions.items():
                with m.If((core.core_bus.a.bits.address >= origin)
                          & (core.core_bus.a.bits.address < (origin + size))):
                    m.d.comb += mmio_valid.eq(1)

            m.d.comb += [
                core.core_bus.connect(mmio_bus),
                core.core_bus.connect(l2c_in_bus),
                l2c_in_bus.a.valid.eq(core.core_bus.a.valid & ~mmio_valid),
                mmio_bus.a.valid.eq(core.core_bus.a.valid & mmio_valid),
                core.core_bus.a.ready.eq(
                    Mux(mmio_valid, mmio_bus.a.ready, l2c_in_bus.a.ready)),
                core.core_bus.d.valid.eq(l2c_in_bus.d.valid
                                         | mmio_bus.d.valid),
                mmio_bus.d.ready.eq(core.core_bus.d.ready),
                l2c_in_bus.d.ready.eq(core.core_bus.d.ready
                                      & ~mmio_bus.d.valid),
                mmio_bus.c.valid.eq(0),
                mmio_bus.e.valid.eq(0),
            ]

            with m.If(mmio_bus.d.valid):
                m.d.comb += core.core_bus.d.bits.eq(mmio_bus.d.bits)
            with m.Else():
                m.d.comb += core.core_bus.d.bits.eq(l2c_in_bus.d.bits)

            l2cache = L2Cache(l2cache_params)
            m.d.comb += l2c_in_bus.connect(l2cache.in_bus)
            soc.bus.add_master(name='l2c_dbus', master=l2cache.out_bus)
            soc.bus.add_master(name='cpu_mmio_bus', master=mmio_bus)
            soc.add_peripheral('l2cache', l2cache)
        else:
            soc.bus.add_master(name='cpu_ibus', master=core.dbus)
            soc.bus.add_master(name='cpu_dbus', master=core.dbus)

        if self.sim:
            sram = DromajoRAM(addr_width=25,
                              data_width=64,
                              ram_base=self.mem_map['sram'])
            soc.add_peripheral('sram',
                               sram,
                               origin=self.mem_map['sram'],
                               cacheable=True)
        else:
            soc.add_ram(name='sram',
                        origin=self.mem_map['sram'],
                        size=0x20000,
                        init=self.ram_image)

        soc.add_controller()

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
    parser.add_argument('--sim', action='store_true')
    args = parser.parse_args()

    rom = read_mem_image(args.rom, word_len=64)
    debug_rom = read_mem_image(args.debug_rom)

    if args.ram is not None:
        ram = read_mem_image(args.ram, word_len=64)
    else:
        ram = [0x6f]

    top = Top(args.freq,
              rom_image=rom,
              debug_rom_image=debug_rom,
              ram_image=ram,
              sim=args.sim)

    platform = None
    if not args.sim:
        platform = KC705Platform()

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
                            platform=platform,
                            name='soc_wrapper'))
