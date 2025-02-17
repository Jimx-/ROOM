from amaranth import *
from amaranth.build import *
from amaranth.utils import log2_int

from amaranth_soc.memory import MemoryMap

from room.consts import *
from room import Core

from roomsoc.soc import SoC
from roomsoc.interconnect import axi, wishbone, tilelink as tl
from roomsoc.interconnect.stream import Decoupled
from roomsoc.peripheral.l2cache import L2Cache
from roomsoc.peripheral.uart import UART
from roomsoc.peripheral.debug import JTAGInterface, DebugModule
from roomsoc.peripheral.sdc import SDController
from roomsoc.peripheral.clint import CLINT
from roomsoc.peripheral.plic import PLIC
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
    vaddr_bits=39,
    paddr_bits=36,
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
        n_tlb_sets=16,
        n_tlb_ways=4,
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
    n_dtlb_sets=16,
    n_dtlb_ways=4,
    use_fpu=True,
    flen=64,
    fma_latency=4,
    io_regions={0xC0000000: 0x40000000},
    use_vm=True,
    use_user=True,
    use_supervisor=True,
    pg_levels=3,
    use_bpd=True,
    ghist_length=64,
    bpd_meta_length=120,
    n_ras_entries=16,
    tage_params=dict(
        table_params=[
            (128, 2, 7),
            (128, 4, 7),
            (256, 8, 8),
            (256, 16, 8),
            (128, 32, 9),
            (128, 64, 9),
        ],
        u_bit_period=2048,
    ),
)

l2cache_params = dict(
    capacity_kb=64,
    n_ways=8,
    block_bytes=64,
    in_bus=dict(
        source_id_width=7,
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

    if hasattr(core.core_debug, 'fp_wb_debug'):
        for i, wb_debug in enumerate(core.core_debug.fp_wb_debug):
            signals[f'i_fp_wb_debug{i}_valid'] = wb_debug.valid
            signals[f'i_fp_wb_debug{i}_uop_id'] = wb_debug.bits.uop_id
            signals[f'i_fp_wb_debug{i}_pdst'] = wb_debug.bits.pdst
            signals[f'i_fp_wb_debug{i}_data'] = wb_debug.bits.data

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

        self.axil_master = axi.AXILiteInterface(data_width=32, addr_width=32)
        self.axi_master = axi.AXIInterface(data_width=64,
                                           addr_width=32,
                                           id_width=4)

        self.ram_bus = axi.AXIInterface(data_width=64,
                                        addr_width=30,
                                        id_width=6)

        self.jtag = JTAGInterface()

        self.uart = UART(divisor=int(self.clk_freq // 115200))

        self.sdc = SDController()

    def elaborate(self, platform):
        m = Module()

        soc = m.submodules.soc = SoC(bus_data_width=64,
                                     bus_addr_width=32,
                                     bus_standard='axi')

        core = Core(core_params, sim_debug=self.sim)
        soc.add_cpu(core)
        m.d.comb += core.reset_vector.eq(0x10000)

        clint = CLINT(n_harts=1)
        m.d.comb += [
            core.interrupts.msip.eq(clint.msip[0]),
            core.interrupts.mtip.eq(clint.mtip[0]),
            clint.rtc_tick.eq(1),
        ]

        plic = PLIC(n_harts=2, n_devices=7)
        m.d.comb += [
            core.interrupts.meip.eq(plic.hart_ints[0]),
            core.interrupts.seip.eq(plic.hart_ints[1]),
        ]

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
            axi_frag = m.submodules.axi_frag = axi.AXIFragmenter(
                self.axi_master, max_size=64, max_flights=2)
            axi_bus = axi_frag.out_bus
            axi_tl = axi.AXI2Tilelink.get_adapted_interface(axi_bus,
                                                            max_flights=2)
            m.submodules.axi_bridge = axi.AXI2Tilelink(axi_bus,
                                                       axi_tl,
                                                       max_flights=2)
            tl_serializer = m.submodules.tl_serializer = tl.Serializer(axi_tl)

            l2cache = L2Cache(l2cache_params)

            #
            # Cached & uncached core bus
            #

            core_l2c_bus = tl.Interface(data_width=core.xlen,
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
                core.core_bus.connect(core_l2c_bus),
                core_l2c_bus.a.valid.eq(core.core_bus.a.valid & ~mmio_valid),
                mmio_bus.a.valid.eq(core.core_bus.a.valid & mmio_valid),
                core.core_bus.a.ready.eq(
                    Mux(mmio_valid, mmio_bus.a.ready, core_l2c_bus.a.ready)),
                core.core_bus.d.valid.eq(core_l2c_bus.d.valid
                                         | mmio_bus.d.valid),
                mmio_bus.d.ready.eq(core.core_bus.d.ready),
                core_l2c_bus.d.ready.eq(core.core_bus.d.ready
                                        & ~mmio_bus.d.valid),
                mmio_bus.c.valid.eq(0),
                mmio_bus.e.valid.eq(0),
            ]

            with m.If(mmio_bus.d.valid):
                m.d.comb += core.core_bus.d.bits.eq(mmio_bus.d.bits)
            with m.Else():
                m.d.comb += core.core_bus.d.bits.eq(core_l2c_bus.d.bits)

            #
            # AXI bus master & cached core bus
            #

            a_arbiter = m.submodules.a_arbiter = tl.Arbiter(
                tl.ChannelA,
                data_width=64,
                addr_width=32,
                size_width=3,
                source_id_width=l2cache.in_source_id_width)

            bus_master_a = Decoupled(
                tl.ChannelA,
                data_width=64,
                addr_width=32,
                size_width=3,
                source_id_width=l2cache.in_source_id_width)

            m.d.comb += [
                tl_serializer.out_bus.a.connect(bus_master_a),
                bus_master_a.bits.source[-1].eq(1),
            ]

            a_arbiter.add(core_l2c_bus.a)
            a_arbiter.add(bus_master_a)

            m.d.comb += [
                a_arbiter.bus.connect(l2cache.in_bus.a),
                l2cache.in_bus.b.connect(core_l2c_bus.b),
                core_l2c_bus.c.connect(l2cache.in_bus.c),
                core_l2c_bus.e.connect(l2cache.in_bus.e),
            ]

            with m.If(l2cache.in_bus.d.bits.source[-1]):
                m.d.comb += l2cache.in_bus.d.connect(tl_serializer.out_bus.d)
            with m.Else():
                m.d.comb += l2cache.in_bus.d.connect(core_l2c_bus.d)

            soc.bus.add_master(name='l2c_dbus', master=l2cache.out_bus)
            soc.bus.add_master(name='cpu_mmio_bus', master=mmio_bus)
            soc.add_peripheral('l2cache', l2cache)
        elif core.core_bus is not None:
            soc.bus.add_master(name='cpu_bus', master=core.core_bus)
        else:
            soc.bus.add_master(name='cpu_ibus', master=core.ibus)
            soc.bus.add_master(name='cpu_dbus', master=core.dbus)

        soc.add_bus(name='sram',
                    bus=self.ram_bus,
                    origin=self.mem_map['sram'],
                    size=0x40000000)

        soc.add_controller()

        soc.add_peripheral('uart', self.uart)
        soc.add_peripheral('sdc', self.sdc)

        soc.add_peripheral('clint', clint)

        soc.add_peripheral('plic', plic)

        core.pma_regions = list(soc.regions())

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
    parser.add_argument('--dtb',
                        type=str,
                        help='Path to device tree blob',
                        default=None)
    parser.add_argument('--sim', action='store_true')
    args = parser.parse_args()

    rom = read_mem_image(args.rom, word_len=64)
    debug_rom = read_mem_image(args.debug_rom)

    if args.ram is not None:
        ram = read_mem_image(args.ram, word_len=64)
    else:
        ram = [0x6f]

    if args.dtb is not None:
        dtb = read_mem_image(args.dtb, word_len=64)
        rom += [0] * (32 - len(rom))
        rom += dtb

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
            verilog.convert(
                top,
                ports=[
                    top.axil_master.aw.addr,
                    top.axil_master.aw.prot,
                    top.axil_master.aw.valid,
                    top.axil_master.aw.ready,
                    top.axil_master.w.data,
                    top.axil_master.w.strb,
                    top.axil_master.w.valid,
                    top.axil_master.w.ready,
                    top.axil_master.b.resp,
                    top.axil_master.b.valid,
                    top.axil_master.b.ready,
                    top.axil_master.ar.addr,
                    top.axil_master.ar.prot,
                    top.axil_master.ar.valid,
                    top.axil_master.ar.ready,
                    top.axil_master.r.data,
                    top.axil_master.r.resp,
                    top.axil_master.r.valid,
                    top.axil_master.r.ready,
                    #
                    top.axi_master.aw.bits.addr,
                    top.axi_master.aw.bits.burst,
                    top.axi_master.aw.bits.len,
                    top.axi_master.aw.bits.size,
                    top.axi_master.aw.bits.lock,
                    top.axi_master.aw.bits.prot,
                    top.axi_master.aw.bits.cache,
                    top.axi_master.aw.bits.qos,
                    top.axi_master.aw.bits.region,
                    top.axi_master.aw.bits.id,
                    top.axi_master.aw.bits.user,
                    top.axi_master.aw.valid,
                    top.axi_master.aw.ready,
                    top.axi_master.w.bits.data,
                    top.axi_master.w.bits.strb,
                    top.axi_master.w.bits.user,
                    top.axi_master.w.bits.last,
                    top.axi_master.w.valid,
                    top.axi_master.w.ready,
                    top.axi_master.b.bits.resp,
                    top.axi_master.b.bits.id,
                    top.axi_master.b.bits.user,
                    top.axi_master.b.valid,
                    top.axi_master.b.ready,
                    top.axi_master.ar.bits.addr,
                    top.axi_master.ar.bits.burst,
                    top.axi_master.ar.bits.len,
                    top.axi_master.ar.bits.size,
                    top.axi_master.ar.bits.lock,
                    top.axi_master.ar.bits.prot,
                    top.axi_master.ar.bits.cache,
                    top.axi_master.ar.bits.qos,
                    top.axi_master.ar.bits.region,
                    top.axi_master.ar.bits.id,
                    top.axi_master.ar.bits.user,
                    top.axi_master.ar.valid,
                    top.axi_master.ar.ready,
                    top.axi_master.r.bits.resp,
                    top.axi_master.r.bits.id,
                    top.axi_master.r.bits.user,
                    top.axi_master.r.bits.data,
                    top.axi_master.r.bits.last,
                    top.axi_master.r.valid,
                    top.axi_master.r.ready,
                    #
                    top.ram_bus.aw.bits.addr,
                    top.ram_bus.aw.bits.burst,
                    top.ram_bus.aw.bits.len,
                    top.ram_bus.aw.bits.size,
                    top.ram_bus.aw.bits.lock,
                    top.ram_bus.aw.bits.prot,
                    top.ram_bus.aw.bits.cache,
                    top.ram_bus.aw.bits.qos,
                    top.ram_bus.aw.bits.region,
                    top.ram_bus.aw.bits.id,
                    top.ram_bus.aw.bits.user,
                    top.ram_bus.aw.valid,
                    top.ram_bus.aw.ready,
                    top.ram_bus.w.bits.data,
                    top.ram_bus.w.bits.strb,
                    top.ram_bus.w.bits.user,
                    top.ram_bus.w.bits.last,
                    top.ram_bus.w.valid,
                    top.ram_bus.w.ready,
                    top.ram_bus.b.bits.resp,
                    top.ram_bus.b.bits.id,
                    top.ram_bus.b.bits.user,
                    top.ram_bus.b.valid,
                    top.ram_bus.b.ready,
                    top.ram_bus.ar.bits.addr,
                    top.ram_bus.ar.bits.burst,
                    top.ram_bus.ar.bits.len,
                    top.ram_bus.ar.bits.size,
                    top.ram_bus.ar.bits.lock,
                    top.ram_bus.ar.bits.prot,
                    top.ram_bus.ar.bits.cache,
                    top.ram_bus.ar.bits.qos,
                    top.ram_bus.ar.bits.region,
                    top.ram_bus.ar.bits.id,
                    top.ram_bus.ar.bits.user,
                    top.ram_bus.ar.valid,
                    top.ram_bus.ar.ready,
                    top.ram_bus.r.bits.resp,
                    top.ram_bus.r.bits.id,
                    top.ram_bus.r.bits.user,
                    top.ram_bus.r.bits.data,
                    top.ram_bus.r.bits.last,
                    top.ram_bus.r.valid,
                    top.ram_bus.r.ready,
                    #
                    top.jtag.tck,
                    top.jtag.tdi,
                    top.jtag.tdo,
                    top.jtag.tms,
                    #
                    top.uart.tx,
                    top.uart.rx,
                    #
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
