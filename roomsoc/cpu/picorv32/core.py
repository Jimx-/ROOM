from amaranth import *

from roomsoc.interconnect import wishbone


class PicoRV32(Elaboratable):
    io_regions = {0x8000_0000: 0x8000_0000}

    def __init__(self, reset_address, variant='standard'):
        self.reset_address = reset_address
        self.variant = variant

        self.idbus = wishbone.Interface(data_width=32,
                                        addr_width=30,
                                        granularity=8)

        self.reset = Signal()
        self.trap = Signal()
        self.interrupt = Signal()

        self.periph_buses = [self.idbus]

    def elaborate(self, platform):
        m = Module()

        mem_valid = Signal()
        mem_instr = Signal()
        mem_ready = Signal()
        mem_addr = Signal(32)
        mem_wdata = Signal(32)
        mem_wstrb = Signal(4)
        mem_rdata = Signal(32)

        cpu_params = dict(
            p_PROGADDR_RESET=self.reset_address,
            p_PROGADDR_IRQ=self.reset_address + 0x0000_0010,
            p_ENABLE_COUNTERS=1,
            p_ENABLE_COUNTERS64=1,
            p_ENABLE_REGS_16_31=1,
            p_ENABLE_REGS_DUALPORT=1,
            p_LATCHED_MEM_RDATA=0,
            p_TWO_STAGE_SHIFT=1,
            p_TWO_CYCLE_COMPARE=0,
            p_TWO_CYCLE_ALU=0,
            p_CATCH_MISALIGN=1,
            p_CATCH_ILLINSN=1,
            p_ENABLE_PCPI=0,
            p_ENABLE_MUL=1,
            p_ENABLE_DIV=1,
            p_ENABLE_FAST_MUL=0,
            p_ENABLE_IRQ=1,
            p_ENABLE_IRQ_QREGS=1,
            p_ENABLE_IRQ_TIMER=1,
            p_ENABLE_TRACE=0,
            p_MASKED_IRQ=0x00000000,
            p_LATCHED_IRQ=0xffffffff,
            p_STACKADDR=0xffffffff,
        )

        if self.variant == 'minimal':
            cpu_params.update(
                p_ENABLE_COUNTERS=0,
                p_ENABLE_COUNTERS64=0,
                p_TWO_STAGE_SHIFT=0,
                p_CATCH_MISALIGN=0,
                p_ENABLE_MUL=0,
                p_ENABLE_DIV=0,
                p_ENABLE_IRQ_TIMER=0,
            )

        cpu_params.update(i_clk=ClockSignal(),
                          i_resetn=~(ResetSignal() | self.reset),
                          o_trap=self.trap,
                          o_mem_valid=mem_valid,
                          o_mem_instr=mem_instr,
                          i_mem_ready=mem_ready,
                          o_mem_addr=mem_addr,
                          o_mem_wdata=mem_wdata,
                          o_mem_wstrb=mem_wstrb,
                          i_mem_rdata=mem_rdata,
                          o_mem_la_read=Signal(),
                          o_mem_la_write=Signal(),
                          o_mem_la_addr=Signal(32),
                          o_mem_la_wdata=Signal(32),
                          o_mem_la_wstrb=Signal(4),
                          o_pcpi_valid=Signal(),
                          o_pcpi_insn=Signal(32),
                          o_pcpi_rs1=Signal(32),
                          o_pcpi_rs2=Signal(32),
                          i_pcpi_wr=0,
                          i_pcpi_rd=0,
                          i_pcpi_wait=0,
                          i_pcpi_ready=0,
                          i_irq=self.interrupt,
                          o_eoi=Signal(32))

        m.submodules.core = Instance(
            "picorv32",
            **cpu_params,
        )

        m.d.comb += [
            self.idbus.adr.eq(mem_addr[2:]),
            self.idbus.dat_w.eq(mem_wdata),
            self.idbus.we.eq(mem_wstrb != 0),
            self.idbus.sel.eq(
                Mux(mem_wstrb != 0, mem_wstrb, mem_valid.replicate(4))),
            self.idbus.cyc.eq(mem_valid),
            self.idbus.stb.eq(mem_valid),
            mem_ready.eq(self.idbus.ack),
            mem_rdata.eq(self.idbus.dat_r),
        ]

        return m
