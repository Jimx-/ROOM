#!/usr/bin/env python
import itertools
import logging
import struct

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge
from cocotb.regression import TestFactory

from cocotbext.axi import AxiLiteBus, AxiLiteMaster, AxiBus, AxiRam
from cocotbext.axi import AxiStreamFrame, AxiStreamBus, AxiStreamSource, AxiStreamSink


class TB:

    def __init__(self, dut):
        self.dut = dut

        self.log = logging.getLogger("cocotb.tb")
        self.log.setLevel(logging.DEBUG)

        cocotb.fork(Clock(dut.sys_clk, 10, units="ns").start())

        self.axi_ram = AxiRam(AxiBus.from_prefix(dut, "instr_axi"),
                              dut.sys_clk,
                              dut.sys_rst,
                              size=2**16)

    def set_idle_generator(self, generator=None):
        if generator:
            self.axi_ram.write_if.b_channel.set_pause_generator(generator())
            self.axi_ram.read_if.r_channel.set_pause_generator(generator())

    def set_backpressure_generator(self, generator=None):
        if generator:
            self.axi_ram.write_if.aw_channel.set_pause_generator(generator())
            self.axi_ram.write_if.w_channel.set_pause_generator(generator())
            self.axi_ram.read_if.ar_channel.set_pause_generator(generator())

    async def reset(self):
        self.dut.sys_rst.setimmediatevalue(0)
        self.dut.sys_rst <= 1
        for _ in range(4):
            await RisingEdge(self.dut.sys_clk)
        self.dut.sys_rst <= 0
        await RisingEdge(self.dut.sys_clk)


async def wrapper_test(dut, idle_inserter=None, backpressure_inserter=None):
    tb = TB(dut)

    tb.set_idle_generator(idle_inserter)
    tb.set_backpressure_generator(backpressure_inserter)

    tb.axi_ram.write_dword(0, 0x697)
    for i in range(4, 128, 4):
        tb.axi_ram.write_dword(i, 0x0f868693)

    await tb.reset()

    for _ in range(128):
        await RisingEdge(tb.dut.sys_clk)


def cycle_pause():
    return itertools.cycle([1, 1, 1, 0])


if cocotb.SIM_NAME:
    factory = TestFactory(wrapper_test)
    factory.add_option("idle_inserter", [None, cycle_pause])
    factory.add_option("backpressure_inserter", [None, cycle_pause])
    factory.generate_tests()
