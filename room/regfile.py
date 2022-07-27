from amaranth import *


class RegFile(Elaboratable):

    def __init__(self, rports, wports, addr_width=5, data_width=32):
        mem = Memory(width=data_width, depth=2**addr_width)

        self.read_ports = [mem.read_port() for _ in range(rports)]
        self.write_ports = [mem.write_port() for _ in range(wports)]

    def elaborate(self, platform):
        m = Module()

        for i, port in enumerate(self.read_ports):
            setattr(m.submodules, f'read_port{i}', port)

        for i, port in enumerate(self.write_ports):
            setattr(m.submodules, f'write_port{i}', port)

        return m
