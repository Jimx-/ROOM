from amaranth import *

from vroom.types import HasVectorParams, VMicroOp

from room.regfile import RFReadPort, RFWritePort


class RegisterRead(HasVectorParams, Elaboratable):

    def __init__(self, num_rports, rports_array, reg_width, params):
        super().__init__(params)

        self.rports_array = rports_array
        self.data_width = reg_width

        self.dis_valid = Signal()
        self.dis_uop = VMicroOp(params)
        self.dis_ready = Signal()

        self.read_ports = [
            RFReadPort(5, self.data_width, name=f'read_port{i}')
            for i in range(num_rports)
        ]

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.dis_ready.eq(1)

        return m
