from amaranth import *

from groom.fu import ExecReq

from room.types import HasCoreParams, MicroOp
from room.regfile import RFReadPort, RFWritePort, RegReadDecoder as CommonDecoder

from roomsoc.interconnect.stream import Decoupled, SkidBuffer


class RegisterFile(Elaboratable):

    def __init__(self, rports, wports, num_regs=32, data_width=32):
        self.num_regs = num_regs
        addr_width = Shape.cast(range(num_regs)).width
        self.data_width = data_width

        self.read_ports = [
            RFReadPort(addr_width, self.data_width, name=f'read_port{i}')
            for i in range(rports)
        ]

        self.write_ports = [
            Decoupled(RFWritePort,
                      addr_width,
                      self.data_width,
                      name=f'write_port{i}') for i in range(wports)
        ]

    def elaborate(self, platform):
        m = Module()

        n_rports = len(self.read_ports)
        n_wports = len(self.write_ports)

        for i, wport in enumerate(self.write_ports):
            rp_base = i * n_rports // n_wports

            m.d.comb += wport.ready.eq(1)

            mem1 = Memory(width=self.data_width, depth=self.num_regs)
            mem1_rport = mem1.read_port(domain='comb')
            setattr(m.submodules, f'mem_read_port{rp_base}', mem1_rport)
            mem1_wport = mem1.write_port()
            setattr(m.submodules, f'mem1_write_port{i}', mem1_wport)

            m.d.comb += [
                mem1_rport.addr.eq(self.read_ports[rp_base].addr),
                self.read_ports[rp_base].data.eq(mem1_rport.data),
                mem1_wport.addr.eq(wport.bits.addr),
                mem1_wport.data.eq(wport.bits.data),
                mem1_wport.en.eq(wport.valid),
            ]

            mem2 = Memory(width=self.data_width, depth=self.num_regs)
            mem2_rport = mem2.read_port(domain='comb')
            setattr(m.submodules, f'mem_read_port{rp_base + 1}', mem2_rport)
            mem2_wport = mem2.write_port()
            setattr(m.submodules, f'mem2_write_port{i}', mem2_wport)

            m.d.comb += [
                mem2_rport.addr.eq(self.read_ports[rp_base + 1].addr),
                self.read_ports[rp_base + 1].data.eq(mem2_rport.data),
                mem2_wport.addr.eq(wport.bits.addr),
                mem2_wport.data.eq(wport.bits.data),
                mem2_wport.en.eq(wport.valid),
            ]

            if n_rports == n_wports * 3:
                mem3 = Memory(width=self.data_width, depth=self.num_regs)
                mem3_rport = mem3.read_port(domain='comb')
                setattr(m.submodules, f'mem_read_port{rp_base + 2}',
                        mem3_rport)
                mem3_wport = mem3.write_port()
                setattr(m.submodules, f'mem3_write_port{i}', mem3_wport)

                m.d.comb += [
                    mem3_rport.addr.eq(self.read_ports[rp_base + 2].addr),
                    self.read_ports[rp_base + 2].data.eq(mem3_rport.data),
                    mem3_wport.addr.eq(wport.bits.addr),
                    mem3_wport.data.eq(wport.bits.data),
                    mem3_wport.en.eq(wport.valid),
                ]

        return m


class RegReadDecoder(Elaboratable):

    def __init__(self, params):
        self.params = params

        self.dis_uop = MicroOp(params)
        self.dis_valid = Signal()

        self.rrd_uop = MicroOp(params)
        self.rrd_valid = Signal()

    def elaborate(self, platform):
        m = Module()

        decoder = m.submodules.decoder = CommonDecoder(self.params)
        m.d.comb += [
            decoder.iss_uop.eq(self.dis_uop),
            decoder.iss_valid.eq(self.dis_valid),
            self.rrd_uop.eq(decoder.rrd_uop),
            self.rrd_valid.eq(decoder.rrd_valid),
        ]

        return m


class RegisterRead(HasCoreParams, Elaboratable):

    def __init__(self, num_rports, rports_array, reg_width, params):
        super().__init__(params)

        self.rports_array = rports_array
        self.data_width = reg_width

        self.dis_valid = Signal()
        self.dis_wid = Signal(range(self.n_warps))
        self.dis_uop = MicroOp(params)
        self.dis_ready = Signal()

        self.read_ports = [
            RFReadPort(Shape.cast(range(32 * self.n_warps)).width,
                       self.data_width,
                       name=f'read_port{i}') for i in range(num_rports)
        ]

        self.exec_req = Decoupled(ExecReq, reg_width, self.params)

    def elaborate(self, platform):
        m = Module()

        rrd_wid = Signal.like(self.dis_wid)
        rrd_uop = MicroOp(self.params)
        rrd_valid = Signal()

        dec = m.submodules.decoder = RegReadDecoder(self.params)
        m.d.comb += [
            rrd_wid.eq(self.dis_wid),
            dec.dis_uop.eq(self.dis_uop),
            dec.dis_valid.eq(self.dis_valid),
            rrd_uop.eq(dec.rrd_uop),
            rrd_valid.eq(dec.rrd_valid),
        ]

        rrd_rs1_data = [
            Signal(self.data_width, name=f'rrd{i}_rs1_data')
            for i in range(self.n_threads)
        ]
        rrd_rs2_data = [
            Signal(self.data_width, name=f'rrd{i}_rs2_data')
            for i in range(self.n_threads)
        ]
        rrd_rs3_data = [
            Signal(self.data_width, name=f'rrd{i}_rs3_data')
            for i in range(self.n_threads)
        ]

        idx = 0
        for nrps, rs1_data, rs2_data, rs3_data in zip(self.rports_array,
                                                      rrd_rs1_data,
                                                      rrd_rs2_data,
                                                      rrd_rs3_data):
            if nrps > 0:
                m.d.comb += [
                    self.read_ports[idx].addr.eq(Cat(rrd_uop.lrs1, rrd_wid))
                ]
            if nrps > 1:
                m.d.comb += [
                    self.read_ports[idx + 1].addr.eq(Cat(
                        rrd_uop.lrs2, rrd_wid))
                ]
            if nrps > 2:
                m.d.comb += [
                    self.read_ports[idx + 2].addr.eq(Cat(
                        rrd_uop.lrs3, rrd_wid))
                ]

            if nrps > 0:
                m.d.comb += rs1_data.eq(
                    Mux(rrd_uop.lrs1 == 0, 0, self.read_ports[idx].data))
            if nrps > 1:
                m.d.comb += rs2_data.eq(
                    Mux(rrd_uop.lrs2 == 0, 0, self.read_ports[idx + 1].data))
            if nrps > 2:
                m.d.comb += rs3_data.eq(
                    Mux(rrd_uop.prs3 == 0, 0, self.read_ports[idx + 2].data))

            idx += nrps

        rrd_req = ExecReq(self.data_width, self.params)
        m.d.comb += [
            rrd_req.uop.eq(rrd_uop),
            rrd_req.wid.eq(rrd_wid),
        ]
        for l, r in zip(rrd_req.rs1_data, rrd_rs1_data):
            m.d.comb += l.eq(r)
        for l, r in zip(rrd_req.rs2_data, rrd_rs2_data):
            m.d.comb += l.eq(r)
        for l, r in zip(rrd_req.rs3_data, rrd_rs3_data):
            m.d.comb += l.eq(r)

        out_buf = m.submodules.out_buf = SkidBuffer(ExecReq, self.data_width,
                                                    self.params)
        m.d.comb += [
            out_buf.enq.bits.eq(rrd_req),
            out_buf.enq.valid.eq(rrd_valid),
            self.dis_ready.eq(out_buf.enq.ready),
            out_buf.deq.connect(self.exec_req),
        ]

        return m
