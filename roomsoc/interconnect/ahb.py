from enum import IntEnum
from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.utils import log2_int


class TransferType(IntEnum):
    IDLE = 0
    BUSY = 1
    NONSEQUENTIAL = 2
    SEQUENTIAL = 3


class Interface(Record):

    def __init__(self, *, addr_width, data_width, name=None):
        self.addr_width = addr_width
        self.data_width = data_width

        layout = [
            ('addr', addr_width, Direction.FANOUT),
            ('burst', 3, Direction.FANOUT),
            ('mastlock', 1, Direction.FANOUT),
            ('prot', 4, Direction.FANOUT),
            ('size', 3, Direction.FANOUT),
            ('trans', 2, Direction.FANOUT),
            ('wdata', data_width, Direction.FANOUT),
            ('write', 1, Direction.FANOUT),
            ('sel', 1, Direction.FANOUT),
            ('rdata', data_width, Direction.FANIN),
            ('readyout', 1, Direction.FANIN),
            ('resp', 1, Direction.FANIN),
        ]

        super().__init__(layout, name=name, src_loc_at=1)

    def write_single(self, addr, data):
        size = log2_int(self.data_width // 8)

        yield self.sel.eq(1)
        yield self.addr.eq(addr)
        yield self.trans.eq(TransferType.NONSEQUENTIAL)
        yield self.size.eq(size)
        yield self.write.eq(1)
        yield
        while not (yield self.readyout):
            yield
        yield self.trans.eq(TransferType.IDLE)
        yield self.size.eq(0)
        yield self.wdata.eq(data)
        yield
        while not (yield self.readyout):
            yield
        resp = (yield self.resp)
        yield self.sel.eq(0)
        return resp

    def read_single(self, addr):
        size = log2_int(self.data_width // 8)

        yield self.sel.eq(1)
        yield self.addr.eq(addr)
        yield self.trans.eq(TransferType.NONSEQUENTIAL)
        yield self.size.eq(size)
        yield self.write.eq(0)
        yield
        while not (yield self.readyout):
            yield
        yield self.trans.eq(TransferType.IDLE)
        yield self.size.eq(0)
        yield
        while not (yield self.readyout):
            yield
        data = (yield self.rdata)
        resp = (yield self.resp)
        yield self.sel.eq(0)
        return (data, resp)


class AHB2Wishbone(Elaboratable):

    def __init__(self, ahb, wishbone, base_addr=0x00000000):
        self.base_addr = base_addr

        self.ahb = ahb
        self.wishbone = wishbone

    def elaborate(self, platform):
        m = Module()

        ahb = self.ahb
        wb = self.wishbone

        wb_adr_shift = log2_int(ahb.data_width // 8)

        ahb_addr = Signal(ahb.addr_width)
        m.d.comb += ahb_addr.eq(ahb.addr - self.base_addr)

        if hasattr(wb, 'err'):
            m.d.comb += ahb.resp.eq(wb.err)

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += ahb.readyout.eq(1)

                with m.If(ahb.sel & (ahb.size == wb_adr_shift)
                          & (ahb.trans == TransferType.NONSEQUENTIAL)):
                    m.d.sync += [
                        wb.adr.eq(ahb_addr[wb_adr_shift:]),
                        wb.we.eq(ahb.write),
                        wb.sel.eq(2**len(wb.sel) - 1),
                    ]

                    m.next = 'ACT'

            with m.State('ACT'):
                m.d.comb += [
                    wb.stb.eq(1),
                    wb.cyc.eq(1),
                ]

                with m.If(wb.we):
                    m.d.comb += wb.dat_w.eq(ahb.wdata)

                with m.If(wb.ack):
                    with m.If(~wb.we):
                        m.d.sync += ahb.rdata.eq(wb.dat_r)

                    m.next = 'IDLE'

        return m
