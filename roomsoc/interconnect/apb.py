from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.utils import log2_int


class Interface(Record):

    def __init__(self, *, addr_width, data_width, name=None):
        self.addr_width = addr_width
        self.data_width = data_width

        layout = [
            ('addr', addr_width, Direction.FANOUT),
            ('sel', 1, Direction.FANOUT),
            ('enable', 1, Direction.FANOUT),
            ('write', 1, Direction.FANOUT),
            ('wdata', data_width, Direction.FANOUT),
            ('ready', 1, Direction.FANIN),
            ('rdata', data_width, Direction.FANIN),
            ('slverr', 1, Direction.FANIN),
        ]

        super().__init__(layout, name=name, src_loc_at=1)

    def _do_transaction(self):
        yield self.sel.eq(1)
        yield
        yield self.enable.eq(1)
        while not (yield self.ready):
            yield
        yield self.sel.eq(0)
        yield self.enable.eq(0)

    def write_data(self, addr, data):
        yield self.addr.eq(addr)
        yield self.wdata.eq(data)
        yield self.write.eq(1)
        yield from self._do_transaction()

    def read_data(self, addr):
        yield self.addr.eq(addr)
        yield self.write.eq(0)
        yield from self._do_transaction()
        return (yield self.rdata)


class APB2Wishbone(Elaboratable):

    def __init__(self, apb, wishbone, base_addr=0x00000000):
        self.base_addr = base_addr

        self.apb = apb
        self.wishbone = wishbone

    def elaborate(self, platform):
        m = Module()

        apb = self.apb
        wb = self.wishbone

        wb_adr_shift = log2_int(apb.data_width // 8)

        apb_addr = Signal(apb.addr_width)
        m.d.comb += apb_addr.eq(apb.addr - self.base_addr)

        m.d.comb += [
            wb.adr.eq(apb_addr[wb_adr_shift:]),
            wb.dat_w.eq(apb.wdata),
            apb.rdata.eq(wb.dat_r),
            wb.stb.eq(apb.sel),
            wb.cyc.eq(apb.sel),
            wb.we.eq(apb.write),
            apb.ready.eq(wb.ack),
            wb.sel.eq(~0),
        ]

        return m
