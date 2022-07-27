from amaranth import *
from amaranth.lib.fifo import FIFOInterface

from room.interface import OBI


class FetchIO:

    def __init__(self):
        self.instr_req = Signal()
        self.instr_data = Signal(32)
        self.instr_valid = Signal()

        self.clear_instr_valid = Signal()

        self.pc_set = Signal()

        self.halt_if = Signal()
        self.id_ready = Signal()


def _incr(signal, modulo):
    if modulo == 2**len(signal):
        return signal + 1
    else:
        return Mux(signal == modulo - 1, 0, signal + 1)


class SyncFIFO(Elaboratable, FIFOInterface):

    def __init__(self, *, width, depth, fwft=True):
        super().__init__(width=width, depth=depth, fwft=fwft)

        self.flush = Signal()
        self.level = Signal(range(depth + 1))

    def elaborate(self, platform):
        m = Module()
        if self.depth == 0:
            m.d.comb += [
                self.w_rdy.eq(0),
                self.r_rdy.eq(0),
            ]
            return m

        m.d.comb += [
            self.w_rdy.eq(self.level != self.depth),
            self.r_rdy.eq(self.level != 0),
            self.w_level.eq(self.level),
            self.r_level.eq(self.level),
        ]

        do_read = self.r_rdy & self.r_en
        do_write = self.w_rdy & self.w_en

        storage = Memory(width=self.width, depth=self.depth)
        w_port = m.submodules.w_port = storage.write_port()
        r_port = m.submodules.r_port = storage.read_port(
            domain="comb" if self.fwft else "sync", transparent=self.fwft)
        produce = Signal(range(self.depth))
        consume = Signal(range(self.depth))

        m.d.comb += [
            w_port.addr.eq(produce),
            w_port.data.eq(self.w_data),
            w_port.en.eq(self.w_en & self.w_rdy),
        ]
        with m.If(self.flush):
            m.d.sync += produce.eq(0)
        with m.Elif(do_write):
            m.d.sync += produce.eq(_incr(produce, self.depth))

        m.d.comb += [
            r_port.addr.eq(consume),
            self.r_data.eq(r_port.data),
        ]
        if not self.fwft:
            m.d.comb += r_port.en.eq(self.r_en)
        with m.If(self.flush):
            m.d.sync += consume.eq(0)
        with m.Elif(do_read):
            m.d.sync += consume.eq(_incr(consume, self.depth))

        with m.If(self.flush):
            m.d.sync += self.level.eq(0)
        with m.Elif(do_write & ~do_read):
            m.d.sync += self.level.eq(self.level + 1)
        with m.Elif(do_read & ~do_write):
            m.d.sync += self.level.eq(self.level - 1)

        return m


class FetchController(Elaboratable):

    def __init__(self, ibus, depth=4):
        self.ibus = ibus
        self.depth = depth

        self.req = Signal()

        self.branch = Signal()
        self.branch_addr = Signal.like(ibus.addr)

        self.fetch_valid = Signal()
        self.fetch_ready = Signal()

        self.fifo_flush = Signal()
        self.fifo_level = Signal(range(depth + 1))
        self.fifo_valid = Signal()
        self.fifo_we = Signal()
        self.fifo_re = Signal()

    def elaborate(self, platform):
        m = Module()

        pc = Signal.like(self.ibus.addr)

        branch_addr_aligned = Cat(self.branch_addr[2:], Const(0, 2))

        with m.FSM():
            with m.State('IDLE'):
                with m.If(self.branch == 1):
                    m.d.comb += self.ibus.addr.eq(branch_addr_aligned)
                with m.Else():
                    m.d.comb += self.ibus.addr.eq(pc + 4)
                with m.If((self.branch == 1)
                          & ~(self.ibus.req & self.ibus.gnt)):
                    m.next = 'BRANCH_WAIT'
            with m.State('BRANCH_WAIT'):
                with m.If(self.branch == 1):
                    m.d.comb += self.ibus.addr.eq(branch_addr_aligned)
                with m.Else():
                    m.d.comb += self.ibus.addr.eq(pc)
                with m.If(self.ibus.req & self.ibus.gnt):
                    m.next = 'IDLE'

        with m.If((self.branch == 1) | (self.ibus.req & self.ibus.gnt)):
            m.d.sync += pc.eq(self.ibus.addr)

        count_up = Signal()
        count_down = Signal()
        count = Signal(range(self.depth + 1))
        flush_count = Signal.like(count)

        m.d.comb += [
            count_up.eq(self.ibus.req & self.ibus.gnt),
            count_down.eq(self.ibus.rvalid),
        ]

        with m.If((~count_up) & count_down):
            m.d.sync += count.eq(count - 1)
        with m.Elif(count_up & (~count_down)):
            m.d.sync += count.eq(count + 1)

        with m.If(self.branch == 1):
            m.d.sync += flush_count.eq(count)
        with m.Elif((self.ibus.rvalid == 1) & (count > 0)):
            m.d.sync += flush_count.eq(count - 1)

        m.d.comb += self.ibus.req.eq(self.req
                                     & (self.fifo_level + count < self.depth))

        m.d.comb += [
            self.fetch_valid.eq((self.fifo_valid | self.ibus.rvalid)
                                & ~(self.branch | (flush_count > 0))),
            self.fifo_we.eq(self.ibus.rvalid
                            & (self.fifo_valid | ~self.fetch_ready)
                            & ~(self.branch | (flush_count > 0))),
            self.fifo_re.eq(self.fifo_valid & self.fetch_ready),
            self.fifo_flush.eq(self.branch),
        ]

        return m


class FetchBuffer(Elaboratable):

    def __init__(self, ibus):
        self.ibus = ibus
        self.depth = 2

        self.req = Signal()

        self.branch = Signal()
        self.branch_addr = Signal.like(ibus.addr)

        self.fetch_ctrl = FetchController(self.ibus, depth=self.depth)
        self.fetch_fifo = SyncFIFO(width=32, depth=self.depth)

        self.fetch_valid = Signal()
        self.fetch_ready = Signal()
        self.fetch_data = Signal(32)

    def elaborate(self, platform):
        m = Module()

        ctrl = m.submodules.fetch_ctrl = self.fetch_ctrl
        fifo = m.submodules.fetch_fifo = self.fetch_fifo

        m.d.comb += [
            ctrl.fifo_level.eq(fifo.level),
            ctrl.fifo_valid.eq(fifo.r_rdy),
            fifo.w_data.eq(self.ibus.rdata),
            fifo.flush.eq(self.fetch_ctrl.fifo_flush),
            fifo.w_en.eq(self.fetch_ctrl.fifo_we),
            fifo.r_en.eq(self.fetch_ctrl.fifo_re),
        ]

        m.d.comb += [
            ctrl.req.eq(self.req),
            ctrl.branch.eq(self.branch),
            ctrl.branch_addr.eq(self.branch_addr),
            ctrl.fetch_ready.eq(self.fetch_ready),
            self.fetch_valid.eq(ctrl.fetch_valid),
        ]

        with m.If(fifo.r_rdy == 1):
            m.d.comb += self.fetch_data.eq(fifo.r_data)
        with m.Else():
            m.d.comb += self.fetch_data.eq(self.ibus.rdata)

        m.d.comb += [ctrl.req.eq(~ResetSignal('sync'))]

        return m


class IFStage(Elaboratable):

    def __init__(self, ibus):
        self.ibus = ibus
        self.io = FetchIO()

    def elaborate(self, platform):
        m = Module()

        buffer = m.submodules.fetch_buffer = FetchBuffer(self.ibus)

        if_ready = Signal()
        if_valid = Signal()

        m.d.comb += [
            if_ready.eq(buffer.fetch_valid & self.io.id_ready),
            if_valid.eq(if_ready & ~self.io.halt_if)
        ]

        with m.If(buffer.fetch_valid == 1):
            m.d.comb += buffer.fetch_ready.eq(self.io.instr_req & if_valid)

        m.d.comb += [
            buffer.branch.eq(self.io.pc_set),
        ]

        with m.If(if_valid):
            m.d.sync += [
                self.io.instr_data.eq(buffer.fetch_data),
                self.io.instr_valid.eq(1),
            ]
        with m.Elif(self.io.clear_instr_valid):
            m.d.sync += self.io.instr_valid.eq(0)

        return m
