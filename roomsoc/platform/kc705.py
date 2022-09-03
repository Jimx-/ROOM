from amaranth import *
from amaranth.vendor.xilinx import XilinxPlatform


class KC705Platform(XilinxPlatform):
    device = "xc7k325t"
    package = "ffg676"
    speed = "2"
    resources = []
    connectors = []

    class AsyncFIFO(Elaboratable):

        def __init__(self, *, width, depth, r_domain="read", w_domain="write"):
            self.width = width
            self.depth = max(depth, 16)
            self.r_domain = r_domain
            self.w_domain = w_domain

            self.w_data = Signal(width, reset_less=True)
            self.w_rdy = Signal()  # writable; not full
            self.w_en = Signal()

            self.r_data = Signal(width, reset_less=True)
            self.r_rdy = Signal()  # readable; not empty
            self.r_en = Signal()

        def elaborate(self, platform):
            m = Module()

            empty = Signal()
            full = Signal()

            rd_rst_busy = Signal()
            wr_rst_busy = Signal()

            m.submodules.fifo_async = Instance(
                'xpm_fifo_async',
                ####
                p_DOUT_RESET_VALUE='0',
                p_ECC_MODE='no_ecc',
                p_FIFO_MEMORY_TYPE='auto',
                p_FIFO_READ_LATENCY=1,
                p_FIFO_WRITE_DEPTH=self.depth,
                p_FULL_RESET_VALUE=0,
                p_PROG_EMPTY_THRESH=2,
                p_PROG_FULL_THRESH=self.depth - 2,
                p_RD_DATA_COUNT_WIDTH=Shape.cast(range(self.width)).width,
                p_READ_DATA_WIDTH=self.width,
                p_READ_MODE='fwft',
                p_SIM_ASSERT_CHK=0,
                p_USE_ADV_FEATURES='0000',
                p_WAKEUP_TIME=0,
                p_WRITE_DATA_WIDTH=self.width,
                p_WR_DATA_COUNT_WIDTH=Shape.cast(range(self.width)).width,
                ###
                o_dout=self.r_data,
                o_empty=empty,
                o_full=full,
                o_rd_rst_busy=rd_rst_busy,
                o_wr_rst_busy=wr_rst_busy,
                ###
                i_din=self.w_data,
                i_rd_clk=ClockSignal(self.r_domain),
                i_rd_en=self.r_en,
                i_rst=ResetSignal(self.w_domain),
                i_sleep=0,
                i_wr_clk=ClockSignal(self.w_domain),
                i_wr_en=self.w_en,
            )

            m.d.comb += [
                self.r_rdy.eq(~empty & ~rd_rst_busy),
                self.w_rdy.eq(~full & ~wr_rst_busy),
            ]

            return m

    def get_async_fifo(self,
                       *,
                       width,
                       depth,
                       r_domain="read",
                       w_domain="write"):
        return KC705Platform.AsyncFIFO(width=width,
                                       depth=depth,
                                       r_domain=r_domain,
                                       w_domain=w_domain)
