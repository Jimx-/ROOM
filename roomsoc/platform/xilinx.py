from amaranth import *
from amaranth.vendor.xilinx import XilinxPlatform as VendorXilinxPlatform

from room.consts import RoundingMode
from room.fpu import FPFormat, FType, FPUOperator, FPUInput, FPUResult

from roomsoc.interconnect.stream import Valid


class XilinxPlatform(VendorXilinxPlatform):

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
        return XilinxPlatform.AsyncFIFO(width=width,
                                        depth=depth,
                                        r_domain=r_domain,
                                        w_domain=w_domain)

    def get_ff_sync(self, ff):
        m = Module()

        if len(ff.i) == 1:
            m.submodules.ff_sync = Instance(
                'xpm_cdc_single',
                ###
                p_SRC_INPUT_REG=0,
                p_DEST_SYNC_FF=ff._stages,
                p_INIT_SYNC_FF=0,
                p_SIM_ASSERT_CHK=0,
                ###
                i_dest_clk=ClockSignal(ff._o_domain),
                i_src_in=ff.i,
                i_src_clk=0,
                ###
                o_dest_out=ff.o,
            )
        else:
            m.submodules.ff_sync = Instance(
                'xpm_cdc_array_single',
                ###
                p_WIDTH=len(ff.i),
                p_SRC_INPUT_REG=0,
                p_DEST_SYNC_FF=ff._stages,
                p_INIT_SYNC_FF=0,
                p_SIM_ASSERT_CHK=0,
                ###
                i_dest_clk=ClockSignal(ff._o_domain),
                i_src_in=ff.i,
                i_src_clk=0,
                ###
                o_dest_out=ff.o,
            )

        return m

    class XilinxFMA(Elaboratable):

        def __init__(self, *, width, format, latency):
            self.width = width

            _fmts = {
                FPFormat.H: FType.FP16,
                FPFormat.S: FType.FP32,
                FPFormat.D: FType.FP64,
            }

            self.ftyp = _fmts.get(format)
            if self.ftyp is None:
                raise ValueError(f"Unsupported FP format: {format}")

            self.latency = latency

            self.inp = Valid(FPUInput, self.width)
            self.out = Valid(FPUResult, self.width)

        def elaborate(self, platform):
            m = Module()

            in1 = Record(self.ftyp.record_layout())
            in2 = Record(self.ftyp.record_layout())
            in3 = Record(self.ftyp.record_layout())
            m.d.comb += [
                in1.eq(self.inp.bits.in1),
                in2.eq(self.inp.bits.in2),
                in3.eq(self.inp.bits.in3),
                in3.sign.eq(self.inp.bits.in3[-1] ^ self.inp.bits.fn_mod),
            ]

            with m.Switch(self.inp.bits.fn):
                with m.Case(FPUOperator.FNMSUB):
                    m.d.comb += in1.sign.eq(~self.inp.bits.in1[-1])

                with m.Case(FPUOperator.ADD):
                    m.d.comb += [
                        in2.sign.eq(0),
                        in2.exp.eq(self.ftyp.bias()),
                        in2.man.eq(0),
                    ]

                with m.Case(FPUOperator.MUL):
                    m.d.comb += [
                        in3.sign.eq(self.inp.bits.rm != RoundingMode.RDN),
                        in3.exp.eq(0),
                        in3.man.eq(0),
                    ]

            tuser = Signal(3)  # NV, OF, UF

            m.submodules.fma = Instance(
                f'xil_fma{self.width}',
                ###
                o_m_axis_result_tvalid=self.out.valid,
                o_m_axis_result_tdata=self.out.bits.data,
                o_m_axis_result_tuser=tuser,
                ###
                i_aclk=ClockSignal(),
                i_aresetn=~ResetSignal(),
                i_s_axis_a_tvalid=self.inp.valid,
                i_s_axis_a_tdata=in1,
                i_s_axis_b_tvalid=self.inp.valid,
                i_s_axis_b_tdata=in2,
                i_s_axis_c_tvalid=self.inp.valid,
                i_s_axis_c_tdata=in3,
            )

            m.d.comb += self.out.bits.status.eq(
                Cat(Const(0, 1), tuser[:2], Const(0, 1), tuser[-1]))

            return m

    def get_fma(self, *, width, format, latency=3):
        return XilinxPlatform.XilinxFMA(width=width,
                                        format=format,
                                        latency=latency)
