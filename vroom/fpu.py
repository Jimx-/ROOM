from amaranth import *

from room.consts import RoundingMode
from room.fpu import FPFormat, IntFormat, FPUOperator, FPUInput, FPUResult, FPUFMA, FPUComp, FPUDivSqrtMulti, FPUCastMulti, FPURec
from room.utils import Pipe

from roomsoc.interconnect.stream import Valid, Decoupled


class VFPUFMA(Elaboratable):

    def __init__(self, width, latency=3):
        self.width = width
        self.latency = latency

        self.inp = Valid(FPUInput, self.width)
        self.out = Valid(FPUResult, self.width)

    def elaborate(self, platform):
        m = Module()

        in_pipe = m.submodules.in_pipe = Pipe(width=len(self.inp.bits),
                                              depth=self.latency)
        in_pipe_out = FPUInput(self.width)
        m.d.comb += [
            in_pipe.in_valid.eq(self.inp.valid),
            in_pipe.in_data.eq(self.inp.bits),
            in_pipe_out.eq(in_pipe.out.bits),
        ]

        dfma_res = []
        if self.width >= 64:
            for i in range(self.width // 64):
                dfma = FPUFMA(64, FPFormat.D, latency=self.latency)
                setattr(m.submodules, f'dfma{i}', dfma)

                m.d.comb += [
                    dfma.inp.valid.eq(self.inp.valid),
                    dfma.inp.bits.fn.eq(self.inp.bits.fn),
                    dfma.inp.bits.fn_mod.eq(self.inp.bits.fn_mod),
                    dfma.inp.bits.rm.eq(self.inp.bits.rm),
                    dfma.inp.bits.in1.eq(self.inp.bits.in1[i * 64:(i + 1) *
                                                           64]),
                    dfma.inp.bits.in2.eq(self.inp.bits.in2[i * 64:(i + 1) *
                                                           64]),
                    dfma.inp.bits.in3.eq(self.inp.bits.in3[i * 64:(i + 1) *
                                                           64]),
                    dfma.inp.bits.src_fmt.eq(self.inp.bits.src_fmt),
                    dfma.inp.bits.dst_fmt.eq(self.inp.bits.dst_fmt),
                    dfma.inp.bits.int_fmt.eq(self.inp.bits.int_fmt),
                ]

                dfma_res.append(dfma.out.bits.data)

        sfma_res = []
        for i in range(self.width // 32):
            sfma = FPUFMA(32, FPFormat.S, latency=self.latency)
            setattr(m.submodules, f'sfma{i}', sfma)

            m.d.comb += [
                sfma.inp.valid.eq(self.inp.valid),
                sfma.inp.bits.fn.eq(self.inp.bits.fn),
                sfma.inp.bits.fn_mod.eq(self.inp.bits.fn_mod),
                sfma.inp.bits.rm.eq(self.inp.bits.rm),
                sfma.inp.bits.in1.eq(self.inp.bits.in1[i * 32:(i + 1) * 32]),
                sfma.inp.bits.in2.eq(self.inp.bits.in2[i * 32:(i + 1) * 32]),
                sfma.inp.bits.in3.eq(self.inp.bits.in3[i * 32:(i + 1) * 32]),
                sfma.inp.bits.src_fmt.eq(self.inp.bits.src_fmt),
                sfma.inp.bits.dst_fmt.eq(self.inp.bits.dst_fmt),
                sfma.inp.bits.int_fmt.eq(self.inp.bits.int_fmt),
            ]

            sfma_res.append(sfma.out.bits.data)

        m.d.comb += self.out.valid.eq(in_pipe.out.valid)
        with m.Switch(in_pipe_out.dst_fmt):
            with m.Case(FPFormat.D):
                m.d.comb += self.out.bits.data.eq(Cat(dfma_res))
            with m.Case(FPFormat.S):
                m.d.comb += self.out.bits.data.eq(Cat(sfma_res))

        return m


class VFPUComp(Elaboratable):

    def __init__(self, width, latency=3):
        self.width = width
        self.latency = latency

        self.inp = Valid(FPUInput, self.width)
        self.out = Valid(FPUResult, self.width)

    def elaborate(self, platform):
        m = Module()

        in_pipe = m.submodules.in_pipe = Pipe(width=len(self.inp.bits),
                                              depth=self.latency)
        in_pipe_out = FPUInput(self.width)
        m.d.comb += [
            in_pipe.in_valid.eq(self.inp.valid),
            in_pipe.in_data.eq(self.inp.bits),
            in_pipe_out.eq(in_pipe.out.bits),
        ]

        dcmp_res = []
        if self.width >= 64:
            for i in range(self.width // 64):
                dcmp = FPUComp(64, FPFormat.D, latency=self.latency)
                setattr(m.submodules, f'dcmp{i}', dcmp)

                m.d.comb += [
                    dcmp.inp.valid.eq(self.inp.valid),
                    dcmp.inp.bits.fn.eq(self.inp.bits.fn),
                    dcmp.inp.bits.fn_mod.eq(self.inp.bits.fn_mod),
                    dcmp.inp.bits.rm.eq(self.inp.bits.rm),
                    dcmp.inp.bits.in1.eq(self.inp.bits.in1[i * 64:(i + 1) *
                                                           64]),
                    dcmp.inp.bits.in2.eq(self.inp.bits.in2[i * 64:(i + 1) *
                                                           64]),
                    dcmp.inp.bits.in3.eq(self.inp.bits.in3[i * 64:(i + 1) *
                                                           64]),
                    dcmp.inp.bits.src_fmt.eq(self.inp.bits.src_fmt),
                    dcmp.inp.bits.dst_fmt.eq(self.inp.bits.dst_fmt),
                    dcmp.inp.bits.int_fmt.eq(self.inp.bits.int_fmt),
                ]

                dcmp_res.append(dcmp.out.bits.data)

        scmp_res = []
        for i in range(self.width // 32):
            scmp = FPUComp(32, FPFormat.S, latency=self.latency)
            setattr(m.submodules, f'scmp{i}', scmp)

            m.d.comb += [
                scmp.inp.valid.eq(self.inp.valid),
                scmp.inp.bits.fn.eq(self.inp.bits.fn),
                scmp.inp.bits.fn_mod.eq(self.inp.bits.fn_mod),
                scmp.inp.bits.rm.eq(self.inp.bits.rm),
                scmp.inp.bits.in1.eq(self.inp.bits.in1[i * 32:(i + 1) * 32]),
                scmp.inp.bits.in2.eq(self.inp.bits.in2[i * 32:(i + 1) * 32]),
                scmp.inp.bits.in3.eq(self.inp.bits.in3[i * 32:(i + 1) * 32]),
                scmp.inp.bits.src_fmt.eq(self.inp.bits.src_fmt),
                scmp.inp.bits.dst_fmt.eq(self.inp.bits.dst_fmt),
                scmp.inp.bits.int_fmt.eq(self.inp.bits.int_fmt),
            ]

            scmp_res.append(scmp.out.bits.data)

        m.d.comb += self.out.valid.eq(in_pipe.out.valid)
        with m.Switch(in_pipe_out.dst_fmt):
            with m.Case(FPFormat.D):
                m.d.comb += self.out.bits.data.eq(Cat(dcmp_res))
            with m.Case(FPFormat.S):
                m.d.comb += self.out.bits.data.eq(Cat(scmp_res))

        with m.If(in_pipe_out.fn == FPUOperator.CMP):
            with m.Switch(in_pipe_out.src_fmt):
                with m.Case(FPFormat.D):
                    m.d.comb += self.out.bits.data.eq(
                        Cat(x[0] for x in dcmp_res))
                with m.Case(FPFormat.S):
                    m.d.comb += self.out.bits.data.eq(
                        Cat(x[0] for x in scmp_res))

        return m


class VFPUDivSqrt(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.a = Signal(width)
        self.b = Signal(width)
        self.is_sqrt = Signal()
        self.fmt = Signal(FPFormat)
        self.rm = Signal(RoundingMode)
        self.in_valid = Signal()
        self.in_ready = Signal()

        self.out = Decoupled(Signal, width)

    def elaborate(self, platform):
        m = Module()

        fdiv_busy = Signal()
        fmt = Signal(FPFormat)
        m.d.comb += self.in_ready.eq(~fdiv_busy)
        with m.If(self.in_valid & self.in_ready):
            m.d.sync += [
                fdiv_busy.eq(1),
                fmt.eq(self.fmt),
            ]
        with m.Elif(self.out.fire):
            m.d.sync += fdiv_busy.eq(0)

        resp_valid = Signal(self.width // 32)
        resp_data_s = Signal(self.width)
        resp_data_d = Signal(self.width)
        for i in range(self.width // 32):
            fdiv = FPUDivSqrtMulti()
            setattr(m.submodules, f'fdiv{i}', fdiv)

            m.d.comb += [
                fdiv.is_sqrt.eq(self.is_sqrt),
                fdiv.fmt.eq(self.fmt),
                fdiv.rm.eq(self.rm),
                resp_valid[i].eq(fdiv.out.valid),
                fdiv.out.ready.eq(~fdiv_busy | self.out.fire),
            ]
            if i & 1:
                m.d.comb += [
                    fdiv.a.eq(self.a[i * 32:(i + 1) * 32]),
                    fdiv.b.eq(self.b[i * 32:(i + 1) * 32]),
                    fdiv.in_valid.eq(self.in_valid & self.in_ready
                                     & (self.fmt == FPFormat.S)),
                    resp_data_s[i * 32:(i + 1) * 32].eq(fdiv.out.bits),
                ]
            else:
                m.d.comb += [
                    fdiv.a.eq(self.a[(i // 2) * 64:(i // 2 + 1) * 64]),
                    fdiv.b.eq(self.b[(i // 2) * 64:(i // 2 + 1) * 64]),
                    fdiv.in_valid.eq(self.in_valid & self.in_ready),
                    resp_data_s[i * 32:(i + 1) * 32].eq(fdiv.out.bits),
                    resp_data_d[(i // 2) * 64:(i // 2 + 1) * 64].eq(
                        fdiv.out.bits),
                ]

        resp_mask = Signal(self.width // 32)
        with m.Switch(fmt):
            with m.Case(FPFormat.D):
                m.d.comb += [
                    self.out.bits.eq(resp_data_d),
                    resp_mask.eq(Const(1, 2).replicate(self.width // 64)),
                ]
            with m.Case(FPFormat.S):
                m.d.comb += [
                    self.out.bits.eq(resp_data_s),
                    resp_mask.eq(~0),
                ]

        m.d.comb += self.out.valid.eq((resp_valid & resp_mask) == resp_mask)

        return m


class VFPUCast(Elaboratable):

    def __init__(self, width, latency=3):
        self.width = width
        self.latency = latency

        self.inp = Valid(FPUInput, self.width)
        self.out = Valid(FPUResult, self.width)

    def elaborate(self, platform):
        m = Module()

        in_pipe = m.submodules.in_pipe = Pipe(width=len(self.inp.bits),
                                              depth=self.latency)
        in_pipe_out = FPUInput(self.width)
        m.d.comb += [
            in_pipe.in_valid.eq(self.inp.valid),
            in_pipe.in_data.eq(self.inp.bits),
            in_pipe_out.eq(in_pipe.out.bits),
        ]

        resp_data_s = Signal(self.width)
        resp_data_d = Signal(self.width)
        for i in range(self.width // 32):
            cast = FPUCastMulti(latency=self.latency)
            setattr(m.submodules, f'cast{i}', cast)

            m.d.comb += [
                cast.inp.bits.fn.eq(self.inp.bits.fn),
                cast.inp.bits.fn_mod.eq(self.inp.bits.fn_mod),
                cast.inp.bits.rm.eq(self.inp.bits.rm),
                cast.inp.bits.src_fmt.eq(self.inp.bits.src_fmt),
                cast.inp.bits.dst_fmt.eq(self.inp.bits.dst_fmt),
                cast.inp.bits.int_fmt.eq(self.inp.bits.int_fmt),
            ]
            if i & 1:
                m.d.comb += [
                    cast.inp.bits.in1.eq(self.inp.bits.in2[i * 32:(i + 1) *
                                                           32]),
                    cast.inp.valid.eq(self.inp.valid
                                      & (self.inp.bits.src_fmt == FPFormat.S)),
                    resp_data_s[i * 32:(i + 1) * 32].eq(cast.out.bits.data),
                ]
            else:
                m.d.comb += [
                    cast.inp.bits.in1.eq(
                        self.inp.bits.in2[(i // 2) * 64:(i // 2 + 1) * 64]),
                    cast.inp.valid.eq(self.inp.valid),
                    resp_data_s[i * 32:(i + 1) * 32].eq(cast.out.bits.data),
                    resp_data_d[(i // 2) * 64:(i // 2 + 1) * 64].eq(
                        cast.out.bits.data),
                ]

        m.d.comb += self.out.valid.eq(in_pipe.out.valid)
        with m.If(in_pipe_out.fn == FPUOperator.F2I):
            with m.Switch(in_pipe_out.int_fmt):
                with m.Case(IntFormat.INT64):
                    m.d.comb += self.out.bits.data.eq(resp_data_d)
                with m.Case(IntFormat.INT32):
                    m.d.comb += self.out.bits.data.eq(resp_data_s)
        with m.Else():
            with m.Switch(in_pipe_out.dst_fmt):
                with m.Case(FPFormat.D):
                    m.d.comb += self.out.bits.data.eq(resp_data_d)
                with m.Case(FPFormat.S):
                    m.d.comb += self.out.bits.data.eq(resp_data_s)

        return m


class VFPURec(Elaboratable):

    def __init__(self, width, latency=3):
        self.width = width
        self.latency = latency

        self.inp = Valid(FPUInput, self.width)
        self.out = Valid(FPUResult, self.width)

    def elaborate(self, platform):
        m = Module()

        in_pipe = m.submodules.in_pipe = Pipe(width=len(self.inp.bits),
                                              depth=self.latency)
        in_pipe_out = FPUInput(self.width)
        m.d.comb += [
            in_pipe.in_valid.eq(self.inp.valid),
            in_pipe.in_data.eq(self.inp.bits),
            in_pipe_out.eq(in_pipe.out.bits),
        ]

        drec_res = []
        if self.width >= 64:
            for i in range(self.width // 64):
                drec = FPURec(64, FPFormat.D, latency=self.latency)
                setattr(m.submodules, f'drec{i}', drec)

                m.d.comb += [
                    drec.inp.valid.eq(self.inp.valid),
                    drec.inp.bits.fn.eq(self.inp.bits.fn),
                    drec.inp.bits.fn_mod.eq(self.inp.bits.fn_mod),
                    drec.inp.bits.rm.eq(self.inp.bits.rm),
                    drec.inp.bits.in1.eq(self.inp.bits.in2[i * 64:(i + 1) *
                                                           64]),
                ]

                drec_res.append(drec.out.bits.data)

        srec_res = []
        for i in range(self.width // 32):
            srec = FPURec(32, FPFormat.S, latency=self.latency)
            setattr(m.submodules, f'srec{i}', srec)

            m.d.comb += [
                srec.inp.valid.eq(self.inp.valid),
                srec.inp.bits.fn.eq(self.inp.bits.fn),
                srec.inp.bits.fn_mod.eq(self.inp.bits.fn_mod),
                srec.inp.bits.rm.eq(self.inp.bits.rm),
                srec.inp.bits.in1.eq(self.inp.bits.in2[i * 32:(i + 1) * 32]),
            ]

            srec_res.append(srec.out.bits.data)

        m.d.comb += self.out.valid.eq(in_pipe.out.valid)
        with m.Switch(in_pipe_out.dst_fmt):
            with m.Case(FPFormat.D):
                m.d.comb += self.out.bits.data.eq(Cat(drec_res))
            with m.Case(FPFormat.S):
                m.d.comb += self.out.bits.data.eq(Cat(srec_res))

        return m


class VFPUReduce(Elaboratable):

    def __init__(self, width, latency=3):
        self.width = width
        self.latency = latency

        self.inp = Valid(FPUInput, self.width)
        self.out = Valid(FPUResult, self.width)

    def elaborate(self, platform):
        m = Module()

        in_pipe = m.submodules.in_pipe = Pipe(width=len(self.inp.bits),
                                              depth=self.latency)
        in_pipe_out = FPUInput(self.width)
        m.d.comb += [
            in_pipe.in_valid.eq(self.inp.valid),
            in_pipe.in_data.eq(self.inp.bits),
            in_pipe_out.eq(in_pipe.out.bits),
        ]

        fadd = m.submodules.fadd = VFPUFMA(self.width, latency=self.latency)
        m.d.comb += [
            fadd.inp.eq(self.inp),
            fadd.inp.valid.eq(self.inp.valid
                              & (self.inp.bits.fn != FPUOperator.MINMAX)),
            fadd.inp.bits.in3.eq(self.inp.bits.in2),
        ]

        fcmp = m.submodules.fcmp = VFPUCast(self.width, latency=self.latency)
        m.d.comb += [
            fcmp.inp.eq(self.inp),
            fcmp.inp.valid.eq(self.inp.valid
                              & (self.inp.bits.fn == FPUOperator.MINMAX)),
        ]

        m.d.comb += self.out.valid.eq(in_pipe.out.valid)
        with m.If(in_pipe_out.fn == FPUOperator.MINMAX):
            m.d.comb += self.out.bits.eq(fcmp.out.bits)
        with m.Else():
            m.d.comb += self.out.bits.eq(fadd.out.bits)

        return m
