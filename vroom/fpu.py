from amaranth import *

from room.fpu import FPFormat, FPUInput, FPUResult, FPUFMA, FPUComp
from room.utils import Pipe

from roomsoc.interconnect.stream import Valid


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

        return m
