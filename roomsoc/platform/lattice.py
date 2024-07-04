from amaranth import *
import amaranth.vendor as vendor


class LatticeECP5Platform(vendor.LatticeECP5Platform):

    device = "LFE5U-85F"
    package = "BG256"
    speed = "2"
    resources = []
    connectors = []

    class DDROutput(Elaboratable):

        def __init__(self):
            self.i1 = Signal()
            self.i2 = Signal()
            self.o = Signal()

        def elaborate(self, platform):
            m = Module()

            m.submodules.oddr = Instance(
                'ODDRX1F',
                ###
                o_Q=self.o,
                ###
                i_SCLK=ClockSignal(),
                i_D0=self.i1,
                i_D1=self.i2,
            )

            return m

    def get_ddr_output(self):
        return LatticeECP5Platform.DDROutput()
