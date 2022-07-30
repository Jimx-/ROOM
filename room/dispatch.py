from amaranth import *

from room.types import MicroOp


class Dispatcher(Elaboratable):

    def __init__(self, params):
        self.core_width = params['core_width']
        self.issue_params = params['issue_params']

        self.ren_valids = Signal(self.core_width)
        self.ren_uops = [
            MicroOp(params, name=f'ren_uop{i}') for i in range(self.core_width)
        ]

        self.dis_uops = [[
            MicroOp(params, name=f'dis_{str(typ).split(".")[-1]}_{i}')
            for i in range(q['dispatch_width'])
        ] for typ, q in self.issue_params.items()]

        self.dis_valids = [
            Signal(q['dispatch_width'],
                   name=f'iq_{str(typ).split(".")[-1]}_valids')
            for typ, q in self.issue_params.items()
        ]

        self.iq_ready = [
            Signal(q['dispatch_width'],
                   name=f'iq_{str(typ).split(".")[-1]}_ready')
            for typ, q in self.issue_params.items()
        ]

        self.ready = Signal(self.core_width)

    def elaborate(self, platform):
        m = Module()

        rr = ~0
        for qr in self.iq_ready:
            rr &= qr
        m.d.comb += self.ready.eq(rr)

        for (typ, q), iq, valids in zip(self.issue_params.items(),
                                        self.dis_uops, self.dis_valids):
            for uop, iq_uop, rv, v in zip(self.ren_uops, iq, self.ren_valids,
                                          valids):
                m.d.comb += [
                    v.eq(rv & ((uop.iq_type & typ) != 0)),
                    iq_uop.eq(uop),
                ]

        return m
