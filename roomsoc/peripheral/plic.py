from amaranth import *
from enum import IntEnum

from .peripheral import Peripheral


class _RegisterMap(IntEnum):
    PRIORITY = 0
    PENDING = 0x1000
    ENABLE = 0x2000
    CONTEXT = 0x200000

    @staticmethod
    def enable_base(i):
        return _RegisterMap.ENABLE + i * 0x80

    @staticmethod
    def context_base(i):
        return _RegisterMap.CONTEXT + i * 0x1000


class PLICGateway(Elaboratable):

    def __init__(self):
        self.interrupt = Signal()

        self.valid = Signal()
        self.ready = Signal()
        self.complete = Signal()

    def elaborate(self, platform):
        m = Module()

        busy = Signal()

        with m.If(self.interrupt & self.ready):
            m.d.sync += busy.eq(1)
        with m.If(self.complete):
            m.d.sync += busy.eq(0)

        m.d.comb += self.valid.eq(self.interrupt & ~busy)

        return m


class PLICSelector(Elaboratable):

    def __init__(self, *, n_devices, n_priorities):
        self.n_devices = n_devices

        self.prio = [
            Signal(range(n_priorities), name=f'prio{i}')
            for i in range(n_devices)
        ]
        self.ip = Signal(n_devices)
        self.max_dev = Signal(range(n_devices + 1))
        self.max_prio = Signal(range(n_priorities))

    def elaborate(self, platform):
        m = Module()

        def find_max(x):
            pri = Signal(len(self.prio[0]) + 1)
            dev = Signal(range(self.n_devices + 1))

            if len(x) > 1:
                mid = len(x) // 2
                left = find_max(x[:mid])
                right = find_max(x[mid:])

                with m.If(left[0] > right[0]):
                    m.d.comb += [
                        pri.eq(left[0]),
                        dev.eq(left[1]),
                    ]
                with m.Else():
                    m.d.comb += [
                        pri.eq(right[0]),
                        dev.eq(mid + right[1]),
                    ]
            else:
                m.d.comb += [
                    pri.eq(x[0]),
                    dev.eq(0),
                ]

            return pri, dev

        eff_prio = [1 << len(self.prio[0])] + [
            Cat(prio, self.ip[i]) for i, prio in enumerate(self.prio)
        ]

        max_pri, max_dev = find_max(eff_prio)
        m.d.comb += [
            self.max_dev.eq(max_dev),
            self.max_prio.eq(max_pri),
        ]

        return m


class PLIC(Peripheral, Elaboratable):

    def __init__(self, *, name=None, n_harts=1, n_devices=1, max_priority=0):
        super().__init__(name=name)
        self.n_harts = n_harts
        self.n_devices = n_devices

        if max_priority != 0:
            raise ValueError("Interrupt priorities not supported by PLIC")

        self.max_priority = max_priority

        bank = self.csr_bank()
        self._priority = [
            bank.csr(32,
                     'rw',
                     addr=_RegisterMap.PRIORITY + i * 4,
                     name=f'priority{i}')
            for i in range(1, self.n_devices + 1)
        ]

        self._pending = bank.csr(self.n_devices + 1,
                                 'r',
                                 addr=_RegisterMap.PENDING)

        self._enable = [
            bank.csr(n_devices + 1,
                     'rw',
                     addr=_RegisterMap.enable_base(i),
                     name=f'enable{i}') for i in range(self.n_harts)
        ]

        self._threshold = [
            bank.csr(32,
                     'rw',
                     addr=_RegisterMap.context_base(i),
                     name=f'threshold{i}') for i in range(self.n_harts)
        ]

        self._claim = [
            bank.csr(32,
                     'rw',
                     addr=_RegisterMap.context_base(i) + 4,
                     name=f'claim{i}') for i in range(self.n_harts)
        ]

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

        self.interrupts = Signal(self.n_devices)
        self.hart_ints = Signal(self.n_harts)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        gateways = []
        for i in range(self.n_devices):
            gateway = PLICGateway()
            setattr(m.submodules, f'gateway{i}', gateway)
            gateways.append(gateway)

            m.d.comb += gateway.interrupt.eq(self.interrupts[i])

        min_priorities = min(self.max_priority, self.n_devices)
        n_priorities = (1 << Shape.cast(range(min_priorities)).width) - 1

        if n_priorities == 0:
            for i in range(self.n_devices):
                m.d.comb += self._priority[i].r_data.eq(1)
            for i in range(self.n_harts):
                m.d.comb += self._threshold[i].r_data.eq(0)

        enables = [
            Signal(self.n_devices, name=f'enable{i}')
            for i in range(self.n_harts)
        ]
        for e, er in zip(enables, self._enable):
            m.d.comb += er.r_data.eq(Cat(Const(0, 1), e))
            with m.If(er.w_stb):
                m.d.sync += e.eq(er.w_data[1:])

        pending = Signal(self.n_devices)
        for i in range(self.n_devices):
            m.d.comb += self._pending.r_data[i + 1].eq(pending[i])

        max_devs = [
            Signal(range(self.n_devices + 1), name=f'max_dev{i}')
            for i in range(self.n_harts)
        ]
        if self.n_devices > 0:
            for i in range(self.n_harts):
                selector = PLICSelector(n_devices=self.n_devices,
                                        n_priorities=max(n_priorities, 1))
                setattr(m.submodules, f'selector{i}', selector)

                for d in range(self.n_devices):
                    m.d.comb += selector.prio[d].eq(self._priority[d].r_data)
                m.d.comb += [
                    selector.ip.eq(pending & enables[i]),
                    max_devs[i].eq(selector.max_dev),
                ]

                max_prio = Signal.like(selector.max_prio)
                m.d.sync += max_prio.eq(selector.max_prio)
                m.d.comb += self.hart_ints[i].eq(
                    max_prio > self._threshold[i].r_data)

        claim_req = Signal(self.n_harts)
        claim_gnt = Signal(range(self.n_devices + 1))
        claimed_dev = Signal(self.n_devices)
        for i in range(self.n_harts):
            with m.If(claim_req[i]):
                m.d.comb += claim_gnt.eq(max_devs[i])

        with m.Switch(claim_gnt):
            for i in range(self.n_devices):
                with m.Case(i + 1):
                    m.d.comb += claimed_dev[i].eq(1)

        for i in range(self.n_devices):
            m.d.comb += gateways[i].ready.eq(~pending[i])
            with m.If(claimed_dev[i] | gateways[i].valid):
                m.d.sync += pending[i].eq(~claimed_dev[i])

        for i in range(self.n_harts):
            m.d.comb += [
                self._claim[i].r_data.eq(max_devs[i]),
                claim_req[i].eq(self._claim[i].r_stb),
            ]

        complete_req = Signal(self.n_harts)
        complete_dev = Signal(range(self.n_devices + 1))

        for i in range(self.n_harts):
            with m.If(self._claim[i].w_stb):
                m.d.comb += complete_dev.eq(self._claim[i].w_data)

                with m.Switch(complete_dev):
                    for d in range(self.n_devices):
                        with m.Case(d + 1):
                            m.d.comb += complete_req[i].eq(enables[i][d])

        with m.If(complete_req.any()):
            with m.Switch(complete_dev):
                for i, gateway in enumerate(gateways):
                    with m.Case(i + 1):
                        m.d.comb += gateway.complete.eq(1)

        return m
