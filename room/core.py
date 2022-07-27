from amaranth import *

from room.if_stage import IFStage
from room.id_stage import IDStage
from room.ex_stage import EXStage


class Core(Elaboratable):

    def __init__(self, ibus):
        self.ibus = ibus

    def elaborate(self, platform):
        m = Module()

        if_stage = m.submodules.if_stage = IFStage(self.ibus)
        id_stage = m.submodules.id_stage = IDStage(if_stage.io)
        ex_stage = m.submodules.ex_stage = EXStage(id_stage.io)

        return m
