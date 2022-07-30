from amaranth import *

from room.consts import *


class MicroOp(Record):

    def __init__(self, params, name=None):
        W = lambda x: Shape.cast(x).width
        core_width = params['core_width']
        num_pregs = params['num_pregs']

        super().__init__(
            [('valid', 1), ('opcode', W(UOpCode)), ('inst', 32), ('is_rvc', 1),
             ('iq_type', W(IssueQueueType)), ('opa_sel', W(OpA)),
             ('opb_sel', W(OpB)), ('imm_sel', W(ImmSel)),
             ('alu_fn', W(ALUOperator)), ('issue_uops', 2), ('imm_packed', 20),
             ('rob_idx', range(params['num_rob_rows'] * core_width)),
             ('pdst', range(num_pregs)), ('prs1', range(num_pregs)),
             ('prs2', range(num_pregs)), ('prs3', range(num_pregs)),
             ('prs1_busy', 1), ('prs2_busy', 1), ('prs3_busy', 1),
             ('stale_pdst', range(num_pregs)), ('ldst', 5), ('lrs1', 5),
             ('lrs2', 5), ('lrs3', 5), ('ldst_valid', 1),
             ('dst_rtype', W(RegisterType)), ('lrs1_rtype', W(RegisterType)),
             ('lrs2_rtype', W(RegisterType))],
            name=name)

    def rf_wen(self):
        return self.dst_rtype != RegisterType.X
