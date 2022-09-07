from amaranth import *

from room.consts import *


class MicroOp(Record):

    def __init__(self, params, name=None):
        W = lambda x: Shape.cast(x).width
        core_width = params['core_width']
        num_pregs = params['num_pregs']
        max_br_count = params['max_br_count']
        fetch_bytes = params['fetch_bytes']
        ftq_size = params['fetch_buffer_size']

        super().__init__(
            [('valid', 1), ('opcode', W(UOpCode)), ('inst', 32), ('is_rvc', 1),
             ('iq_type', W(IssueQueueType)), ('fu_type', W(FUType)),
             ('br_type', W(BranchType)), ('opa_sel', W(OpA)),
             ('opb_sel', W(OpB)), ('imm_sel', W(ImmSel)),
             ('alu_fn', W(ALUOperator)), ('csr_addr', 12),
             ('csr_cmd', W(CSRCommand)), ('is_load', 1), ('is_sta', 1),
             ('is_std', 1), ('issue_uops', 2), ('is_br', 1), ('is_jal', 1),
             ('is_jalr', 1), ('br_tag', range(max_br_count)),
             ('br_mask', max_br_count), ('ftq_idx', range(ftq_size)),
             ('edge_inst', 1), ('pc_lsb', range(fetch_bytes)), ('taken', 1),
             ('imm_packed', 20),
             ('rob_idx', range(params['num_rob_rows'] * core_width)),
             ('ldq_idx', range(params['ldq_size'])),
             ('stq_idx', range(params['stq_size'])),
             ('pdst', range(num_pregs)), ('prs1', range(num_pregs)),
             ('prs2', range(num_pregs)), ('prs3', range(num_pregs)),
             ('prs1_busy', 1), ('prs2_busy', 1), ('prs3_busy', 1),
             ('stale_pdst', range(num_pregs)), ('mem_cmd', W(MemoryCommand)),
             ('mem_size', 2), ('mem_signed', 1), ('uses_ldq', 1),
             ('uses_stq', 1), ('is_ecall', 1), ('clear_pipeline', 1),
             ('flush_on_commit', 1), ('exception', 1), ('exc_cause', 32),
             ('ldst', 5), ('lrs1', 5), ('lrs2', 5), ('lrs3', 5),
             ('ldst_valid', 1), ('dst_rtype', W(RegisterType)),
             ('lrs1_rtype', W(RegisterType)), ('lrs2_rtype', W(RegisterType))],
            name=name)

    def allocate_brtag(self):
        return self.is_jalr | self.is_br

    def rf_wen(self):
        return self.dst_rtype != RegisterType.X
