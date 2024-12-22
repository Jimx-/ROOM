from amaranth import *
from amaranth.utils import log2_int

from room.consts import *


class HasCoreParams:

    def __init__(self, params, *args, **kwargs):
        self.params = params

        self.is_groom = params.get('is_groom', False)

        self.core_id = params.get('core_id', 0)

        self.xlen = params['xlen']
        self.flen = params['flen']
        self.use_fpu = params['use_fpu']

        self.vaddr_bits = params['vaddr_bits']
        self.vaddr_bits_extended = self.vaddr_bits + (self.vaddr_bits
                                                      < self.xlen)

        self.io_regions = params['io_regions']

        #
        # Floating point
        #

        self.fma_latency = params['fma_latency']

        #
        # LR/SC
        #

        self.lrsc_cycles = params.get('lrsc_cycles', 80)

        if not self.is_groom:
            self.core_width = params['core_width']

            self.use_vm = params['use_vm']
            self.use_user = params['use_user']
            self.use_supervisor = params['use_supervisor']

            self.paddr_bits = params['paddr_bits']

            #
            # Instruction fetch
            #
            self.fetch_width = params['fetch_width']
            self.fetch_bytes = self.fetch_width << 1
            self.fetch_buffer_size = self.ftq_size = params[
                'fetch_buffer_size']
            self.max_br_count = params['max_br_count']

            #
            # Physical register file
            #

            self.num_int_pregs = params['num_int_pregs']
            self.num_fp_pregs = params['num_fp_pregs']
            self.num_pregs = max(self.num_int_pregs, self.num_fp_pregs)

            #
            # Breakpoint
            #

            self.num_breakpoints = params['num_breakpoints']

            #
            # Issue
            #

            self.issue_params = params['issue_params']
            self.int_width = self.issue_params[
                IssueQueueType.INT]['issue_width']
            self.mem_width = self.issue_params[
                IssueQueueType.MEM]['issue_width']

            #
            # ROB
            #

            self.num_rob_rows = params['num_rob_rows']

            #
            # LSU
            #

            self.ldq_size = params['ldq_size']
            self.stq_size = params['stq_size']

            #
            # Virtual memory
            #

            self.pg_offset_bits = params.get('pg_offset_bits', 12)
            self.pg_level_bits = 10 - log2_int(self.xlen // 32)
            self.pg_levels = params['pg_levels']

            self.sv_addr_bits = self.pg_offset_bits + self.pg_level_bits * self.pg_levels
            self.vpn_bits = self.vaddr_bits - self.pg_offset_bits
            self.ppn_bits = self.paddr_bits - self.pg_offset_bits

        #
        # GROOM
        #

        if self.is_groom:
            self.n_cores = params['n_cores']
            self.n_warps = params['n_warps']
            self.n_threads = params['n_threads']
            self.n_barriers = params['n_barriers']

            self.mem_width = self.n_threads
            self.max_br_count = 0

            #
            # Instruction fetch
            #

            self.fetch_bytes = 4

            #
            # Issue
            #

            self.issue_params = params['issue_params']
            self.issue_queue_depth = self.issue_params['queue_depth']

            #
            # Shared memory
            #

            self.use_smem = params.get('smem_params') is not None
            if self.use_smem:
                self.smem_params = params['smem_params']
                self.smem_base = self.smem_params['base']
                self.smem_size = self.smem_params['size']
                self.smem_banks = self.smem_params['n_banks']

            #
            # Rasterizer
            #

            self.use_raster = params['use_raster']


class MicroOp(HasCoreParams, Record):

    ID_WIDTH = 10

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        common_layout = [
            ('valid', 1),
            ('uop_id', MicroOp.ID_WIDTH),
            ('inst', 32),
            ('opcode', UOpCode),
            ('iq_type', IssueQueueType),
            ('fu_type', FUType),
            ('br_type', BranchType),
            ('opa_sel', OpA),
            ('opb_sel', OpB),
            ('imm_sel', ImmSel),
            ('alu_fn', ALUOperator),
            ('alu_dw', ALUWidth),
            ('csr_addr', 12),
            ('csr_cmd', CSRCommand),
            ('is_load', 1),
            ('is_sta', 1),
            ('is_std', 1),
            ('is_br', 1),
            ('is_jal', 1),
            ('is_jalr', 1),
            ('is_amo', 1),
            ('br_mask', self.max_br_count),
            ('imm_packed', 20),
            ('mem_cmd', MemoryCommand),
            ('mem_size', 2),
            ('mem_signed', 1),
            ('uses_ldq', 1),
            ('uses_stq', 1),
            ('is_fence', 1),
            ('is_fencei', 1),
            ('is_ecall', 1),
            ('clear_pipeline', 1),
            ('flush_on_commit', 1),
            ('exc_ae_if', 1),
            ('exc_pf_if', 1),
            ('bp_debug_if', 1),
            ('bp_exc_if', 1),
            ('exception', 1),
            ('exc_cause', self.xlen),
            ('ldst', 5),
            ('lrs1', 5),
            ('lrs2', 5),
            ('lrs3', 5),
            ('ldst_valid', 1),
            ('dst_rtype', RegisterType),
            ('lrs1_rtype', RegisterType),
            ('lrs2_rtype', RegisterType),
            ('fp_valid', 1),
            ('fp_single', 1),
            ('frs3_en', 1),
        ]

        if self.is_groom:
            groom_layout = [
                ('pc', 32),
                ('tmask', self.n_threads),
                ('stall_warp', 1),
                ('lsq_wid', range(self.n_warps)),
                ('lsq_tid', range(self.n_threads)),
            ]

            layout = common_layout + groom_layout
        else:
            room_layout = [
                ('is_rvc', 1),
                ('issue_uops', 2),
                ('br_tag', range(self.max_br_count)),
                ('ftq_idx', range(self.ftq_size)),
                ('edge_inst', 1),
                ('pc_lsb', range(self.fetch_bytes)),
                ('taken', 1),
                ('rob_idx', range(self.num_rob_rows * self.core_width)),
                ('ldq_idx', range(self.ldq_size)),
                ('stq_idx', range(self.stq_size)),
                ('pdst', range(self.num_pregs)),
                ('prs1', range(self.num_pregs)),
                ('prs2', range(self.num_pregs)),
                ('prs3', range(self.num_pregs)),
                ('prs1_busy', 1),
                ('prs2_busy', 1),
                ('prs3_busy', 1),
                ('stale_pdst', range(self.num_pregs)),
            ]

            layout = common_layout + room_layout

        Record.__init__(self, layout, name=name, src_loc_at=src_loc_at + 1)

    def allocate_brtag(self):
        return self.is_jalr | self.is_br

    def rf_wen(self):
        return self.dst_rtype != RegisterType.X

    def fu_type_has(self, typ):
        return (self.fu_type & typ) != 0
