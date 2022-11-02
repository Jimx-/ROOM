from rich.console import Console
from rich.table import Table
from rich.text import Text

import riscvmodel.insn as insn
import ctypes

PREG_COLORS = [
    'rgb(0,255,0)', 'rgb(255,0,255)', 'rgb(0,127,255)', 'rgb(255,127,0)',
    'rgb(127,191,127)', 'rgb(120,45,139)', 'rgb(48,130,3)', 'rgb(253,133,190)',
    'rgb(236,0,30)', 'rgb(0,0,255)', 'rgb(0,255,255)', 'rgb(255,255,0)',
    'rgb(0,255,127)', 'rgb(0,127,127)', 'rgb(139,141,248)', 'rgb(0,0,127)',
    'rgb(131,254,21)', 'rgb(130,245,232)', 'rgb(160,71,1)', 'rgb(135,2,253)',
    'rgb(234,222,133)', 'rgb(243,49,144)', 'rgb(152,153,28)', 'rgb(8,65,59)',
    'rgb(42,192,193)', 'rgb(109,4,37)', 'rgb(204,74,250)', 'rgb(23,65,197)',
    'rgb(32,193,62)', 'rgb(189,119,111)', 'rgb(95,114,97)', 'rgb(229,184,251)',
    'rgb(91,134,179)', 'rgb(235,182,51)', 'rgb(102,82,233)', 'rgb(178,27,79)',
    'rgb(87,253,106)', 'rgb(240,65,42)', 'rgb(183,8,163)', 'rgb(67,0,177)',
    'rgb(69,66,2)', 'rgb(162,250,148)', 'rgb(160,192,207)', 'rgb(82,203,4)',
    'rgb(167,100,194)', 'rgb(203,242,59)', 'rgb(101,59,70)', 'rgb(31,10,58)',
    'rgb(80,249,178)', 'rgb(43,48,131)', 'rgb(2,187,128)', 'rgb(62,248,40)',
    'rgb(178,205,1)', 'rgb(249,131,78)', 'rgb(100,192,241)', 'rgb(4,137,191)',
    'rgb(200,168,155)', 'rgb(5,177,0)', 'rgb(205,236,209)', 'rgb(0,187,250)',
    'rgb(68,27,251)', 'rgb(155,48,209)', 'rgb(12,135,63)', 'rgb(202,111,36)',
    'rgb(170,12,6)', 'rgb(105,180,66)', 'rgb(251,2,106)', 'rgb(179,70,122)',
    'rgb(88,2,100)', 'rgb(203,131,254)', 'rgb(181,178,90)', 'rgb(249,4,179)',
    'rgb(5,241,195)', 'rgb(83,86,157)', 'rgb(65,210,136)', 'rgb(107,111,32)',
    'rgb(5,56,253)', 'rgb(69,156,126)', 'rgb(252,104,247)', 'rgb(142,228,79)',
    'rgb(72,131,245)', 'rgb(253,171,127)', 'rgb(69,251,244)',
    'rgb(203,11,229)', 'rgb(252,48,238)', 'rgb(140,117,146)',
    'rgb(218,100,160)', 'rgb(204,60,187)', 'rgb(253,207,197)',
    'rgb(91,40,192)', 'rgb(10,89,8)', 'rgb(5,11,196)', 'rgb(126,2,183)',
    'rgb(119,214,178)', 'rgb(208,157,3)', 'rgb(251,89,111)'
]


class Instruction:

    int_regs = [
        'zero', 'ra', 'sp', 'gp', 'tp', 't0', 't1', 't2', 's0', 's1', 'a0',
        'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 's2', 's3', 's4', 's5', 's6',
        's7', 's8', 's9', 's10', 's11', 't3', 't4', 't5', 't6'
    ]

    def __init__(self, uop_id, pc, inst):
        self.uop_id = uop_id
        self.pc = pc
        self.inst = inst

        self.br_mask = -1

        self.pdst = 0
        self.rd_data = 0
        self.prs1 = 0
        self.rs1_data = 0
        self.prs2 = 0
        self.rs2_data = 0

        self.addr = 0
        self.data = 0

    def commit(self, table):
        uop_id = f'{self.uop_id:x}'
        pc = f'{self.pc:x}'

        inst = f'{self.inst:032b}'

        opcode = int(inst[-7:], 2)
        funct3 = int(inst[-15:-12], 2)
        funct7 = int(inst[-32:-25], 2)
        funct6 = int(inst[-32:-26], 2)
        funct5 = int(inst[-25:-20], 2)
        funct12 = int(inst[-32:-20], 2)
        rd = self.int_regs[int(inst[-12:-7], 2)]
        rs1 = self.int_regs[int(inst[-20:-15], 2)]
        rs2 = self.int_regs[int(inst[-25:-20], 2)]

        shamt5 = int(inst[-25:-20], 2)
        shamt6 = int(inst[-26:-20], 2)
        imm_I = ctypes.c_int(int(inst[-32] * 21 + inst[-31:-20], 2)).value
        imm_S = ctypes.c_int(
            int(inst[-32] * 21 + inst[-31:-25] + inst[-12:-7], 2)).value
        imm_B = ctypes.c_int(
            int(inst[-32] * 20 + inst[-8] + inst[-31:-25] + inst[-12:-8] + '0',
                2)).value
        imm_U = ctypes.c_int(
            int(inst[-32] + inst[-31:-20] + inst[-20:-12] + '0' * 12, 2)).value
        imm_J = ctypes.c_int(
            int(
                inst[-32] * 12 + inst[-20:-12] + inst[-21] + inst[-31:-21] +
                '0', 2)).value

        inst_text = Text()
        note_text = Text()

        OPV = lambda name: getattr(insn, f'Instruction{name}'
                                   ).field_opcode.value
        F3 = lambda name: getattr(insn, f'Instruction{name}'
                                  ).field_funct3.value
        F7 = lambda name: getattr(insn, f'Instruction{name}'
                                  ).field_funct7.value

        rd_text = Text(rd, style=PREG_COLORS[self.pdst])
        rs1_text = Text(rs1, style=PREG_COLORS[self.prs1])
        rs2_text = Text(rs2, style=PREG_COLORS[self.prs2])

        illegal_insn = False

        if opcode == OPV('ADDI'):
            opstr = ''

            for name in ['addi', 'slti', 'sltiu', 'xori', 'ori', 'andi']:
                if funct3 == F3(name.upper()):
                    opstr = name

            inst_text.append(opstr.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')
            inst_text.append(rs1_text)
            inst_text.append(f'[={self.rs1_data:x}]')
            inst_text.append(', ')
            inst_text.append(str(imm_I))

            if funct3 == F3('ADDI'):
                if rs1 == 'zero' and rs2 == 'zero' and imm_I == 0:
                    inst_text = 'nop'
                elif imm_I == 0:
                    inst_text = Text('mv    ')
                    inst_text.append(rd_text)
                    inst_text.append(f'[={self.rd_data:x}]')
                    inst_text.append(', ')
                    inst_text.append(rs1_text)
                    inst_text.append(f'[={self.rs1_data:x}]')

        elif opcode == OPV('ADD'):
            opstr = ''

            for name in ['add', 'slt', 'sltu', 'xor', 'or', 'and']:
                if funct3 == F3(name.upper()):
                    opstr = name

            inst_text.append(opstr.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')
            inst_text.append(rs1_text)
            inst_text.append(f'[={self.rs1_data:x}]')
            inst_text.append(', ')
            inst_text.append(rs2_text)
            inst_text.append(f'[={self.rs2_data:x}]')

        elif opcode == OPV('LD'):
            opstr = ''

            size = {0: 'b', 1: 'h', 2: 'w', 3: 'd'}[funct3 & 3]
            unsigned = {0: '', 1: 'u'}[(funct3 >> 2) & 1]

            opstr = 'l' + size + unsigned

            inst_text.append(opstr.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(f', {imm_I}(')
            inst_text.append(rs1_text)
            inst_text.append(f')[={self.addr:x}]')

            note_text.append(f'MEM[{self.addr:x}] -> {self.rd_data:x}')

        elif opcode == OPV('SD'):
            opstr = ''

            size = {0: 'b', 1: 'h', 2: 'w', 3: 'd'}[funct3 & 3]

            opstr = 's' + size

            inst_text.append(opstr.ljust(5))
            inst_text.append(' ')
            inst_text.append(rs2_text)
            inst_text.append(f'[={self.data:x}]')
            inst_text.append(f', {imm_S}(')
            inst_text.append(rs1_text)
            inst_text.append(f')[={self.addr:x}]')

            note_text.append(f'MEM[{self.addr:x}] <- {self.data:x}')

        elif opcode == OPV('BEQ'):
            opstr = ''

            if funct3 == F3('BEQ'):
                opstr = 'beq'
                taken = self.rs1_data == self.rs2_data
            elif funct3 == F3('BNE'):
                opstr = 'bne'
                taken = self.rs1_data != self.rs2_data
            elif funct3 == F3('BLT'):
                opstr = 'blt'
                taken = ctypes.c_long(self.rs1_data).value < ctypes.c_long(
                    self.rs2_data).value
            elif funct3 == F3('BLTU'):
                opstr = 'bltu'
                taken = self.rs1_data < self.rs2_data
            elif funct3 == F3('BGE'):
                opstr = 'bge'
                taken = ctypes.c_long(self.rs1_data).value >= ctypes.c_long(
                    self.rs2_data).value
            elif funct3 == F3('BGEU'):
                opstr = 'bgeu'
                taken = self.rs1_data >= self.rs2_data
            else:
                illegal_insn = True

            inst_text.append(opstr.ljust(5))
            inst_text.append(' ')
            inst_text.append(rs1_text)
            inst_text.append(f'[={self.rs1_data:x}]')
            inst_text.append(', ')
            inst_text.append(rs2_text)
            inst_text.append(f'[={self.rs2_data:x}]')
            inst_text.append(f', {imm_B}[={self.pc + imm_B:x}]')

            note_text.append('Taken' if taken else 'Not taken')

        elif opcode == OPV('JAL'):
            if rd == 'zero':
                inst_text.append(f'j     {imm_J}[={self.pc + imm_J:x}]')
            elif rd == 'ra':
                inst_text.append(f'call  {imm_J}[={self.pc + imm_J:x}]')
                note_text.append(f'Return address {self.rd_data:x}')
            else:
                inst_text.append('jal'.ljust(5))
                inst_text.append(' ')
                inst_text.append(rd_text)
                inst_text.append(f'[={self.rd_data:x}]')
                inst_text.append(f', {imm_J}[={self.pc + imm_J:x}]')

        elif opcode == OPV('JALR'):
            if rs1 == 'ra':
                inst_text.append('ret')
                note_text.append(f'Return address {self.rs1_data + imm_I:x}')
            elif rd == 'zero':
                inst_text.append('jr'.ljust(5))
                inst_text.append(' ')
                inst_text.append(f'{imm_I}(')
                inst_text.append(rs1_text)
                inst_text.append(f')[={self.rs1_data + imm_I:x}]')
            else:
                inst_text.append('jalr'.ljust(5))
                inst_text.append(' ')
                inst_text.append(rd_text)
                inst_text.append(f'[={self.rd_data:x}]')
                inst_text.append(f', {imm_I}(')
                inst_text.append(rs1_text)
                inst_text.append(f')[={self.rs1_data + imm_I:x}]')

        elif opcode == OPV('AUIPC'):
            inst_text.append('auipc'.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(f', {imm_U:x}')
            inst_text.append(f'[={self.pc + imm_U:x}]')

        elif opcode == OPV('LUI'):
            inst_text.append('lui'.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(f', {imm_U:x}')

        else:
            illegal_insn = True

        if illegal_insn:
            inst_text = Text(hex(self.inst), style='red')

        table.add_row(uop_id, pc, inst_text, note_text)
        return (uop_id, pc, inst_text, note_text)


class TraceParser:

    def __init__(self):
        self.insts = {}

    def parse(self, fin, console, csv_file=None):
        self.insts.clear()

        table = Table(title='Instruction trace')

        table.add_column('ID')
        table.add_column('PC', justify='right', style='cyan', no_wrap=True)
        table.add_column('Instruction')
        table.add_column('Note')

        for line in fin.readlines():
            line = line.strip()

            if not line: continue

            cmd, *args = line.split()

            if cmd == '+':
                continue

            elif cmd == 'I':
                id = int(args[0])
                pc = int(args[1], base=16)
                inst = int(args[2], base=16)

                if self.insts.get(id) is not None:
                    raise ValueError(f'Duplicate micro-op ID: {id}')

                self.insts[id] = Instruction(id, pc, inst)

            elif cmd == 'ID':
                id = int(args[0])
                br_mask = int(args[1], base=16)

                inst = self.insts.get(id)
                if inst is None:
                    raise ValueError(f'Micro-op {id} not found')

                inst.br_mask = br_mask

            elif cmd == 'EX':
                id = int(args[0])
                prs1 = int(args[2])
                rs1_data = int(args[3], base=16)
                prs2 = int(args[4])
                rs2_data = int(args[5], base=16)

                inst = self.insts.get(id)
                if inst is None:
                    raise ValueError(f'Micro-op {id} not found')

                inst.prs1 = prs1
                inst.rs1_data = rs1_data
                inst.prs2 = prs2
                inst.rs2_data = rs2_data

            elif cmd == 'MEM':
                id = int(args[0])
                opcode = int(args[1])
                prs1 = int(args[2])
                prs2 = int(args[3])
                addr = int(args[4], base=16)
                data = int(args[5], base=16)

                inst = self.insts.get(id)
                if inst is None:
                    raise ValueError(f'Micro-op {id} not found')

                inst.prs1 = prs1
                inst.prs2 = prs2

                if opcode == 2:
                    inst.addr = addr
                elif opcode == 3:
                    inst.data = data
                else:
                    inst.addr = addr
                    inst.data = data

            elif cmd == 'WB':
                id = int(args[0])
                pdst = int(args[1])
                rd_data = int(args[2], base=16)

                inst = self.insts.get(id)
                if inst is None:
                    raise ValueError(f'Micro-op {id} not found')

                inst.pdst = pdst
                inst.rd_data = rd_data

            elif cmd == 'BRK':
                br_mask = int(args[0], base=16)

                killed = []
                for k, v in self.insts.items():
                    if v.br_mask & br_mask != 0:
                        killed.append(k)

                for k in killed:
                    self.insts.pop(k)

            elif cmd == 'BRR':
                br_mask = int(args[0], base=16)

                for v in self.insts.values():
                    v.br_mask &= ~br_mask

            elif cmd == 'C':
                id = int(args[0])

                inst = self.insts.get(id)
                if inst is None:
                    raise ValueError(f'Micro-op {id} not found')

                uop_id, pc, uop, note = inst.commit(table)
                if csv_file is not None:
                    csv_file.write(f'"{uop_id}","{pc}","{uop}","{note}"\n')

                self.insts.pop(id)

            elif cmd == 'X':
                self.insts.clear()

        console.print(table)


if __name__ == '__main__':
    console = Console()

    parser = TraceParser()

    with console.pager(styles=True):
        with open('trace.log', 'r') as fin, open('trace.csv', 'w') as csv:
            parser.parse(fin, console=console, csv_file=csv)
