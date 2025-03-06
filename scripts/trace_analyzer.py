from rich.console import Console
from rich.table import Table
from rich.text import Text
import distinctipy
import tinyrv
import ctypes

NUM_PREGS = 96
PREG_COLORS = [
    f'rgb({int(255*r)},{int(255*g)},{int(255*b)})'
    for r, g, b in distinctipy.get_colors(NUM_PREGS)
]

EXCEPTION_CAUSE = {
    0x0: 'Misaligned fetch',
    0x1: 'Fetch access fault',
    0x2: 'Illegal instruction',
    0x3: 'Breakpoint',
    0x4: 'Misaligned load',
    0x5: 'Load access fault',
    0x6: 'Misaligned store',
    0x7: 'Store access fault',
    0x8: 'Ecall (U)',
    0x9: 'Ecall (S)',
    0xA: 'Ecall (VS)',
    0xB: 'Ecall (M)',
    0xC: 'Fetch page fault',
    0xD: 'Load page fault',
    0xF: 'Store page fault',
}

INTERRUPT_CAUSE = {
    0x1: 'SSIP',
    0x3: 'MSIP',
    0x5: 'STIP',
    0x7: 'MTIP',
    0x9: 'SEIP',
    0xB: 'MEIP',
}


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
        self.decoded = False

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

        inst_text = Text()
        note_text = Text()

        illegal_insn = False

        op = tinyrv.decode(self.inst)

        rd_text = ''
        rs1_text = ''
        rs2_text = ''

        if (rd := op.args.get('rd')) is not None:
            rd_text = Text(self.int_regs[rd], style=PREG_COLORS[self.pdst])
        if (rs1 := op.args.get('rs1')) is not None:
            rs1_text = Text(self.int_regs[rs1], style=PREG_COLORS[self.prs1])
        if (rs2 := op.args.get('rs2')) is not None:
            rs2_text = Text(self.int_regs[rs2], style=PREG_COLORS[self.prs2])

        if (rd_p := op.args.get('rd_p')) is not None:
            rd_text = Text(self.int_regs[rd_p + 8],
                           style=PREG_COLORS[self.pdst])
        if (rd_n0 := op.args.get('rd_n0')) is not None:
            rd_text = Text(self.int_regs[rd_n0], style=PREG_COLORS[self.pdst])
        if (rd_n2 := op.args.get('rd_n2')) is not None:
            rd_text = Text(self.int_regs[rd_n2], style=PREG_COLORS[self.pdst])
        if (rd_rs1_p := op.args.get('rd_rs1_p')) is not None:
            rd_text = Text(self.int_regs[rd_rs1_p + 8],
                           style=PREG_COLORS[self.pdst])
            rs1_text = Text(self.int_regs[rd_rs1_p + 8],
                            style=PREG_COLORS[self.prs1])
        if (rd_rs1_n0 := op.args.get('rd_rs1_n0')) is not None:
            rd_text = Text(self.int_regs[rd_rs1_n0],
                           style=PREG_COLORS[self.pdst])
            rs1_text = Text(self.int_regs[rd_rs1_n0],
                            style=PREG_COLORS[self.prs1])
        if (rs1_p := op.args.get('rs1_p')) is not None:
            rs1_text = Text(self.int_regs[rs1_p + 8],
                            style=PREG_COLORS[self.prs1])
        if (rs1_n0 := op.args.get('rs1_n0')) is not None:
            rs1_text = Text(self.int_regs[rs1_n0],
                            style=PREG_COLORS[self.prs1])
        if (rs2_p := op.args.get('rs2_p')) is not None:
            rs2_text = Text(self.int_regs[rs2_p + 8],
                            style=PREG_COLORS[self.prs2])
        if (rs2_n0 := op.args.get('rs2_n0')) is not None:
            rs2_text = Text(self.int_regs[rs2_n0],
                            style=PREG_COLORS[self.prs2])

        op_name = op.name.replace('_', '.')

        if op.name in {
                'lb', 'lh', 'lw', 'ld', 'lbu', 'lhu', 'lwu', 'sb', 'sh', 'sw',
                'sd'
        }:
            inst_text.append(op.name.ljust(5))
            inst_text.append(' ')
            if 'rd' in op.args:
                inst_text.append(rd_text)
                inst_text.append(f'[={self.rd_data:x}]')
            else:
                inst_text.append(rs2_text)
                inst_text.append(f'[={self.data:x}]')
            inst_text.append(f', {op.imm12}(')
            inst_text.append(rs1_text)
            inst_text.append(f')[={self.addr:x}]')

            if op.name.startswith('l'):
                note_text.append(f'mem[{self.addr:x}] -> {self.rd_data:x}')
            else:
                note_text.append(f'mem[{self.addr:x}] <- {self.data:x}')

        elif op.name == 'jalr':
            if op.rs1 == 1 and op.imm12 == 0:
                inst_text.append('ret'.ljust(5))
                note_text.append(
                    f'Return address {self.rs1_data + op.imm12:x}')
            else:
                if 'rd' in op.args and op.rd == 0:
                    inst_text.append('jr'.ljust(5))
                else:
                    inst_text.append(op.name.ljust(5))
                inst_text.append(' ')
                if 'rd' in op.args:
                    inst_text.append(rd_text)
                    inst_text.append(f'[={self.rd_data:x}]')
                else:
                    inst_text.append(rs2_text)
                    inst_text.append(f'[={self.data:x}]')
                inst_text.append(', ')
                inst_text.append(rs1_text)
                inst_text.append(f'[={self.rs1_data + op.imm12:x}]')

        elif op.name.startswith('csr'):
            inst_text.append(op.name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')
            inst_text.append(tinyrv.csrs.get(op.csr, hex(op.csr)))
            inst_text.append(', ')
            if 'rs1' in op.args:
                inst_text.append(rs1_text)
                inst_text.append(f'[={self.rs1_data:x}]')
            else:
                inst_text.append(
                    f'{hex(op.zimm) if abs(op.zimm) > 255 else op.zimm}')

        elif 'rd_rs1_p' in op.args:
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')
            inst_text.append(rs1_text)
            inst_text.append(f'[={self.rs1_data:x}]')
            inst_text.append(', ')

            if 'rs2_p' in op.args:
                inst_text.append(rs2_text)
                inst_text.append(f'[={self.rs2_data:x}]')
            elif 'nzuimm6' in op.args:
                inst_text.append(str(op.nzuimm6))
            elif 'imm6' in op.args:
                inst_text.append(str(op.imm6))

        elif 'rd_rs1_n0' in op.args:
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')
            inst_text.append(rs1_text)
            inst_text.append(f'[={self.rs1_data:x}]')
            inst_text.append(', ')

            if 'rs2_n0' in op.args:
                inst_text.append(rs2_text)
                inst_text.append(f'[={self.rs2_data:x}]')
            elif 'nzuimm6' in op.args:
                inst_text.append(str(op.nzuimm6))
            elif 'nzimm6' in op.args:
                inst_text.append(str(op.nzimm6))
            elif 'imm6' in op.args:
                inst_text.append(str(op.imm6))

        elif 'rd_n0' in op.args:
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')

            if 'imm6' in op.args:
                inst_text.append(str(op.imm6))
            elif 'uimm8sp' in op.args:
                inst_text.append(f'{hex(op.uimm8sp)}(sp)')
                inst_text.append(f'[={self.rs1_data:x}]')
            elif 'uimm9sp' in op.args:
                inst_text.append(f'{hex(op.uimm9sp)}(sp)')
                inst_text.append(f'[={self.rs1_data:x}]')
            else:
                inst_text.append(rs2_text)
                inst_text.append(f'[={self.rs2_data:x}]')

        elif op.name in {'c_sw', 'c_sd', 'c_lw', 'c_ld'}:
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            if 'rs2_p' in op.args:
                inst_text.append(rs2_text)
                inst_text.append(f'[={self.data:x}]')
            else:
                inst_text.append(rd_text)
                inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')

            if 'uimm7' in op.args:
                inst_text.append(str(op.uimm7))
            else:
                inst_text.append(str(op.uimm8))
            inst_text.append('(')
            inst_text.append(rs1_text)
            inst_text.append(')')
            inst_text.append(f'[={self.addr:x}]')

            if op.name.startswith('c_l'):
                note_text.append(f'mem[{self.addr:x}] -> {self.rd_data:x}')
            else:
                note_text.append(f'mem[{self.addr:x}] <- {self.data:x}')

        elif op.name == 'c_lui':
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')
            inst_text.append(hex(op.nzimm18))

        elif op.name == 'c_swsp':
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rs2_text)
            inst_text.append(f'[={self.data:x}]')
            inst_text.append(f', ')
            inst_text.append(f'{hex(op.uimm8sp_s)}(sp)')
            inst_text.append(f'[={self.rs1_data:x}]')

        elif op.name == 'c_sdsp':
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rs2_text)
            inst_text.append(f'[={self.data:x}]')
            inst_text.append(f', ')
            inst_text.append(f'{hex(op.uimm9sp_s)}(sp)')
            inst_text.append(f'[={self.rs1_data:x}]')

        elif op.name == 'c_addi4spn':
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')
            inst_text.append(hex(op.nzuimm10))

        elif op.name == 'c_addi16sp':
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(hex(op.nzimm10))

        elif op.name == 'c_jalr':
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append('ra')
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')
            inst_text.append(rs1_text)
            inst_text.append(f'[={self.rs1_data:x}]')

        elif op.name == 'c_jr':
            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rs1_text)
            inst_text.append(f'[={self.rs1_data:x}]')

        elif op.name == 'c_j':
            target = hex(self.pc + op.imm12)

            inst_text.append(op_name.ljust(5))
            inst_text.append(' ')
            inst_text.append(target)

        elif 'rd' in op.args and 'rs1' in op.args:
            if op.name == 'addi' and op.imm12 == 0:
                if op.rd == 0 and op.rs1 == 0:
                    inst_text.append('nop')
                else:
                    inst_text.append('mv'.ljust(5))
                    inst_text.append(' ')
                    inst_text.append(rd_text)
                    inst_text.append(f'[={self.rd_data:x}]')
                    inst_text.append(', ')
                    inst_text.append(rs1_text)
                    inst_text.append(f'[={self.rs1_data:x}]')

            else:
                inst_text.append(op.name.ljust(5))
                inst_text.append(' ')
                inst_text.append(rd_text)
                inst_text.append(f'[={self.rd_data:x}]')
                inst_text.append(', ')
                inst_text.append(rs1_text)
                inst_text.append(f'[={self.rs1_data:x}]')
                if 'rs2' in op.args:
                    inst_text.append(', ')
                    inst_text.append(rs2_text)
                    inst_text.append(f'[={self.rs2_data:x}]')

                for name in ('imm12', 'shamtw', 'shamtd'):
                    if name in op.args:
                        inst_text.append(', ')
                        inst_text.append(
                            hex(op.args[name]) if abs(op.args[name]) >
                            255 else str(op.args[name]))

        elif op.name == 'jal' or ('bimm12' in op.args):
            target = hex(self.pc +
                         (op.jimm20 if 'jimm20' in op.args else op.bimm12))

            if 'rd' in op.args and op.rd == 0:
                inst_text.append('j'.ljust(5))
                inst_text.append(' ')
                inst_text.append(target)
            elif 'rd' in op.args and op.rd == 1:
                inst_text.append('call'.ljust(5))
                inst_text.append(' ')
                inst_text.append(target)
                note_text.append(f'Return address {self.rd_data:x}')
            else:
                sep = ''
                inst_text.append(op.name.ljust(5))
                inst_text.append(' ')
                if 'rd' in op.args:
                    inst_text.append(rd_text)
                    inst_text.append(f'[={self.rd_data:x}]')
                    sep = ', '

                if 'rs1' in op.args:
                    inst_text.append(sep)
                    inst_text.append(rs1_text)
                    inst_text.append(f'[={self.rs1_data:x}]')
                    sep = ', '

                if 'rs2' in op.args:
                    inst_text.append(sep)
                    inst_text.append(rs2_text)
                    inst_text.append(f'[={self.rs2_data:x}]')
                    sep = ', '

                inst_text.append(sep)
                inst_text.append(target)

        elif 'rd' in op.args and 'imm20' in op.args:
            inst_text.append(op.name.ljust(5))
            inst_text.append(' ')
            inst_text.append(rd_text)
            inst_text.append(f'[={self.rd_data:x}]')
            inst_text.append(', ')
            inst_text.append(
                hex(op.imm20) if abs(op.imm20) > 255 else str(op.imm20))

        elif op.valid():
            inst_text.append(op.name.ljust(5))
            inst_text.append(' ')
            inst_text.append(op.arg_str())

        else:
            illegal_insn = True

        if op.name in {'beq', 'bne', 'blt', 'bltu', 'bge', 'bgeu'}:
            taken = False

            match op.name:
                case 'beq':
                    taken = self.rs1_data == self.rs2_data
                case 'bne':
                    taken = self.rs1_data != self.rs2_data
                case 'blt':
                    taken = ctypes.c_long(self.rs1_data).value < ctypes.c_long(
                        self.rs2_data).value
                case 'bltu':
                    taken = self.rs1_data < self.rs2_data
                case 'bge':
                    taken = ctypes.c_long(
                        self.rs1_data).value >= ctypes.c_long(
                            self.rs2_data).value
                case 'bgeu':
                    taken = self.rs1_data >= self.rs2_data

            note_text.append('Taken' if taken else 'Not taken')

        if illegal_insn:
            inst_text = Text(hex(self.inst), style='red')
            note_text = Text('Illegal instruction', style='red')

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
                inst.decoded = True

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
                    if (v.br_mask & br_mask) != 0 or not v.decoded:
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

            elif cmd == 'EXC':
                cause = int(args[0])
                inst = int(args[1], base=16)

                if cause & (1 << 63):
                    cause = cause & 63
                    cause_str = f'Interrupt {INTERRUPT_CAUSE.get(cause, str(cause))}'
                else:
                    cause_str = EXCEPTION_CAUSE.get(cause, f'Cause {cause}')

                exc = Text(f'Exception ({cause_str})',
                           style='bold italic red underline')
                table.add_row('', '', exc, '')

            elif cmd == 'X':
                self.insts.clear()

        console.print(table)


if __name__ == '__main__':
    console = Console()

    parser = TraceParser()

    with console.pager(styles=True):
        with open('trace.log', 'r') as fin, open('trace.csv', 'w') as csv:
            parser.parse(fin, console=console, csv_file=csv)
