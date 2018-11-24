import re
import sys

ENDIAN = 'little'

WORD, BYTE = 'word', 'byte'
SIZE = {'word': 4, 'byte': 1}

TEXT_SECTION, DATA_SECTION = 0, 1

REGISTER_MAP = {
    '$A0': 0x0,
    '$A1': 0x1,
    '$A2': 0x2,
    '$A3': 0x3,
    '$A4': 0x4,
    '$A5': 0x5,
    '$A6': 0x6,
    '$A7': 0x7,
    '$RA': 0x8,
    '$RV': 0x9,
    '$AX': 0xA,
    '$AC': 0xB,
    '$CX': 0xC,
    '$T0': 0xD,
    '$T1': 0xE,
    '$T2': 0xF
}

INSTRUCTION_MAP = {
    # LRX
    'SLL':   0x00,
    'SRL':   0x01,
    'SRLU':  0x02,
    'MUL':   0x03,
    'DIV':   0x04,
    'DIVU':  0x05,
    'MOD':   0x06,
    'MODU':  0x07,
    'ADD':   0x08,
    'SUB':   0x09,
    'MOV':   0x0A,
    'CMP':   0x0B,
    'CMPU':  0x0C,
    'AND':   0x0D,
    'OR':    0x0E,
    'XOR':   0x0F,
    'LW':    0x10,
    'LB':    0x11,
    'SW':    0x12,
    'SB':    0x13,

    # LRX-F
    'CVTFW': 0x16,
    'CVTWF': 0x17,
    'MULF':  0x18,
    'DIVF':  0x19,
    'ADDF':  0x1A,
    'SUBF':  0x1B,
    'CMPF':  0x1C,

    # SRX
    'PUSH':  0x20,
    'POP':   0x21,
    'HALT':  0x22,
    'RET':   0x23,
    'NOT':   0x24,

    # IMM
    'SLLI':  0x28,
    'SRLI':  0x29,
    'SRLUI': 0x2A,
    'ADDI':  0x2B,
    'CMPI':  0x2C,
    'CMPUI': 0x2D,
    'LEA':   0x2E,

    # JA
    'JMP':   0x35,
    'JE':    0x36,
    'JNE':   0x37,
    'JL':    0x38,
    'JG':    0x39,
    'CALL':  0x3A,
    'SYS':   0x3B,
}


class Syntax:
    COMMENT = '#'
    LABEL = ':'
    TOKEN = r'[\w\$\._%:-]+'
    TEXT = '.text'
    DATA = '.data'


class Context:
    labels = {}
    pc = 0

    def add_label(self, label):
        # TODO: Error check
        self.labels[label] = self.pc

    def update_labels(self, data: list):
        # Since the data section is parsed before we know the size
        # of the entire program, the data section labels are updated
        # to their original value + the size of the text section
        for d in data:
            if d.name is None:
                # Skip unlabled data
                continue
            self.labels[d.name] += self.pc


class Value:
    def __init__(self, context, value: str, size: int, name: str = None):
        self.value = value
        self.name = name
        self.size = size
        context.pc += size

    def compile(self, context):
        if self.value in context.labels:
            # The value of this data is a label
            # Look up label and use its value as the operand
            address = context.labels[self.value]

            # Addresses are unsigned words
            return address.to_bytes(self.size, byteorder=ENDIAN, signed=False)

        # The value can be directly parsed as an integer and
        # converted to its byte representation
        imm = int(self.value)
        return imm.to_bytes(self.size, ENDIAN, signed=True)

        # TODO: Floating point

    def __repr__(self):
        if self.name:
            rep = 'Value[{}]({}, Label({}))'
            return rep.format(self.size, self.value, self.name)

        return 'Value[{}]({})'.format(self.size, self.value)


class Register:
    def __init__(self, context, name: str):
        self.name = name.upper()
        
        # Register addresses are 1 byte in length
        context.pc += SIZE[BYTE]

    def compile(self, context):
        if self.name in REGISTER_MAP:
            # Look up register name and return its byte representation
            return REGISTER_MAP[self.name].to_bytes(SIZE[BYTE], ENDIAN)

        print('Bad register name')

    def __repr__(self):
        return 'R({})'.format(self.name.lower())


class Instruction:
    def __init__(self, name: str, operands: list, context):
        self.name = name.upper()
        self.operands = tuple(operands)
        
        # Increment pc to fit the instruction's opcode
        context.pc += SIZE[BYTE]

    def compile(self, context) -> list:
        opcode = INSTRUCTION_MAP[self.name]

        # Convert opcode to bytearray
        bin_opcode = opcode.to_bytes(SIZE[BYTE], ENDIAN, signed=False)
        compiled_instruction = bytearray(bin_opcode)

        for op in self.operands:
            # Compile each operand
            compiled_instruction += op.compile(context)

        return compiled_instruction

    def __repr__(self):
        return '{}{}'.format(self.name, self.operands)


def parse_data(tok: list, context) -> list:
    data_type = tok[1].lower()
    data_size = SIZE[data_type]

    label = tok[0][:-1]
    value = tok[2]

    if len(tok) > 3:
        # This is an array of data_type
        elements = tok[2:]

        # First Word in an array indicates its size,
        # this is the array's base address.
        array = [Value(context, len(elements) * data_size, data_size, label)]
        array += [Value(context, e, data_size) for e in elements]

        return array

    return [Value(context, value, data_size, label)]


def parse_instruction(tok: list, context) -> Instruction:
    name = tok[0].upper()
    opcode = INSTRUCTION_MAP[name]

    if opcode < 0x20:
        # LRX (long register format) instruction
        # lrx $register, $register
        operands = [Register(context, tok[1]), Register(context, tok[2])]
        return Instruction(name, operands, context)

    if opcode < 0x28:
        # SRX (short register format) instruction
        # srx $register
        operands = [Register(context, tok[1])]
        return Instruction(name, operands, context)

    if opcode < 0x35:
        # IMM (immediate format) instruction
        # imm $register, imm
        imm = Value(context, tok[2], SIZE[WORD])
        operands = [Register(context, tok[1]), imm]
        return Instruction(name, operands, context)

    if opcode < 0x40:
        # JA (jump-address format) instruction
        # ja address
        operands = [Value(context, tok[1], SIZE[WORD])]
        return Instruction(name, operands, context)


def parse(src: list) -> list:
    section = DATA_SECTION
    context = Context()

    text = []
    data = []

    for line in src:
        line = line.split(Syntax.COMMENT)[0]
        tok = re.findall(Syntax.TOKEN, line)

        if not tok:
            continue

        if tok[0] == Syntax.TEXT:
            section = TEXT_SECTION
            context.pc = 0
            continue

        if tok[0] == Syntax.DATA:
            section = DATA_SECTION
            context.pc = 0
            continue

        if tok[0][-1] == Syntax.LABEL:
            # Register this label to the context
            context.add_label(tok[0][:-1])
            if len(tok) == 1:
                # Line only contains a label and nothing else
                continue

        if section == DATA_SECTION:
            # Parse data definition
            token = parse_data(tok, context)
            data += token
            continue

        if section == TEXT_SECTION:
            # Parse instruction
            if tok[0][-1] == Syntax.LABEL:
                # Strip label
                tok.pop(0)

            token = parse_instruction(tok, context)
            text.append(token)

    context.update_labels(data)
    return text + data, context


def compile(tokens: list, context):
    """ Compile tokens to bytecode """
    for tok in tokens:
        yield tok.compile(context)


def print_ast(tokens: list):
    [print(tok) for tok in tokens]


def run(filename: str, output: str = 'out.bgx', debug=False):
    """ Assemble filename to output """
    with open(filename, 'r') as file:
        tokens, context = parse(file.readlines())

        if debug:
            print_ast(tokens)

    with open(output, 'wb+') as file:
        for compiled in compile(tokens, context):
            file.write(compiled)


def main(argv: list):
    if len(argv) == 0:
        print('Missing input')
        return

    if len(argv) == 1:
        run(argv[0])
        return

    dbg_flag = argv[-1] == '-d'

    if len(argv) == 2:
        run(argv[0], debug=dbg_flag)
        return

    run(argv[0], argv[1], dbg_flag)


if __name__ == '__main__':
    main(sys.argv[1:])
