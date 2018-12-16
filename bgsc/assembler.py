import re
import sys
import struct
import configparser

ENDIAN = 'little'

WORD, HALF, BYTE = 'word', 'half', 'byte'
SIZE = {WORD: 4, HALF: 2, BYTE: 1}

TEXT_SECTION, DATA_SECTION = 0, 1

REGISTER_MAP = {
    'EAX': 0x0,
    'ECX': 0x1,
    'EDX': 0x2,
    'ESP': 0x3,
    'EBP': 0x4,
    'R0':  0x0,
    'R1':  0x1,
    'R2':  0x2,
    'R3':  0x3,
    'R4':  0x4,
}

INSTRUCTION_MAP = {
    # LRX
    'L_SLL':   0x00,
    'L_SRL':   0x01,
    'L_SRLU':  0x02,
    'L_MUL':   0x03,
    'L_DIV':   0x04,
    'L_DIVU':  0x05,
    'L_MOD':   0x06,
    'L_MODU':  0x07,
    'L_ADD':   0x08,
    'L_SUB':   0x09,
    'L_MOV':   0x0A,
    'L_CMP':   0x0B,
    'L_CMPU':  0x0C,
    'L_AND':   0x0D,
    'L_OR':    0x0E,
    'L_XOR':   0x0F,

    # LRX-F
    'L_MULF':  0x12,
    'L_DIVF':  0x13,
    'L_ADDF':  0x14,
    'L_SUBF':  0x15,
    'L_CMPF':  0x16,

    # SRX
    'S_CVTFW': 0x17,
    'S_CVTWF': 0x18,
    'S_PUSH':  0x1A,
    'S_POP':   0x1B,
    'S_NOT':   0x1C,
    'S_RET':   0x1D,
    'S_HALT':  0x1E,
    'S_FREE':  0x1F,

    # IMM
    'I_LDW':   0x24,
    'I_LDB':   0x25,
    'I_STW':   0x26,
    'I_STB':   0x27,
    'I_SLL':   0x28,
    'I_SRL':   0x29,
    'I_SRLU':  0x2A,
    'I_ADD':   0x2B,
    'I_CMP':   0x2C,
    'I_CMPU':  0x2D,
    'I_LEA':   0x2E,
    'I_HEAP':  0x2F,

    # JA
    'J_JE':    0x34,
    'J_JNE':   0x35,
    'J_JL':    0x36,
    'J_JLE':   0x37,
    'J_JG':    0x38,
    'J_JGE':   0x39,
    'J_JMP':   0x3A,
    'J_PUSH':  0x3B,
    'J_CALL':  0x3C,
    'J_SYS':   0x3D,
}


class Syntax:
    COMMENT = ';'
    LABEL = ':'
    TOKEN = r'[\w\$\._%:-]+'
    TEXT = '.text'
    DATA = '.data'


class Context:
    labels = {}
    extern = {}
    ip = 0

    debug_info = {
        'LRX': 0,
        'SRX': 0,
        'IMM': 0,
        'JA':  0
    }

    def add_label(self, label):
        # TODO: Error check
        self.labels[label] = self.ip

    def update_labels(self, data: list):
        # Since the data section is parsed before we know the size
        # of the entire program, the data section labels are updated
        # to their original value + the size of the text section
        for d in data:
            if d.name is None:
                # Skip unlabled data
                continue
            self.labels[d.name] += self.ip

    def instruction_count(self):
        return self.debug_info['LRX'] + self.debug_info['SRX'] + \
               self.debug_info['IMM'] + self.debug_info['JA']


class Header:
    SIGN_MAGIC = 0
    SIGN_VERSION = 1
    STACK_SIZE = 2
    HEAP_SIZE = 3
    DATA_PTR = 4

    def __init__(self, context):
        self.value = [
            Value(context, 0x79, SIZE[BYTE]),   # Assembler signature (magic)
            Value(context, 0x30, SIZE[BYTE]),   # Assembler signature (version)
            Value(context, 0x100, SIZE[HALF]),  # Initial stack size @Temp
            Value(context, 0x00, SIZE[HALF]),   # Initial heap size
            Value(context, 0x00, SIZE[WORD]),   # Start of data section
        ]
        # context.ip += (2 * SIZE[WORD]) + SIZE[HALF]

    def compile(self, context):
        compiled_header = bytearray()

        for h in self.value:
            compiled_header += h.compile(context)

        return compiled_header

    def update(self, context):
        self.value[Header.DATA_PTR] = context.ip

    def __repr__(self):
        return '\n'.join([str(x) for x in self.value])


class Value:
    def __init__(self, context, value: str, size: int, name: str = None):
        self.value = value
        self.name = name
        self.size = size
        context.ip += size

    def compile(self, context):
        if self.value in context.labels:
            # The value of this data is a label
            # Look up label and use its value as the operand
            address = context.labels[self.value]

            # Addresses are unsigned words
            return address.to_bytes(self.size, byteorder=ENDIAN, signed=False)

        if isinstance(self.value, str) and self.value in context.extern:
            # The value of this data is an external address
            address = int(context.extern[self.value])

            # Addresses are unsigned words
            return address.to_bytes(self.size, byteorder=ENDIAN, signed=False)

        if isinstance(self.value, str) and '.' in self.value:
            # Convert floating point to integer representation
            imm = float(self.value)
            return bytearray(struct.pack('f', imm))

        # The value can be directly parsed as an integer and
        # converted to its byte representation
        imm = int(self.value)

        return imm.to_bytes(
            self.size, byteorder=ENDIAN, signed=(self.size != SIZE[BYTE]))

        # TODO: Floating point

    def __repr__(self):
        if self.size == 4:
            type = WORD
        elif self.size == 2:
            type = HALF
        else:
            type = BYTE

        if self.name:
            rep = 'Value(sizeof({}), {}, Label({}))'
            return rep.format(type, self.value, self.name)

        return 'Value(sizeof({}), {})'.format(type, self.value)


class Register:
    def __init__(self, context, name: str):
        self.name = name.upper()

        # Register addresses are 1 byte in length
        context.ip += SIZE[BYTE]

    def compile(self, context):
        if self.name in REGISTER_MAP:
            # Look up register name and return its byte representation
            return REGISTER_MAP[self.name].to_bytes(SIZE[BYTE], ENDIAN)

        print('Bad register name', self.name)

    def __repr__(self):
        return 'R({})'.format(self.name.lower())


class Instruction:
    def __init__(self, name: str, operands: list, context):
        self.name = name.upper()
        self.operands = tuple(operands)

        # Increment ip to fit the instruction's opcode
        context.ip += SIZE[BYTE]

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
        return '{}{}'.format(self.name.split('_')[-1].lower(), self.operands)


def parse_data(tok: list, context) -> list:
    data_type = tok[1].lower()
    label = tok[0][:-1]
    value = tok[2]

    if data_type == 'space':
        size = int(value)
        # First Word in an array indicates its size
        array = [Value(context, size, SIZE[WORD], label)]
        return array + [Value(context, '0', size)]

    data_size = SIZE[data_type]

    if len(tok) > 3:
        # This is an array of data_type
        elements = tok[2:]

        # First Word in an array indicates its size,
        # this is the array's base address.
        array = [Value(context, len(elements) * data_size, SIZE[WORD], label)]
        array += [Value(context, e, data_size) for e in elements]

        return array

    return [Value(context, value, data_size, label)]


def is_register(tok: str) -> bool:
    return tok.upper() in REGISTER_MAP


def is_lrx(tok: list) -> bool:
    if len(tok) != 3:
        return False

    return is_register(tok[1]) and is_register(tok[2])


def is_srx(tok: list) -> bool:
    if len(tok) != 2:
        return False
    return is_register(tok[1])


def is_imm(tok: list) -> bool:
    if len(tok) != 3:
        return False

    return is_register(tok[1]) and not is_register(tok[2])


def is_ja(tok: list) -> bool:
    if len(tok) != 2:
        return False

    return not is_register(tok[1])


def parse_instruction(tok: list, context) -> Instruction:
    name = tok[0].upper()

    if is_lrx(tok):
        # LRX (long register format) instruction
        # lrx $register, $register
        context.debug_info['LRX'] += 1
        operands = [Register(context, tok[1]), Register(context, tok[2])]
        return Instruction('L_'+name, operands, context)

    if is_srx(tok):
        # SRX (short register format) instruction
        # srx $register
        context.debug_info['SRX'] += 1
        operands = [Register(context, tok[1])]
        return Instruction('S_'+name, operands, context)

    if is_imm(tok):
        # IMM (immediate format) instruction
        # imm $register, imm
        context.debug_info['IMM'] += 1
        imm = Value(context, tok[2], SIZE[WORD])
        operands = [Register(context, tok[1]), imm]
        return Instruction('I_'+name, operands, context)

    if is_ja(tok):
        # JA (jump-address format) instruction
        # ja address
        context.debug_info['JA'] += 1
        operands = [Value(context, tok[1], SIZE[WORD])]
        return Instruction('J_'+name, operands, context)


def parse_str(line: str) -> str:
    m = re.search('\"(.*?)\"', line)

    if not m:
        return line

    literal = m.group(1)

    if len(literal) == 0:
        parsed = ''
    else:
        parsed = ''.join([str(ord(c)) + ',' for c in literal])

    return re.sub('\"(.*?)\"', parsed, line)


def parse(src: list) -> list:
    section = DATA_SECTION
    context = Context()

    header = Header(context)
    text = []
    data = []

    for line in src:
        line = line.split(Syntax.COMMENT)[0]
        line = parse_str(line)
        tok = re.findall(Syntax.TOKEN, line)

        if not tok:
            continue

        if tok[0] == Syntax.TEXT:
            section = TEXT_SECTION
            context.ip = 0xa
            continue

        if tok[0] == Syntax.DATA:
            section = DATA_SECTION
            context.ip = 0  # @Todo sizeof header
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

    header.value[Header.DATA_PTR].value = context.ip
    context.update_labels(data)

    return [header] + text + data, context


def compile(tokens: list, context):
    """ Compile tokens to bytecode """
    for tok in tokens:
        yield tok.compile(context)


def print_ast(tokens: list):
    [print(tok) for tok in tokens]


def run(filename: str, output: str = 'out.bgx', debug=False):
    """ Assemble filename to output """
    config = configparser.ConfigParser()
    config.read('asm.ini')

    with open(filename, 'r') as file:
        tokens, context = parse(file.readlines())

        if debug:
            print('-- parser output --')
            print_ast(tokens)
            print('-- end of parser output --\n')
            print(context.debug_info)
            print('instruction count:', context.instruction_count())

    program_size = 0
    context.extern = config['Extern']

    with open(output, 'wb+') as file:
        for compiled in compile(tokens, context):
            program_size += len(compiled)
            file.write(compiled)

    print('assembly {}, {} bytes'.format(output, program_size))


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
