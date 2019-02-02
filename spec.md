## Registers

|   | Register | Description     |
|---|----------|-----------------|
| 0 | eax      | Accumulator     |
| 1 | edx      | Alt Accumulator |
| 2 | esp      | Stack pointer   |
| 3 | ebp      | Base pointer    |

## Aligned Instruction Encoding

| Format | Encoding                                                                   |
|--------|----------------------------------------------------------------------------|
| SRX    | `oooooooo xxxxxxxx dddddddd xxxxxxxx`                                      |
| LRX    | `oooooooo xxxxxxxx dddddddd ssssssss`                                      |
| IMM    | `oooooooo xxxxxxxx xxxxxxxx xxxxxxxxx iiiiiiii iiiiiiii iiiiiiii iiiiiiii` |
| IRX    | `oooooooo xxxxxxxx dddddddd xxxxxxxxx iiiiiiii iiiiiiii iiiiiiii iiiiiiii` |

## Dynamic Instruction Encoding

| Format | Encoding                                                |
|--------|---------------------------------------------------------|
| SRX    | `oooooooo dddddddd`                                     |
| LRX    | `oooooooo dddddddd ssssssss`                            |
| IMM    | `oooooooo iiiiiiii iiiiiiii iiiiiiii iiiiiiii`          |
| IMRX   | `oooooooo dddddddd iiiiiiii iiiiiiii iiiiiiii iiiiiiii` |

## LRX (Long Register Format)
These instructions take 2 registers as operands and perform arithmetic/logical operations. With the exception of compare instructions, which only set cpu flags, these operations store their result in the first register operand.

| Opcode | Mmemonic | op0 | op1 | Operation                              |
|--------|----------|-----|-----|----------------------------------------|
| 00     | SLL      | dst | src | `dst <- dst << src`                    |
| 01     | SRL      | dst | src | `dst <- dst >> src`                    |
| 02     | SRLU     | dst | src | `dst <- unsigned(dst) >> src`          |
| 03     | MUL      | dst | src | `dst <- dst * src`                     |
| 04     | DIV      | dst | src | `dst <- dst / src`                     |
| 05     | DIVU     | dst | src | `dst <- unsigned(dst) / unsigned(src)` |
| 06     | MOD      | dst | src | `dst <- dst % src`                     |
| 07     | MODU     | dst | src | `dst <- unsigned(dst) % unsigned(src)` |
| 08     | ADD      | dst | src | `dst <- dst + src`                     |
| 09     | SUB      | dst | src | `dst <- dst - src`                     |
| 0A     | MOV      | dst | src | `dst <- src`                           |
| 0B     | AND      | dst | src | `dst <- dst & src`                     |
| 0C     | OR       | dst | src | `dst <- dst | src`                     |
| 0D     | XOR      | dst | src | `dst <- dst ^ src`                     |
| 0E     | CEQ      | lhs | rhs | `eax <- lhs = rhs`                     |
| 0F     | CNE      | lhs | rhs | `cf <- lhs = rhs`                      |
| 10     | CLT      | lhs | rhs | `cf <- lhs = rhs`                      |
| 11     | CLE      | lhs | rhs | `cf <- lhs = rhs`                      |
| 12     | CGT      | lhs | rhs | `cf <- lhs = rhs`                      |
| 13     | CGE      | lhs | rhs | `cf <- lhs = rhs`                      |
| 14     | CLTU     | lhs | rhs | `cf <- lhs = rhs`                      |
| 15     | CLEU     | lhs | rhs | `cf <- lhs = rhs`                      |
| 16     | CGTU     | lhs | rhs | `cf <- lhs = rhs`                      |
| 17     | CGEU     | lhs | rhs | `cf <- lhs = rhs`                      |
| 18     | CLTF     | lhs | rhs | `cf <- lhs < rhs                       |
| 19     | CLEF     | lhs | rhs | `eax <- lhs <= rhs`                    |
| 1A     | CGTF     | lhs | rhs | `eax <- lhs > rhs`                     |
| 1B     | CGEF     | lhs | rhs | `eax <- lhs >= rhs`                    |
| 1C     | MULF     | dst | src | `dst <- float(dst) * float(src)`       |
| 1D     | DIVF     | dst | src | `dst <- float(dst) / float(src)`       |
| 1E     | ADDF     | dst | src | `dst <- float(dst) + float(src)`       |
| 1F     | SUBF     | dst | src | `dst <- float(dst) - float(src)`       |

## SRX (Short Register Format)
These instructions take a single register as an operand. Note that the `heap` instruction returns the address of the allocated memory to the `eax` register.

| Opcode | Mmemonic | op0 | op1 | Operation                            |
|--------|----------|-----|-----|--------------------------------------|
| 20     | CVTFW    | dst |     | `dst <- word(dst)`                   |
| 21     | CVTWF    | dst |     | `dst <- float(dst)`                  |
| 22     | ALLOC    | src |     | `eax <- heap->alloc(size: src)`      |
| 23     | FREE     | src |     | `heap->free(address: src)`           |
| 24     | PUSH     | src |     | `esp <- esp - 4; memory[esp] <- src` |
| 25     | POP      | dst |     | `dst <- memory[esp]; esp <- esp + 4` |
| 26     | NOT      | dst |     | `dst <- ~dst`                        |
| 27     | NEG      | dst |     | `dst <- -dst`                        |
| 28     | NEGF     | dst |     | `dst <- -dst`                        |
| 29     | RET      | src |     | `ip <- src`                          |
| 2A     | HALT     | src |     | `exit_code <- src; stop execution`   |

## IRX (Immediate-Register Format)
These instructions take a register as the first operand, and a 32-bit immediate value as the second.
Note that for load instructions the value read from memory is returned in the `eax` register, and store instructions write the value from the `eax` register to memory.

| Opcode | Mmemonic | op0 | op1 | Operation                         |
|--------|----------|-----|-----|-----------------------------------|
| 2D     | LDW      | src | imm | `eax <- memory[src + imm] (word)` |
| 2E     | LEA      | dst | imm | `dst <- imm`                      |
| 2F     | STW      | src | imm | `memory[src + imm] <- eax (word)` |
| 30     | ADD      | dst | imm | `dst <- dst + imm`                |
| 31     | SLL      | dst | imm | `dst <- dst << imm`               |

## IMM (Immediate Format)

These instructions take a 32-bit immediate value as the only operand. These include most of the branching instructions. Note that the call instruction pushes a pointer to the next instruction before branching to the address. The call external instruction writes the return value from the call to the eax register.

| Opcode | Mmemonic | op0     | op1 | Operation                            |
|--------|----------|---------|-----|--------------------------------------|
| 3A     | JMP      | address |     | `ip <- address`                      |
| 3B     | JE       | address |     | `(cf = EQ): ip <- address`           |
| 3C     | JNE      | address |     | `(cf <> EQ): ip <- address`          |
| 3D     | PUSH     | imm     |     | `esp <- esp - 4; memory[esp] <- imm` |
| 3E     | CALL     | address |     | `push(ip + sizeof(IMM)); ip <- imm`  |
| 3F     | CALX     | imm     |     | `eax <- System.Execute(imm)`         |
