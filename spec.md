## Registers

|   | Register | Description     |
|---|----------|-----------------|
| 0 | eax      | Accumulator     |
| 1 | ecx      | Counter         |
| 2 | edx      | Alt Accumulator |
| 3 | esp      | Stack pointer   |
| 4 | ebp      | Base pointer    |

## Instruction Encoding

| Format | Encoding                                                |
|--------|---------------------------------------------------------|
| SRX    | `oooooooo dddddddd`                                     |
| LRX    | `oooooooo dddddddd ssssssss`                            |
| IMM    | `oooooooo iiiiiiii iiiiiiii iiiiiiii iiiiiiii`          |
| IMRX   | `oooooooo dddddddd iiiiiiii iiiiiiii iiiiiiii iiiiiiii` |

## Packed Encoding (Future Spec)
Currently unavailable.

| Format | Encoding                                        |
|--------|-------------------------------------------------|
| SRX    | `oooooodd`                                      |
| LRX    | `ooooddss`                                      |
| LRX-F  | `oooooooo ddddssss`                             |
| IMM    | `oooooooo iiiiiiii iiiiiiii iiiiiiii iiiiiiii`  |
| IMRX   | `oooooodd  iiiiiiii iiiiiiii iiiiiiii iiiiiiii` |

## LRX (Long Register Format)
These instructions take 2 registers as operands and perform arithmetic/logical operations. With the exception of compare instructions, which only set cpu flags, these operations store their result in the first register operand.

| Opcode | Mmemonic | op0 | op1 | Operation                                                       |
|--------|----------|-----|-----|-----------------------------------------------------------------|
| 00     | SLL      | dst | src | `dst <- dst << src`                                             |
| 01     | SRL      | dst | src | `dst <- dst >> src`                                             |
| 02     | SRLU     | dst | src | `dst <- unsigned(dst) >> src`                                   |
| 03     | MUL      | dst | src | `dst <- dst * src`                                              |
| 04     | DIV      | dst | src | `dst <- dst / src`                                              |
| 05     | DIVU     | dst | src | `dst <- unsigned(dst) / unsigned(src)`                          |
| 06     | MOD      | dst | src | `dst <- dst % src`                                              |
| 07     | MODU     | dst | src | `dst <- unsigned(dst) % unsigned(src)`                          |
| 08     | ADD      | dst | src | `dst <- dst + src`                                              |
| 09     | SUB      | dst | src | `dst <- dst - src`                                              |
| 0A     | MOV      | dst | src | `dst <- src`                                                    |
| 0B     | CMP      | lhs | rhs | `cf <- (lhs < rhs): BELOW, (lhs > rhx): ABOVE, (lhs = rhs): EQ` |
| 0C     | CMPU     | lhs | rhs | `cf <- unsigned comparison`                                     |
| 0D     | AND      | dst | src | `dst <- dst & src`                                              |
| 0E     | OR       | dst | src | `dst <- dst | src`                                              |
| 0F     | XOR      | dst | src | `dst <- dst ^ src`                                              |
| 12     | MULF     | dst | src | `dst <- float(dst) * float(src)`                                |
| 13     | DIVF     | dst | src | `dst <- float(dst) / float(src)`                                |
| 14     | ADDF     | dst | src | `dst <- float(dst) + float(src)`                                |
| 15     | SUBF     | dst | src | `dst <- float(dst) - float(src)`                                |
| 16     | CMPF     | lhs | rhs | `cf <- floating-point comparison`                               |

## SRX (Short Register Format)
These instructions take a single register as an operand. Note that the `heap` instruction returns the address of the allocated memory to the `eax` register.

| Opcode | Mmemonic | op0 | op1 | Operation                            |
|--------|----------|-----|-----|--------------------------------------|
| 17     | CVTFW    | dst |     | `dst <- word(dst)`                   |
| 18     | CVTWF    | dst |     | `dst <- float(dst)`                  |
| 19     | HEAP     | src |     | `eax <- heap->alloc(size: src)`      |
| 1A     | PUSH     | src |     | `esp <- esp - 4; memory[esp] <- src` |
| 1B     | POP      | dst |     | `dst <- memory[esp]; esp <- esp + 4` |
| 1C     | NOT      | dst |     | `dst <- ~dst`                        |
| 1D     | RET      | src |     | `ip <- src`                          |
| 1E     | HALT     | src |     | `exit_code <- src; stop execution`   |
| 1F     | FREE     | src |     | `heap->free(address: src)`           |

## IMRX (Immediate-Register Format)
These instructions take a register as the first operand, and a 32-bit immediate value as the second.
Note that for load instructions the value read from memory is returned in the `eax` register, and store instructions write the value from the `eax` register to memory.

| Opcode | Mmemonic | op0 | op1 | Operation                         |
|--------|----------|-----|-----|-----------------------------------|
| 24     | LDW      | src | imm | `eax <- memory[src + imm] (word)` |
| 25     | LDB      | src | imm | `eax <- memory[src + imm] (byte)` |
| 26     | STW      | src | imm | `memory[src + imm] <- eax (word)` |
| 27     | STB      | src | imm | `memory[src + imm] <- eax (byte)` |
| 28     | SLL      | dst | imm | `dst <- dst << imm`               |
| 29     | SRL      | dst | imm | `dst <- dst >> imm`               |
| 2A     | SRLU     | dst | imm | `dst <- unsigned(dst) >> imm`     |
| 2B     | ADD      | dst | imm | `dst <- dst + imm`                |
| 2C     | CMP      | lhs | imm | `cf <- compare(lhs, imm)`         |
| 2D     | CMPU     | lhs | imm | `cf <- unsigned comparison`       |
| 2E     | LEA      | dst | imm | `dst <- imm`                      |

## IMM (Immediate Format)

These instructions take a 32-bit immediate value as the only operand. These include most of the branching instructions. Note that the call instruction pushes a pointer to the next instruction before branching to the address. The system call instruction writes the return value from the system call to the eax register.

| Opcode | Mmemonic | op0     | op1 | Operation                                |
|--------|----------|---------|-----|------------------------------------------|
| 34     | JE       | address |     | `(cf = EQ): ip <- address`               |
| 35     | JNE      | address |     | `(cf <> EQ): ip <- address`              |
| 36     | JL       | address |     | `(cf = BELOW): ip <- address`            |
| 37     | JLE      | address |     | `(cf = BELOW || cf = EQ): ip <- address` |
| 38     | JG       | address |     | `(cf = ABOVE): ip <- address`            |
| 39     | JGE      | address |     | `(cf = ABOVE || cf = EQ): ip <- address` |
| 3A     | JMP      | address |     | `ip <- address`                          |
| 3B     | PUSH     | imm     |     | `esp <- esp - 4; memory[esp] <- imm`     |
| 3C     | CALL     | address |     | `push(ip + sizeof(IMM)); ip <- imm`      |
| 3D     | SYS      | imm     |     | `eax <- System.Execute(imm)`             |
