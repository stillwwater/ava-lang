## Registers

| Address | Register | Description         |
|---------|----------|---------------------|
| 0x0     | a0       | Argument registers  |
| 0x1     | a1       | ^                   |
| 0x2     | a2       | ^                   |
| 0x3     | a3       | ^                   |
| 0x4     | a4       | ^                   |
| 0x5     | a5       | ^                   |
| 0x6     | a6       | ^                   |
| 0x7     | a7       | ^                   |
| 0x8     | ra       | Return address      |
| 0x9     | rv       | Return value        |
| 0xA     | ax       | Accumulator         |
| 0xB     | ac       | Constant            |
| 0xC     | cx       | Counter             |
| 0xD     | t0       | Temporary registers |
| 0xE     | t1       | ^                   |
| 0xF     | t2       | ^                   |

## Instruction Format

| Format | Size (bits) | 0  | 1               | 2   | 3   | 4 | 5   |
|--------|-------------|----|-----------------|-----|-----|---|-----|
| SRX    | 16          | OP | R0              | n/a |     |   |     |
| LRX    | 24          | OP | R0              | R1  | n/a |   |     |
| J      | 40          | OP | ADDRESS         |     |     |   | n/a |
| IMM    | 48          | OP | IMMEDIATE VALUE |     |     |   |     |


## Arithmentic Instructions

| Format | Hex  | Mmemonic | op1 | op2 | Operation                                                         |
|--------|------|----------|-----|-----|-------------------------------------------------------------------|
| LRX    | 0x00 | SLL      | $d  | $s  | $d \leftarrow d << s$                                             |
| ^      | 0x01 | SRL      | $d  | $s  | $d \leftarrow d >> s$                                             |
| ^      | 0x02 | SRLU     | $d  | $s  | Unsigned shift right logical                                      |
| ^      | 0x03 | MUL      | $d  | $s  | $d \leftarrow d * s$                                              |
| ^      | 0x04 | DIV      | $d  | $s  | $d \leftarrow d / s$                                              |
| ^      | 0x05 | DIVU     | $d  | $s  | Unsigned division                                                 |
| ^      | 0x06 | MOD      | $d  | $s  | $d \leftarrow d $ mod $s$                                         |
| ^      | 0x07 | MODU     | $d  | $s  | $d \leftarrow d $ mod $s$                                         |
| ^      | 0x08 | ADD      | $d  | $s  | $d \leftarrow d + s$                                              |
| ^      | 0x09 | SUB      | $d  | $s  | $d \leftarrow d - s$                                              |
| LRX-F  | 0x16 | CVTFW    | $d  | $s  | $d \leftarrow word(s_f)$                                        |
| ^      | 0x17 | CVTWF    | $d  | $s  | $d_f \leftarrow float(s)$                                       |
| ^      | 0x18 | MULF     | $d  | $s  | $d_f \leftarrow d_f * s_f$                                        |
| ^      | 0x19 | DIVF     | $d  | $s  | $d_f \leftarrow d_f / s_f$                                        |
| ^      | 0x1A | ADDF     | $d  | $s  | $d_f \leftarrow d_f + s_f$                                        |
| ^      | 0x1B | SUBF     | $d  | $s  | $d_f \leftarrow d_f - s_f$                                        |
| IMM    | 0x28 | SLLI     | $d  | imm | $d \leftarrow d << imm$                                           |
| ^      | 0x29 | SRLI     | $d  | imm | $d \leftarrow d >> imm$                                           |
| ^      | 0x2A | SRLUI    | $d  | imm | unsigned shift right logical                                      |
| ^      | 0x2B | ADDI     | $d  | imm | $d \leftarrow d + imm$                                            |


## Logical & Comparison Instructions

| Format | Hex  | Mmemonic | op1 | op2 | Operation                                                       |
|--------|------|----------|-----|-----|-----------------------------------------------------------------|
| LRX    | 0x0B | CMP      | $s0 | $s1 | $cf \leftarrow (s_0 < s_1): -1, (s_0 > s_1): 1, (s_0 = s_1): 0$ |
| ^      | 0x0C | CMPU     | $s0 | $s1 | Unsigned comparison                                             |
| ^      | 0x0D | AND      | $d  | $s  | $d \leftarrow d$ & $s$                                          |
| ^      | 0x0E | OR       | $d  | $s  | $d \leftarrow d$ \| $s$                                         |
| ^      | 0x0F | XOR      | $d  | $s  | $d \leftarrow d$ ^ $ s$                                         |
| LRX-F  | 0x1C | CMPF     | $s0 | $s1 | Floating point comparison                                       |
| SRX    | 0x22 | NOT      | $d  |     | $d \leftarrow not(d)$                                           |
| IMM    | 0x23 | CMPI     | $d  | imm | compare immediate value                                         |
| ^      | 0x2D | CMPUI    | $d  | imm | compare immediate value                                         |



## Store / Load Instructions

| Format | Hex  | Mmemonic | op1 | op2 | Operation                         |
|--------|------|----------|-----|-----|-----------------------------------|
| LRX    | 0x0A | MOV      | $d  | $s  | $d \leftarrow s$                  |
| ^      | 0x10 | LW       | $d  | $s  | $d_W \leftarrow memory[s]$ (word) |
| ^      | 0x11 | LB       | $d  | $s  | $d_B \leftarrow memory[s]$ (byte) |
| ^      | 0x12 | SW       | $d  | $s  | $memory[s] \leftarrow d_W$ (word) |
| ^      | 0x13 | SB       | $d  | $s  | $memory[s] \leftarrow d_B$ (byte) |
| SRX    | 0x20 | PUSH     | $s  |     | Push value in $s$ to the stack    |
| ^      | 0x21 | POP      | $d  |     | Pop value from the stack into $s$ |
| IMM    | 0x2E | LEA      | $d  | imm | $d \leftarrow imm$                |


## Branch instructions

| Format | Hex  | Mmemonic | op1 | op2 | Operation                                         |
|--------|------|----------|-----|-----|---------------------------------------------------|
| J      | 0x35 | JMP      | imm |     | $pc \leftarrow memory[imm]$                       |
| ^      | 0x36 | JE       | imm |     | $(cf = 0): pc \leftarrow memory[imm]$             |
| ^      | 0x37 | JNE      | imm |     | $(cf \neq 0): pc \leftarrow memory[imm]$          |
| ^      | 0x38 | JL       | imm |     | $(cf < 0): pc \leftarrow memory[imm]$             |
| ^      | 0x39 | JG       | imm |     | $(cf > 0): pc \leftarrow memory[imm]$             |
| ^      | 0x3A | CALL     | imm |     | $ra \leftarrow pc, \ \ pc \leftarrow memory[imm]$ |
| ^      | 0x2B | SYS      | imm |     | system call                                       |
| SRX    | 0x23 | RET      | $s  |     | $pc \leftarrow s$ (jump to adress in $s$)         |

---

## Instruction Reference

| Format | Hex  | Mmemonic | op1 | op2 | Operation                                                       |
|--------|------|----------|-----|-----|-----------------------------------------------------------------|
| LRX    | 0x00 | SLL      | $d  | $s  | $d \leftarrow d << s$                                           |
| ^      | 0x01 | SRL      | $d  | $s  | $d \leftarrow d >> s$                                           |
| ^      | 0x02 | SRLU     | $d  | $s  | Unsigned shift right logical                                    |
| ^      | 0x03 | MUL      | $d  | $s  | $d \leftarrow d * s$                                            |
| ^      | 0x04 | DIV      | $d  | $s  | $d \leftarrow d / s$                                            |
| ^      | 0x05 | DIVU     | $d  | $s  | Unsigned division                                               |
| ^      | 0x06 | MOD      | $d  | $s  | $d \leftarrow d $ mod $s$                                       |
| ^      | 0x07 | MODU     | $d  | $s  | $d \leftarrow d $ mod $s$                                       |
| ^      | 0x08 | ADD      | $d  | $s  | $d \leftarrow d + s$                                            |
| ^      | 0x09 | SUB      | $d  | $s  | $d \leftarrow d - s$                                            |
| ^      | 0x0A | MOV      | $d  | $s  | $d \leftarrow s$                                                |
| ^      | 0x0B | CMP      | $s0 | $s1 | $cf \leftarrow (s_0 < s_1): -1, (s_0 > s_1): 1, (s_0 = s_1): 0$ |
| ^      | 0x0C | CMPU     | $s0 | $s1 | Unsigned comparison                                             |
| ^      | 0x0D | AND      | $d  | $s  | $d \leftarrow d$ & $s$                                          |
| ^      | 0x0E | OR       | $d  | $s  | $d \leftarrow d$ \| $s$                                         |
| ^      | 0x0F | XOR      | $d  | $s  | $d \leftarrow d$ ^ $ s$                                         |
| ^      | 0x10 | LW       | $d  | $s  | $d_W \leftarrow memory[s]$ (word)                               |
| ^      | 0x11 | LB       | $d  | $s  | $d_B \leftarrow memory[s]$ (byte)                               |
| ^      | 0x12 | SW       | $d  | $s  | $memory[s] \leftarrow d_W$ (word)                               |
| ^      | 0x13 | SB       | $d  | $s  | $memory[s] \leftarrow d_B$ (byte)                               |
| LRX-F  | 0x16 | CVTFW    | $d  | $s  | $d \leftarrow word(s_f)$                                        |
| ^      | 0x17 | CVTWF    | $d  | $s  | $d_f \leftarrow float(s)$                                       |
| ^      | 0x18 | MULF     | $d  | $s  | $d_f \leftarrow d_f * s_f$                                      |
| ^      | 0x19 | DIVF     | $d  | $s  | $d_f \leftarrow d_f / s_f$                                      |
| ^      | 0x1A | ADDF     | $d  | $s  | $d_f \leftarrow d_f + s_f$                                      |
| ^      | 0x1B | SUBF     | $d  | $s  | $d_f \leftarrow d_f - s_f$                                      |
| ^      | 0x1C | CMPF     | $s0 | $s1 | Floating point comparison                                       |
| SRX    | 0x20 | PUSH     | $s  |     | Push value in $s$ to the stack                                  |
| ^      | 0x21 | POP      | $d  |     | Pop value from the stack into $s$                               |
| ^      | 0x22 | HALT     | $s  |     | Stops execution and returns exit code in $s$                    |
| ^      | 0x23 | RET      | $s  |     | $pc \leftarrow s$ (jump to adress in $s$)                       |
| ^      | 0x24 | NOT      | $d  |     | $d \leftarrow not(d)$                                           |
| IMM    | 0x28 | SLLI     | $d  | imm | $d \leftarrow d << imm$                                         |
| ^      | 0x29 | SRLI     | $d  | imm | $d \leftarrow d >> imm$                                         |
| ^      | 0x2A | SRLUI    | $d  | imm | unsigned shift right logical                                    |
| ^      | 0x2B | ADDI     | $d  | imm | $d \leftarrow d + imm$                                          |
| ^      | 0x2C | CMPI     | $d  | imm | compare immediate value                                         |
| ^      | 0x2D | CMPUI    | $d  | imm | compare immediate value                                         |
| ^      | 0x2E | LEA      | $d  | imm | $d \leftarrow imm$                                              |
| J      | 0x35 | JMP      | imm |     | $pc \leftarrow memory[imm]$                                     |
| ^      | 0x36 | JE       | imm |     | $(cf = 0): pc \leftarrow memory[imm]$                           |
| ^      | 0x37 | JNE      | imm |     | $(cf \neq 0): pc \leftarrow memory[imm]$                        |
| ^      | 0x38 | JL       | imm |     | $(cf < 0): pc \leftarrow memory[imm]$                           |
| ^      | 0x39 | JG       | imm |     | $(cf > 0): pc \leftarrow memory[imm]$                           |
| ^      | 0x3A | CALL     | imm |     | $ra \leftarrow pc, \ \ pc \leftarrow memory[imm]$               |
| ^      | 0x3B | SYS      | imm |     | system call                                                     |
