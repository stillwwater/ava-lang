## Registers

| Address | Register | Description         |
|---------|----------|---------------------|
| 0x0     | cf       | Comparison result   |
| 0x1     | ax       | Accumulator         |
| 0x2     | cx       | Counter             |
| 0x3     | ra       | Return address      |
| 0x4     | v0       | Return value        |
| 0x5     | t0       | Temporary registers |
| 0x6     | t1       | ^                   |
| 0x7     | t2       | ^                   |
| 0x8     | a0       | Argument registers  |
| 0x9     | a1       | ^                   |
| 0xA     | a2       | ^                   |
| 0xB     | a3       | ^                   |
| 0xC     | a4       | ^                   |
| 0xD     | a5       | ^                   |
| 0xE     | a6       | ^                   |
| 0xF     | a7       | ^                   |

## Instructions

| Format | Size (bits) | 0  | 1               | 2   | 3   | 4 | 5   |
|--------|-------------|----|-----------------|-----|-----|---|-----|
| SRX    | 16          | OP | R0              | n/a |     |   |     |
| LRX    | 24          | OP | R0              | R1  | n/a |   |     |
| J      | 40          | OP | ADDRESS         |     |     |   | n/a |
| IMM    | 48          | OP | IMMEDIATE VALUE |     |     |   |     |

| Format | Hex  | Dec | Mmemonic | op1 | op2 | Operation                                                       |
|--------|------|-----|----------|-----|-----|-----------------------------------------------------------------|
| LRX    | 0x00 | 0   | SLL      | $d  | $s  | $d \leftarrow d << s$                                           |
| ^      | 0x01 | 1   | SRL      | $d  | $s  | $d \leftarrow d >> s$                                           |
| ^      | 0x02 | 2   | SRLU     | $d  | $s  | Unsigned shift right logical                                    |
| ^      | 0x03 | 3   | MUL      | $d  | $s  | $d \leftarrow d * s$                                            |
| ^      | 0x04 | 4   | DIV      | $d  | $s  | $d \leftarrow d / s$                                            |
| ^      | 0x05 | 5   | DIVU     | $d  | $s  | Unsigned division                                               |
| ^      | 0x06 | 6   | MOD      | $d  | $s  | $d \leftarrow d $ mod $s$                                       |
| ^      | 0x07 | 7   | MOV      | $d  | $s  | $d \leftarrow s$                                                |
| ^      | 0x08 | 8   | ADD      | $d  | $s  | $d \leftarrow d + s$                                            |
| ^      | 0x09 | 9   | SUB      | $d  | $s  | $d \leftarrow d - s$                                            |
| ^      | 0x0A | 10  | CMP      | $s0 | $s1 | $cf \leftarrow (s_0 < s_1): -1, (s_0 > s_1): 1, (s_0 = s_1): 0$ |
| ^      | 0x0B | 11  | CMPU     | $s0 | $s1 | Unsigned comparison                                             |
| ^      | 0x0C | 12  | AND      | $d  | $s  | $d \leftarrow d$ & $s$                                          |
| ^      | 0x0D | 13  | OR       | $d  | $s  | $d \leftarrow d$ \| $s$                                         |
| ^      | 0x0E | 14  | XOR      | $d  | $s  | $d \leftarrow d$ ^ $ s$                                         |
| ^      | 0x0F | 15  | LW       | $d  | $s  | $d_W \leftarrow memory[s]$ (word)                               |
| ^      | 0x10 | 16  | LB       | $d  | $s  | $d_B \leftarrow memory[s]$ (byte)                               |
| ^      | 0x11 | 17  | SW       | $d  | $s  | $memory[s] \leftarrow d_W$ (word)                               |
| ^      | 0x12 | 18  | SB       | $d  | $s  | $memory[s] \leftarrow d_B$ (byte)                               |
| LRX-F  | 0x13 | 19  | ADDF     | $d  | $s  | $d_f \leftarrow d_f + s_f$                                      |
| ^      | 0x14 | 20  | SUBF     | $d  | $s  | $d_f \leftarrow d_f - s_f$                                      |
| ^      | 0x15 | 21  | CMPF     | $s0 | $s1 | Floating point comparison                                       |
| ^      | 0x16 | 22  | CVTF     | $d  | $s  | $d \leftarrow int(s_f)$                                         |
| ^      | 0x17 | 23  | MULF     | $d  | $s  | $d_f \leftarrow d_f * s_f$                                      |
| ^      | 0x18 | 24  | DIVF     | $d  | $s  | $d_f \leftarrow d_f / s_f$                                      |
| SRX    | 0x20 | 32  | RET      | $s  |     | $pc \leftarrow s$ (jump to adress in $s$)                       |
| ^      | 0x21 | 33  | SYS      | $s  |     | system call                                                     |
| ^      | 0x22 | 34  | NOT      | $d  |     | $d \leftarrow not(d)$                                           |
| ^      | 0x23 | 35  | PUSH     | $s  |     | Push value in $s$ to the stack                                  |
| ^      | 0x24 | 36  | POP      | $d  |     | Pop value from the stack into $s$                               |
| IMM    | 0x40 | 64  | SLLI     | $d  | imm | $d \leftarrow d << imm$                                         |
| ^      | 0x41 | 65  | SRLI     | $d  | imm | $d \leftarrow d >> imm$                                         |
| ^      | 0x42 | 66  | SRLUI    | $d  | imm | unsigned shift right logical                                    |
| ^      | 0x43 | 67  | ADDI     | $d  | imm | $d \leftarrow d + imm$                                          |
| ^      | 0x44 | 68  | CMPI     | $d  | imm | compare immediate value                                         |
| ^      | 0x45 | 69  | LI       | $d  | imm | $d \leftarrow imm$                                              |
| ^      | 0x46 | 70  | LA       | $d  | imm | $d_W \leftarrow memory[imm]$                                    |
| J      | 0x80 | 128 | JMP      | imm |     | $pc \leftarrow memory[imm]$                                     |
| ^      | 0x81 | 129 | JZ       | imm |     | $(cf = 0): pc \leftarrow memory[imm]$                           |
| ^      | 0x82 | 130 | JNZ      | imm |     | $(cf \neq 0): pc \leftarrow memory[imm]$                        |
| ^      | 0x83 | 131 | JG       | imm |     | $(cf > 0): pc \leftarrow memory[imm]$                           |
| ^      | 0x84 | 132 | JL       | imm |     | $(cf < 0): pc \leftarrow memory[imm]$                           |
| ^      | 0x85 | 133 | CALL     | imm |     | $ra \leftarrow pc, \ \ pc \leftarrow memory[imm]$               |
