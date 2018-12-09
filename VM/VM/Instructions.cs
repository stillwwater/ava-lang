using System;

namespace VM
{
    public static class Instructions
    {
        struct LRX
        {
            internal const int SIZE = 3;
            internal byte dst;
            internal byte src;

            LRX(Script vm) {
                byte op0 = vm.memory[vm.cpu.inp + 1];
                byte op1 = vm.memory[vm.cpu.inp + 2];

                if (op0 > Registers.NUM_REGISTERS || op1 > Registers.NUM_REGISTERS) {
                    vm.cpu.status.error = Cpu.Error.BAD_REGISTER;
                    dst = 0;
                    src = 0;
                    return;
                }

                vm.cpu.inp += LRX.SIZE;
                dst = op0;
                src = op1;
            }

            internal static bool SLL(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] = vm.cpu.registers[lrx.dst] << (int)vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool SRL(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] = (uint)((int)vm.cpu.registers[lrx.dst] >> (int)vm.cpu.registers[lrx.src]);
                return false;
            }

            internal static bool SRLU(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] = vm.cpu.registers[lrx.dst] >> (int)vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool MUL(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] *= vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool DIV(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] = (uint)((int)vm.cpu.registers[lrx.dst] / (int)vm.cpu.registers[lrx.src]);
                return false;
            }

            internal static bool DIVU(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] /= vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool MOD(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] = (uint)((int)vm.cpu.registers[lrx.dst] % (int)vm.cpu.registers[lrx.src]);
                return false;
            }

            internal static bool MODU(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] %= vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool ADD(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] += vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool SUB(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] -= vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool MOV(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] = vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool CMP(Script vm) {
                var lrx = new LRX(vm);
                int a = (int)vm.cpu.registers[lrx.dst];
                int b = (int)vm.cpu.registers[lrx.src];

                vm.cpu.status.CMP_BELOW = a < b;
                vm.cpu.status.CMP_EQUAL = a == b;
                vm.cpu.status.CMP_ABOVE = a > b;
                return false;
            }

            internal static bool CMPU(Script vm) {
                var lrx = new LRX(vm);
                uint a = vm.cpu.registers[lrx.dst];
                uint b = vm.cpu.registers[lrx.src];

                vm.cpu.status.CMP_BELOW = a < b;
                vm.cpu.status.CMP_EQUAL = a == b;
                vm.cpu.status.CMP_ABOVE = a > b;
                return false;
            }

            internal static bool AND(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] &= vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool OR(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] |= vm.cpu.registers[lrx.src];
                return false;
            }

            internal static bool XOR(Script vm) {
                var lrx = new LRX(vm);
                vm.cpu.registers[lrx.dst] ^= vm.cpu.registers[lrx.src];
                return false;
            }

            // Floating point

            internal static bool MULF(Script vm) {
                var lrx = new LRX(vm);
                float f0 = IntToFloat(vm.cpu.registers[lrx.dst]);
                float f1 = IntToFloat(vm.cpu.registers[lrx.src]);

                vm.cpu.registers[lrx.dst] = FloatToInt(f0 * f1);
                return false;
            }

            internal static bool DIVF(Script vm) {
                var lrx = new LRX(vm);
                float f0 = IntToFloat(vm.cpu.registers[lrx.dst]);
                float f1 = IntToFloat(vm.cpu.registers[lrx.src]);

                vm.cpu.registers[lrx.dst] = FloatToInt(f0 / f1);
                return false;
            }

            internal static bool ADDF(Script vm) {
                var lrx = new LRX(vm);
                float f0 = IntToFloat(vm.cpu.registers[lrx.dst]);
                float f1 = IntToFloat(vm.cpu.registers[lrx.src]);

                vm.cpu.registers[lrx.dst] = FloatToInt(f0 + f1);
                return false;
            }

            internal static bool SUBF(Script vm) {
                var lrx = new LRX(vm);
                float f0 = IntToFloat(vm.cpu.registers[lrx.dst]);
                float f1 = IntToFloat(vm.cpu.registers[lrx.src]);

                vm.cpu.registers[lrx.dst] = FloatToInt(f0 - f1);
                return false;
            }

            internal static bool CMPF(Script vm) {
                var lrx = new LRX(vm);
                float f0 = IntToFloat(vm.cpu.registers[lrx.dst]);
                float f1 = IntToFloat(vm.cpu.registers[lrx.src]);

                vm.cpu.status.CMP_BELOW = f0 < f1;
                vm.cpu.status.CMP_EQUAL = f0 == f1;
                vm.cpu.status.CMP_ABOVE = f0 > f1;
                return false;
            }
        }

        struct SRX
        {
            internal const int SIZE = 2;
            internal byte dst;

            SRX(Script vm, bool is_jump) {
                byte op = vm.memory[vm.cpu.inp + 1];

                if (op > Registers.NUM_REGISTERS) {
                    vm.cpu.status.error = Cpu.Error.BAD_REGISTER;
                    dst = 0;
                    return;
                }

                if (is_jump) {
                    // Verify the address is executable
                    uint address = vm.cpu.registers[op];
                    if (address < Header.SIZE || address > vm.cpu.dat) {
                        vm.cpu.status.error = Cpu.Error.BAD_MEMORY_ACCESS;
                        address = Header.SIZE;
                        dst = op;
                        return;
                    }
                } else {
                    vm.cpu.inp += SRX.SIZE;
                }

                dst = op;
            }

            internal static bool CVTFW(Script vm) {
                var srx = new SRX(vm, is_jump: false);
                // Convert floating point to word
                float value = IntToFloat(vm.cpu.registers[srx.dst]);
                vm.cpu.registers[srx.dst] = (uint)value;
                return false;
            }

            internal static bool CVTWF(Script vm) {
                var srx = new SRX(vm, is_jump: false);
                // Convert word to floating point
                float value = vm.cpu.registers[srx.dst];

                // Store floating point representation in uint value
                uint repr = FloatToInt(value);
                vm.cpu.registers[srx.dst] = repr;
                return false;
            }

            internal static bool PUSH(Script vm) {
                var srx = new SRX(vm, is_jump: false);
                // Stack grows upwards
                vm.cpu.registers[Registers.ESP] -= 4;
                uint stack_ptr = vm.cpu.registers[Registers.ESP];

                // if (stack_ptr == Script.MAX_MEMORY) {
                //     vm.cpu.status.STACK_OVERFLOW = true;
                //     return;
                // }

                uint word = vm.cpu.registers[srx.dst];

                vm.WriteWord(word, stack_ptr);
                return false;
            }

            internal static bool POP(Script vm) {
                var srx = new SRX(vm, is_jump: false);
                uint stack_ptr = vm.cpu.registers[Registers.ESP];

                if (stack_ptr > Script.MAX_MEMORY) {
                    vm.cpu.status.error = Cpu.Error.STACK_UNDERFLOW;
                    return true;
                }

                uint word = (uint)vm.ReadWord(stack_ptr);
                vm.cpu.registers[Registers.ESP] += 4;
                vm.cpu.registers[srx.dst] = word;
                return false;
            }

            internal static bool RET(Script vm) {
                var srx = new SRX(vm, is_jump: true);
                uint address = vm.cpu.registers[srx.dst];

                vm.cpu.inp = address; // Jump to address
                return false;
            }

            internal static bool HALT(Script vm) {
                var srx = new SRX(vm, is_jump: false);
                uint status_code = vm.cpu.registers[srx.dst];

                vm.Halt((int)status_code);
                return false;
            }

            internal static bool NOT(Script vm) {
                var srx = new SRX(vm, is_jump: false);
                uint word = vm.cpu.registers[srx.dst];
                vm.cpu.registers[srx.dst] = ~word;
                return false;
            }
        }

        struct IMM
        {
            internal const int SIZE = 6;
            internal byte dst;
            internal int constant;

            IMM(Script vm) {
                byte op0 = vm.memory[vm.cpu.inp + 1];
                int op1  = vm.ReadWord(vm.cpu.inp + 2);

                if (op0 > Registers.NUM_REGISTERS) {
                    vm.cpu.status.error = Cpu.Error.BAD_REGISTER;
                    dst = 0;
                    constant = op1;
                    return;
                }

                vm.cpu.inp += IMM.SIZE;
                dst = op0;
                constant = op1;
            }

            internal static bool LWA(Script vm) {
                var imm = new IMM(vm);
                uint address = (uint)(vm.cpu.registers[imm.dst] + imm.constant);
                uint value = (uint)vm.ReadWord(address);
                vm.cpu.registers[Registers.EAX] = value;
                return false;
            }

            internal static bool LBA(Script vm) {
                var imm = new IMM(vm);
                int address = (int)vm.cpu.registers[imm.dst] + imm.constant;
                vm.cpu.registers[Registers.EAX] = vm.memory[address];
                return false;
            }

            internal static bool SWA(Script vm) {
                var imm = new IMM(vm);
                uint value = vm.cpu.registers[Registers.EAX];
                uint address = (uint)(vm.cpu.registers[imm.dst] + imm.constant);
                vm.WriteWord(value, address);
                return false;
            }

            internal static bool SBA(Script vm) {
                var imm = new IMM(vm);
                uint value = vm.cpu.registers[Registers.EAX];
                uint address = (uint)(vm.cpu.registers[imm.dst] + imm.constant);
                vm.WriteByte((byte)value, address);
                return false;
            }

            internal static bool SLL(Script vm) {
                var imm = new IMM(vm);
                vm.cpu.registers[imm.dst] = vm.cpu.registers[imm.dst] << imm.constant;
                return false;
            }

            internal static bool SRL(Script vm) {
                var imm = new IMM(vm);
                vm.cpu.registers[imm.dst] = (uint)((int)vm.cpu.registers[imm.dst] >> imm.constant);
                return false;
            }

            internal static bool SRLU(Script vm) {
                var imm = new IMM(vm);
                vm.cpu.registers[imm.dst] = vm.cpu.registers[imm.dst] >> imm.constant;
                return false;
            }

            internal static bool ADD(Script vm) {
                var imm = new IMM(vm);
                vm.cpu.registers[imm.dst] = (uint)(vm.cpu.registers[imm.dst] + imm.constant);
                return false;
            }

            internal static bool CMP(Script vm) {
                var imm = new IMM(vm);
                int a = (int)vm.cpu.registers[imm.dst];
                int b = imm.constant;

                vm.cpu.status.CMP_BELOW = a < b;
                vm.cpu.status.CMP_EQUAL = a == b;
                vm.cpu.status.CMP_ABOVE = a > b;
                return false;
            }

            internal static bool CMPU(Script vm) {
                var imm = new IMM(vm);
                uint a = vm.cpu.registers[imm.dst];
                uint b = (uint)imm.constant;

                vm.cpu.status.CMP_BELOW = a < b;
                vm.cpu.status.CMP_EQUAL = a == b;
                vm.cpu.status.CMP_ABOVE = a > b;
                return false;
            }

            internal static bool LEA(Script vm) {
                var imm = new IMM(vm);
                vm.cpu.registers[imm.dst] = (uint)imm.constant;
                return false;
            }
        }

        struct JA
        {
            internal const int SIZE = 5;
            internal uint address;

            JA(Script vm, bool is_jump) {
                uint op = (uint)vm.ReadWord(vm.cpu.inp + 1);

                if (is_jump) {
                    // Verify the address is executable
                    if (op < Header.SIZE || op > vm.cpu.dat) {
                        vm.cpu.status.error = Cpu.Error.BAD_MEMORY_ACCESS;
                        address = Header.SIZE;
                        return;
                    }
                } else {
                    vm.cpu.inp += JA.SIZE;
                }

                address = op;
            }

            internal static bool JE(Script vm) {
                var ja = new JA(vm, is_jump: true);
                if (vm.cpu.status.CMP_EQUAL) {
                    vm.cpu.inp = ja.address;
                    return false;
                }
                vm.cpu.inp += JA.SIZE;
                return false;
            }

            internal static bool JNE(Script vm) {
                var ja = new JA(vm, is_jump: true);
                if (!vm.cpu.status.CMP_EQUAL) {
                    vm.cpu.inp = ja.address;
                    return false;
                }
                vm.cpu.inp += JA.SIZE;
                return false;
            }

            internal static bool JL(Script vm) {
                var ja = new JA(vm, is_jump: true);
                if (vm.cpu.status.CMP_BELOW) {
                    vm.cpu.inp = ja.address;
                    return false;
                }
                vm.cpu.inp += JA.SIZE;
                return false;
            }

            internal static bool JLE(Script vm) {
                var ja = new JA(vm, is_jump: true);
                if (vm.cpu.status.CMP_BELOW || vm.cpu.status.CMP_EQUAL) {
                    vm.cpu.inp = ja.address;
                    return false;
                }
                vm.cpu.inp += JA.SIZE;
                return false;
            }

            internal static bool JG(Script vm) {
                var ja = new JA(vm, is_jump: true);
                if (vm.cpu.status.CMP_ABOVE) {
                    vm.cpu.inp = ja.address;
                    return false;
                }
                vm.cpu.inp += JA.SIZE;
                return false;
            }

            internal static bool JGE(Script vm) {
                var ja = new JA(vm, is_jump: true);
                if (vm.cpu.status.CMP_ABOVE || vm.cpu.status.CMP_EQUAL) {
                    vm.cpu.inp = ja.address;
                    return false;
                }
                vm.cpu.inp += JA.SIZE;
                return false;
            }

            internal static bool JMP(Script vm) {
                var ja = new JA(vm, is_jump: true);
                vm.cpu.inp = ja.address;
                return false;
            }

            internal static bool CALL(Script vm) {
                var ja = new JA(vm, is_jump: true);

                vm.cpu.registers[Registers.ESP] -= sizeof(int);
                uint stack_ptr = vm.cpu.registers[Registers.ESP];

                // Push pointer to next instrcution
                vm.WriteWord(vm.cpu.inp + JA.SIZE, stack_ptr);
                vm.cpu.inp = ja.address; // Jump to address
                return false;
            }

            internal static bool PUSH(Script vm) {
                var ja = new JA(vm, is_jump: false);
                // Stack grows down
                vm.cpu.registers[Registers.ESP] -= sizeof(int);
                uint stack_ptr = vm.cpu.registers[Registers.ESP];

                // if (stack_ptr == vm.cpu.stk) {
                //     vm.cpu.status.STACK_OVERFLOW = true;
                //     return;
                // }

                vm.WriteWord(ja.address, stack_ptr);
                return false;
            }

            internal static bool SYS(Script vm) {
                var ja = new JA(vm, is_jump: false);
                // Call extern procedure
                SysCallbacks.ExecuteCallback(vm, ja.address);
                return false;
            }
        }

        static Func<Script, bool>[] instruction_table = new Func<Script, bool>[64] {
            /* 0x00 */    LRX.SLL,
            /* 0x01 */    LRX.SRL,
            /* 0x02 */    LRX.SRLU,
            /* 0x03 */    LRX.MUL,
            /* 0x04 */    LRX.DIV,
            /* 0x05 */    LRX.DIVU,
            /* 0x06 */    LRX.MOD,
            /* 0x07 */    LRX.MODU,
            /* 0x08 */    LRX.ADD,
            /* 0x09 */    LRX.SUB,
            /* 0x0A */    LRX.MOV,
            /* 0x0B */    LRX.CMP,
            /* 0x0C */    LRX.CMPU,
            /* 0x0D */    LRX.AND,
            /* 0x0E */    LRX.OR,
            /* 0x0F */    LRX.XOR,
            /* LRX-F */
            /* 0x10 */    null,
            /* 0x11 */    null,
            /* 0x12 */    LRX.MULF,
            /* 0x13 */    LRX.DIVF,
            /* 0x14 */    LRX.ADDF,
            /* 0x15 */    LRX.SUBF,
            /* 0x16 */    LRX.CMPF,

            /* SRX-F */
            /* 0x17 */    SRX.CVTFW,
            /* 0x18 */    SRX.CVTWF,
            /* SRX  */
            /* 0x19 */    null,
            /* 0x1A */    SRX.PUSH,
            /* 0x1B */    SRX.POP,
            /* 0x1C */    SRX.NOT,
            /* 0x1D */    SRX.RET,
            /* 0x1E */    SRX.HALT,

            /* 0x1F */    null,
            /* 0x20 */    null,
            /* 0x21 */    null,
            /* 0x22 */    null,
            /* 0x23 */    null,

            /* 0x24 */    IMM.LWA,
            /* 0x25 */    IMM.LBA,
            /* 0x26 */    IMM.SWA,
            /* 0x27 */    IMM.SBA,
            /* 0x28 */    IMM.SLL,
            /* 0x29 */    IMM.SRL,
            /* 0x2A */    IMM.SRLU,
            /* 0x2B */    IMM.ADD,
            /* 0x2C */    IMM.CMP,
            /* 0x2D */    IMM.CMPU,
            /* 0x2E */    IMM.LEA,

            /* 0x2F */    null,
            /* 0x30 */    null,
            /* 0x31 */    null,
            /* 0x32 */    null,
            /* 0x33 */    null,

            /* 0x34 */    JA.JE,
            /* 0x35 */    JA.JNE,
            /* 0x36 */    JA.JL,
            /* 0x37 */    JA.JLE,
            /* 0x38 */    JA.JG,
            /* 0x39 */    JA.JGE,
            /* 0x3A */    JA.JMP,
            /* 0x3B */    JA.PUSH,
            /* 0x3C */    JA.CALL,
            /* 0x3D */    JA.SYS,

            /* 0x3E */    null,
            /* 0x3F */    null,
        };

        public static bool Execute(byte opcode, Script vm) {
            if (opcode > instruction_table.Length) {
                // Unknown instruction
                vm.cpu.status.error = Cpu.Error.BAD_INSTRUCTION;
                return true;
            }

            var instruction = instruction_table[opcode];

            if (instruction == null) {
                // Unavalable instruction
                vm.cpu.status.error = Cpu.Error.BAD_INSTRUCTION;
                return true;
            }

            return instruction(vm);
        }

        public static uint FloatToInt(float value) {
            return BitConverter.ToUInt32(BitConverter.GetBytes(value), 0);
        }

        public static float IntToFloat(uint value) {
            return BitConverter.ToSingle(BitConverter.GetBytes(value), 0);
        }

        internal static string RegisterName(byte register) {
            switch (register) {
                case 0: return "EAX";
                case 1: return "ECX";
                case 2: return "EDX";
                case 3: return "ESP";
                case 4: return "EBP";
            }

            return null;
        }

        internal static string Disassemble(Script vm, int ip, out int size) {
            byte opcode = vm.memory[ip];
            string name = instruction_table[opcode].Method.Name;

            if (opcode < 0x17) {
                string dst_s = RegisterName(vm.memory[ip + 1]);
                string src_s = RegisterName(vm.memory[ip + 2]);
                size = LRX.SIZE;
                return string.Format("{3:X8}  {4:X2}  {0, -8}{1}, {2}", name, dst_s, src_s, ip, opcode);
            }

            if (opcode < 0x24) {
                string dst = RegisterName(vm.memory[ip + 1]);
                size = SRX.SIZE;
                return string.Format("{2:X8}  {3:X2}  {0, -8}{1}", name, dst, ip, opcode);
            }

            if (opcode < 0x34) {
                string dst = RegisterName(vm.memory[ip + 1]);
                int imm = vm.ReadWord((uint)ip + 2);
                size = IMM.SIZE;
                return string.Format("{3:X8}  {4:X2}  {0, -8}{1}, {2:X}", name, dst, imm, ip, opcode);
            }

            if (opcode < 0x40) {
                int imm = vm.ReadWord((uint)ip + 1);
                size = JA.SIZE;
                return string.Format("{2:X8}  {3:X2}  {0, -8}{1:X}", name, imm, ip, opcode);
            }

            size = 0;
            return "";
        }
    }
}
