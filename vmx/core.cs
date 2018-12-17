namespace Ava
{
    struct LRX
    {
        internal const int SIZE = 3;
        internal byte dst;
        internal byte src;

        LRX(Script vm) {
            byte op0 = vm.memory[vm.cpu.ip + 1];
            byte op1 = vm.memory[vm.cpu.ip + 2];

            if (op0 > Registers.NUM_REGISTERS || op1 > Registers.NUM_REGISTERS) {
                vm.cpu.status.error = Cpu.Error.BAD_REGISTER;
                dst = 0;
                src = 0;
                return;
            }

            vm.cpu.ip += LRX.SIZE;
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
            float f0 = vm.IntToFloat(vm.cpu.registers[lrx.dst]);
            float f1 = vm.IntToFloat(vm.cpu.registers[lrx.src]);

            vm.cpu.registers[lrx.dst] = vm.FloatToInt(f0 * f1);
            return false;
        }

        internal static bool DIVF(Script vm) {
            var lrx = new LRX(vm);
            float f0 = vm.IntToFloat(vm.cpu.registers[lrx.dst]);
            float f1 = vm.IntToFloat(vm.cpu.registers[lrx.src]);

            vm.cpu.registers[lrx.dst] = vm.FloatToInt(f0 / f1);
            return false;
        }

        internal static bool ADDF(Script vm) {
            var lrx = new LRX(vm);
            float f0 = vm.IntToFloat(vm.cpu.registers[lrx.dst]);
            float f1 = vm.IntToFloat(vm.cpu.registers[lrx.src]);

            vm.cpu.registers[lrx.dst] = vm.FloatToInt(f0 + f1);
            return false;
        }

        internal static bool SUBF(Script vm) {
            var lrx = new LRX(vm);
            float f0 = vm.IntToFloat(vm.cpu.registers[lrx.dst]);
            float f1 = vm.IntToFloat(vm.cpu.registers[lrx.src]);

            vm.cpu.registers[lrx.dst] = vm.FloatToInt(f0 - f1);
            return false;
        }

        internal static bool CMPF(Script vm) {
            var lrx = new LRX(vm);
            float f0 = vm.IntToFloat(vm.cpu.registers[lrx.dst]);
            float f1 = vm.IntToFloat(vm.cpu.registers[lrx.src]);

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
            byte op = vm.memory[vm.cpu.ip + 1];

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
                vm.cpu.ip += SRX.SIZE;
            }

            dst = op;
        }

        internal static bool CVTFW(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            // Convert floating point to word
            float value = vm.IntToFloat(vm.cpu.registers[srx.dst]);
            vm.cpu.registers[srx.dst] = (uint)value;
            return false;
        }

        internal static bool CVTWF(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            // Convert word to floating point
            float value = vm.cpu.registers[srx.dst];

            // Store floating point representation in uint value
            uint repr = vm.FloatToInt(value);
            vm.cpu.registers[srx.dst] = repr;
            return false;
        }

        internal static bool HEAP(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            uint address = Heap.Alloc(vm, vm.cpu.registers[srx.dst]);
            vm.cpu.registers[Registers.EAX] = address;
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

            vm.cpu.ip = address; // Jump to address
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

        internal static bool FREE(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            return Heap.Free(vm, vm.cpu.registers[srx.dst]);
        }
    }

    struct IMRX
    {
        internal const int SIZE = 6;
        internal byte dst;
        internal int constant;

        IMRX(Script vm) {
            byte op0 = vm.memory[vm.cpu.ip + 1];
            int op1  = vm.ReadWord(vm.cpu.ip + 2);

            if (op0 > Registers.NUM_REGISTERS) {
                vm.cpu.status.error = Cpu.Error.BAD_REGISTER;
                dst = 0;
                constant = op1;
                return;
            }

            vm.cpu.ip += IMRX.SIZE;
            dst = op0;
            constant = op1;
        }

        internal static bool LWA(Script vm) {
            var imm = new IMRX(vm);
            uint address = (uint)(vm.cpu.registers[imm.dst] + imm.constant);
            uint value = (uint)vm.ReadWord(address);
            vm.cpu.registers[Registers.EAX] = value;
            return false;
        }

        internal static bool LBA(Script vm) {
            var imm = new IMRX(vm);
            int address = (int)vm.cpu.registers[imm.dst] + imm.constant;
            vm.cpu.registers[Registers.EAX] = vm.memory[address];
            return false;
        }

        internal static bool SWA(Script vm) {
            var imm = new IMRX(vm);
            uint value = vm.cpu.registers[Registers.EAX];
            uint address = (uint)(vm.cpu.registers[imm.dst] + imm.constant);
            vm.WriteWord(value, address);
            return false;
        }

        internal static bool SBA(Script vm) {
            var imm = new IMRX(vm);
            uint value = vm.cpu.registers[Registers.EAX];
            uint address = (uint)(vm.cpu.registers[imm.dst] + imm.constant);
            vm.WriteByte((byte)value, address);
            return false;
        }

        internal static bool SLL(Script vm) {
            var imm = new IMRX(vm);
            vm.cpu.registers[imm.dst] = vm.cpu.registers[imm.dst] << imm.constant;
            return false;
        }

        internal static bool SRL(Script vm) {
            var imm = new IMRX(vm);
            vm.cpu.registers[imm.dst] = (uint)((int)vm.cpu.registers[imm.dst] >> imm.constant);
            return false;
        }

        internal static bool SRLU(Script vm) {
            var imm = new IMRX(vm);
            vm.cpu.registers[imm.dst] = vm.cpu.registers[imm.dst] >> imm.constant;
            return false;
        }

        internal static bool ADD(Script vm) {
            var imm = new IMRX(vm);
            vm.cpu.registers[imm.dst] = (uint)(vm.cpu.registers[imm.dst] + imm.constant);
            return false;
        }

        internal static bool CMP(Script vm) {
            var imm = new IMRX(vm);
            int a = (int)vm.cpu.registers[imm.dst];
            int b = imm.constant;

            vm.cpu.status.CMP_BELOW = a < b;
            vm.cpu.status.CMP_EQUAL = a == b;
            vm.cpu.status.CMP_ABOVE = a > b;
            return false;
        }

        internal static bool CMPU(Script vm) {
            var imm = new IMRX(vm);
            uint a = vm.cpu.registers[imm.dst];
            uint b = (uint)imm.constant;

            vm.cpu.status.CMP_BELOW = a < b;
            vm.cpu.status.CMP_EQUAL = a == b;
            vm.cpu.status.CMP_ABOVE = a > b;
            return false;
        }

        internal static bool LEA(Script vm) {
            var imm = new IMRX(vm);
            vm.cpu.registers[imm.dst] = (uint)imm.constant;
            return false;
        }
    }

    struct IMM
    {
        internal const int SIZE = 5;
        internal uint address;

        IMM(Script vm, bool is_jump) {
            uint op = (uint)vm.ReadWord(vm.cpu.ip + 1);

            if (is_jump) {
                // Verify the address is executable
                if (op < Header.SIZE || op > vm.cpu.dat) {
                    vm.cpu.status.error = Cpu.Error.BAD_MEMORY_ACCESS;
                    address = Header.SIZE;
                    return;
                }
            } else {
                vm.cpu.ip += IMM.SIZE;
            }

            address = op;
        }

        internal static bool JE(Script vm) {
            var ja = new IMM(vm, is_jump: true);
            if (vm.cpu.status.CMP_EQUAL) {
                vm.cpu.ip = ja.address;
                return false;
            }
            vm.cpu.ip += IMM.SIZE;
            return false;
        }

        internal static bool JNE(Script vm) {
            var ja = new IMM(vm, is_jump: true);
            if (!vm.cpu.status.CMP_EQUAL) {
                vm.cpu.ip = ja.address;
                return false;
            }
            vm.cpu.ip += IMM.SIZE;
            return false;
        }

        internal static bool JL(Script vm) {
            var ja = new IMM(vm, is_jump: true);
            if (vm.cpu.status.CMP_BELOW) {
                vm.cpu.ip = ja.address;
                return false;
            }
            vm.cpu.ip += IMM.SIZE;
            return false;
        }

        internal static bool JLE(Script vm) {
            var ja = new IMM(vm, is_jump: true);
            if (vm.cpu.status.CMP_BELOW || vm.cpu.status.CMP_EQUAL) {
                vm.cpu.ip = ja.address;
                return false;
            }
            vm.cpu.ip += IMM.SIZE;
            return false;
        }

        internal static bool JG(Script vm) {
            var ja = new IMM(vm, is_jump: true);
            if (vm.cpu.status.CMP_ABOVE) {
                vm.cpu.ip = ja.address;
                return false;
            }
            vm.cpu.ip += IMM.SIZE;
            return false;
        }

        internal static bool JGE(Script vm) {
            var ja = new IMM(vm, is_jump: true);
            if (vm.cpu.status.CMP_ABOVE || vm.cpu.status.CMP_EQUAL) {
                vm.cpu.ip = ja.address;
                return false;
            }
            vm.cpu.ip += IMM.SIZE;
            return false;
        }

        internal static bool JMP(Script vm) {
            var ja = new IMM(vm, is_jump: true);
            vm.cpu.ip = ja.address;
            return false;
        }

        internal static bool CALL(Script vm) {
            var ja = new IMM(vm, is_jump: true);

            vm.cpu.registers[Registers.ESP] -= sizeof(int);
            uint stack_ptr = vm.cpu.registers[Registers.ESP];

            // Push pointer to next instrcution
            vm.WriteWord(vm.cpu.ip + IMM.SIZE, stack_ptr);
            vm.cpu.ip = ja.address; // Jump to address
            return false;
        }

        internal static bool PUSH(Script vm) {
            var ja = new IMM(vm, is_jump: false);
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
            var ja = new IMM(vm, is_jump: false);
            // Call extern procedure
            SysCallbacks.ExecuteCallback(vm, ja.address);
            return false;
        }
    }
}
