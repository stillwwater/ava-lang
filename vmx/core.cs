namespace Ava
{
    struct LRX
    {
        internal const int SIZE = 1;
        internal byte dst;
        internal byte src;

        LRX(Script vm) {
            // @Performance: This address is already read by the vm
            // perhaps it would be more efficient to pass the read
            // value to the instruction.
            int data = vm.memory[vm.cpu.eip];
            byte op0 = (byte)((data & 0x00_FF_00_00) >> 16);
            byte op1 = (byte)((data & 0xFF_00_00_00) >> 24);

            if (op0 > Registers.NUM_REGISTERS || op1 > Registers.NUM_REGISTERS) {
                vm.cpu.status = Cpu.Status.BAD_REGISTER;
                dst = 0;
                src = 0;
                return;
            }

            vm.cpu.eip += LRX.SIZE;
            dst = op0;
            src = op1;
        }

        internal static Cpu.Status SLL(Script vm) {
            var lrx = new LRX(vm);
            int dst = vm.cpu.registers[lrx.dst];
            int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] = dst << src;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status SRL(Script vm) {
            var lrx = new LRX(vm);
            int dst = vm.cpu.registers[lrx.dst];
            int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] = dst >> src;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status SRLU(Script vm) {
            var lrx  = new LRX(vm);
            uint dst = (uint)vm.cpu.registers[lrx.dst];
            int src  = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] = (int)(dst >> src);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status MUL(Script vm) {
            var lrx = new LRX(vm);
			int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] *= src;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status DIV(Script vm) {
            var lrx = new LRX(vm);
			int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] /= src;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status DIVU(Script vm) {
            var lrx = new LRX(vm);
			uint dst = (uint)vm.cpu.registers[lrx.dst];
			uint src = (uint)vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] = (int)(dst / src);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status MOD(Script vm) {
            var lrx = new LRX(vm);
			int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] %= src;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status MODU(Script vm) {
            var lrx = new LRX(vm);
			uint dst = (uint)vm.cpu.registers[lrx.dst];
			uint src = (uint)vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] = (int)(dst % src);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status ADD(Script vm) {
            var lrx = new LRX(vm);
			int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] += src;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status SUB(Script vm) {
            var lrx = new LRX(vm);
			int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] -= src;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status MOV(Script vm) {
            var lrx = new LRX(vm);
			int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] = src;

            return Cpu.Status.OK;
        }

        internal static Cpu.Status AND(Script vm) {
            var lrx = new LRX(vm);
			int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] &= src;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status OR(Script vm) {
            var lrx = new LRX(vm);
			int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] |= src;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status XOR(Script vm) {
            var lrx = new LRX(vm);
			int src = vm.cpu.registers[lrx.src];
            vm.cpu.registers[lrx.dst] ^= src;
            return Cpu.Status.OK;
        }

        ///
        /// Signed compare instructions
        ///

        internal static Cpu.Status CEQ(Script vm) {
            var lrx = new LRX(vm);
			int dst = vm.cpu.registers[lrx.dst];
			int src = vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst == src ? 1 : 0;
            return Cpu.Status.OK;
        }

		internal static Cpu.Status CNE(Script vm) {
			var lrx = new LRX(vm);
			int dst = vm.cpu.registers[lrx.dst];
			int src = vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst != src ? 1 : 0;
            return Cpu.Status.OK;
		}

		internal static Cpu.Status CLT(Script vm) {
			var lrx = new LRX(vm);
			int dst = vm.cpu.registers[lrx.dst];
			int src = vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst < src ? 1 : 0;
            return Cpu.Status.OK;
		}

		internal static Cpu.Status CLE(Script vm) {
			var lrx = new LRX(vm);
			int dst = vm.cpu.registers[lrx.dst];
			int src = vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst <= src ? 1 : 0;
            return Cpu.Status.OK;
		}

		internal static Cpu.Status CGT(Script vm) {
			var lrx = new LRX(vm);
			int dst = vm.cpu.registers[lrx.dst];
			int src = vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst > src ? 1 : 0;
            return Cpu.Status.OK;
		}

		internal static Cpu.Status CGE(Script vm) {
			var lrx = new LRX(vm);
			int dst = vm.cpu.registers[lrx.dst];
			int src = vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst >= src ? 1 : 0;
            return Cpu.Status.OK;
		}

		///
		/// Unsigned compare instructions
		///

		internal static Cpu.Status CLTU(Script vm) {
			var lrx = new LRX(vm);
			uint dst = (uint)vm.cpu.registers[lrx.dst];
			uint src = (uint)vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst < src ? 1 : 0;
			return Cpu.Status.OK;
		}

		internal static Cpu.Status CLEU(Script vm) {
			var lrx = new LRX(vm);
			uint dst = (uint)vm.cpu.registers[lrx.dst];
			uint src = (uint)vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst <= src ? 1 : 0;
			return Cpu.Status.OK;
		}

		internal static Cpu.Status CGTU(Script vm) {
			var lrx = new LRX(vm);
			uint dst = (uint)vm.cpu.registers[lrx.dst];
			uint src = (uint)vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst > src ? 1 : 0;
			return Cpu.Status.OK;
		}

		internal static Cpu.Status CGEU(Script vm) {
			var lrx = new LRX(vm);
			uint dst = (uint)vm.cpu.registers[lrx.dst];
			uint src = (uint)vm.cpu.registers[lrx.src];
			vm.cpu.registers[Registers.EAX] = dst >= src ? 1 : 0;
			return Cpu.Status.OK;
		}

		///
		/// Floating point compare
        /// [0x18..0x1B]
		///

		internal static Cpu.Status CLTF(Script vm) {
			var lrx = new LRX(vm);
			float f0 = Float32.FromInt(vm.cpu.registers[lrx.dst]);
			float f1 = Float32.FromInt(vm.cpu.registers[lrx.src]);
			vm.cpu.registers[Registers.EAX] = f0 < f1 ? 1 : 0;
			return Cpu.Status.OK;
		}

		internal static Cpu.Status CLEF(Script vm) {
			var lrx = new LRX(vm);
			float f0 = Float32.FromInt(vm.cpu.registers[lrx.dst]);
			float f1 = Float32.FromInt(vm.cpu.registers[lrx.src]);
			vm.cpu.registers[Registers.EAX] = f0 <= f1 ? 1 : 0;
			return Cpu.Status.OK;
		}

		internal static Cpu.Status CGTF(Script vm) {
			var lrx = new LRX(vm);
			float f0 = Float32.FromInt(vm.cpu.registers[lrx.dst]);
			float f1 = Float32.FromInt(vm.cpu.registers[lrx.src]);
			vm.cpu.registers[Registers.EAX] = f0 > f1 ? 1 : 0;
			return Cpu.Status.OK;
		}

		internal static Cpu.Status CGEF(Script vm) {
			var lrx = new LRX(vm);
			float f0 = Float32.FromInt(vm.cpu.registers[lrx.dst]);
			float f1 = Float32.FromInt(vm.cpu.registers[lrx.src]);
			vm.cpu.registers[Registers.EAX] = f0 >= f1 ? 1 : 0;
			return Cpu.Status.OK;
		}

        ///
        /// Floating point arithmetic
        /// [0x1C..0x1F]
        ///

        internal static Cpu.Status MULF(Script vm) {
            var lrx = new LRX(vm);
            float f0 = Float32.FromInt(vm.cpu.registers[lrx.dst]);
            float f1 = Float32.FromInt(vm.cpu.registers[lrx.src]);
            vm.cpu.registers[lrx.dst] = Float32.ToInt(f0 * f1);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status DIVF(Script vm) {
            var lrx = new LRX(vm);
            float f0 = Float32.FromInt(vm.cpu.registers[lrx.dst]);
            float f1 = Float32.FromInt(vm.cpu.registers[lrx.src]);
            vm.cpu.registers[lrx.dst] = Float32.ToInt(f0 / f1);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status ADDF(Script vm) {
            var lrx = new LRX(vm);
            float f0 = Float32.FromInt(vm.cpu.registers[lrx.dst]);
            float f1 = Float32.FromInt(vm.cpu.registers[lrx.src]);
            vm.cpu.registers[lrx.dst] = Float32.ToInt(f0 + f1);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status SUBF(Script vm) {
            var lrx = new LRX(vm);
            float f0 = Float32.FromInt(vm.cpu.registers[lrx.dst]);
            float f1 = Float32.FromInt(vm.cpu.registers[lrx.src]);
            vm.cpu.registers[lrx.dst] = Float32.ToInt(f0 - f1);
            return Cpu.Status.OK;
        }
    }

    struct SRX
    {
        internal const int SIZE = 1;
        internal byte dst;

        SRX(Script vm, bool is_jump) {
            int data = vm.memory[vm.cpu.eip];
            byte op = (byte)((data & 0x00_FF_00_00) >> 16);

            if (op > Registers.NUM_REGISTERS) {
                vm.cpu.status = Cpu.Status.BAD_REGISTER;
                dst = 0;
                return;
            }

            if (is_jump) {
                // Verify the address is executable
                int address = vm.cpu.registers[op];
                if (address < 0 || address > vm.cpu.dat) {
                    vm.cpu.status = Cpu.Status.BAD_MEMORY_ACCESS;
                    address = 0;
                    dst = op;
                    return;
                }
            } else {
                vm.cpu.eip += SRX.SIZE;
            }

            dst = op;
        }

        internal static Cpu.Status CVTFW(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            float value = Float32.FromInt(vm.cpu.registers[srx.dst]);
            // Floor float value to convert to int
            vm.cpu.registers[srx.dst] = (int)value;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status CVTWF(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            // Convert word to floating point
            float value = (float)vm.cpu.registers[srx.dst];

            // Store floating point representation in int value
            vm.cpu.registers[srx.dst] = Float32.ToInt(value);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status ALLOC(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            int size = vm.cpu.registers[srx.dst];
            int address = Heap.Alloc(vm, size);
            vm.cpu.registers[Registers.EAX] = address;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status FREE(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            int address = vm.cpu.registers[srx.dst];
            return Heap.Free(vm, address);
        }

        internal static Cpu.Status PUSH(Script vm) {
            var srx = new SRX(vm, is_jump: false);

            vm.cpu.registers[Registers.ESP] -= 1;
            int stack_ptr = vm.cpu.registers[Registers.ESP];

            int word = vm.cpu.registers[srx.dst];
            vm.WriteWord(word, stack_ptr);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status POP(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            int stack_ptr = vm.cpu.registers[Registers.ESP];

            if (stack_ptr > Script.MAX_MEMORY) {
                return Cpu.Status.STACK_UNDERFLOW;
            }

            int word = vm.ReadWord(stack_ptr);
            vm.cpu.registers[Registers.ESP] += 1;
            vm.cpu.registers[srx.dst] = word;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status NOT(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            int dst = vm.cpu.registers[srx.dst];
            vm.cpu.registers[srx.dst] = -dst;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status NEG(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            int dst = vm.cpu.registers[srx.dst];
            vm.cpu.registers[srx.dst] = -dst;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status NEGF(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            float f0 = Float32.FromInt(vm.cpu.registers[srx.dst]);
            vm.cpu.registers[srx.dst] = Float32.ToInt(-f0);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status RET(Script vm) {
            var srx = new SRX(vm, is_jump: true);
            int address = vm.cpu.registers[srx.dst];

            if (address == -1) {
                // Returning to -1 address means the procedure
                // was called from native code, so we pause
                // then VM so that native code execution may continue.
                vm.cpu.status = Cpu.Status.OK;
                vm.cpu.eip += SRX.SIZE;
                vm.Halt(1);
                return Cpu.Status.OK;
            }

            vm.cpu.eip = address; // Jump to address
            return Cpu.Status.OK;
        }

        internal static Cpu.Status HALT(Script vm) {
            var srx = new SRX(vm, is_jump: false);
            int status_code = vm.cpu.registers[srx.dst];
            vm.Halt(status_code);
            return Cpu.Status.OK;
        }
    }

    struct IRX
    {
        internal const int SIZE = 2;
        internal byte dst;
        internal int constant;

        IRX(Script vm) {
            int data = vm.memory[vm.cpu.eip];
            byte op0 = (byte)((data & 0x00_FF_00_00) >> 16);
            int op1  = vm.memory[vm.cpu.eip + 1];

            if (op0 > Registers.NUM_REGISTERS) {
                vm.cpu.status = Cpu.Status.BAD_REGISTER;
                dst = 0;
                constant = op1;
                return;
            }

            vm.cpu.eip += IRX.SIZE;
            dst = op0;
            constant = op1;
        }

        internal static Cpu.Status LEA(Script vm) {
            var irx = new IRX(vm);
            vm.cpu.registers[irx.dst] = irx.constant;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status LDW(Script vm) {
            var irx = new IRX(vm);
            int address = vm.cpu.registers[irx.dst] + irx.constant;
            int value = vm.ReadWord(address);
            vm.cpu.registers[Registers.EAX] = value;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status STW(Script vm) {
            var irx = new IRX(vm);
            int value = vm.cpu.registers[Registers.EAX];
            int address = vm.cpu.registers[irx.dst] + irx.constant;
            vm.WriteWord(value, address);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status SLL(Script vm) {
            var irx = new IRX(vm);
            int dst = vm.cpu.registers[irx.dst];
            vm.cpu.registers[irx.dst] = dst << irx.constant;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status ADD(Script vm) {
            var irx = new IRX(vm);
            int dst = vm.cpu.registers[irx.dst];
            vm.cpu.registers[irx.dst] += irx.constant;
            return Cpu.Status.OK;
        }
    }

    struct IMM
    {
        internal const int SIZE = 2;
        internal int address;

        IMM(Script vm, bool is_jump) {
            int op = vm.memory[vm.cpu.eip + 1];

            if (is_jump) {
                // Verify the address is executable
                if (op < 0 || op > vm.cpu.dat) {
                    vm.cpu.status = Cpu.Status.BAD_MEMORY_ACCESS;
                    address = 0;
                    return;
                }
            } else {
                vm.cpu.eip += IMM.SIZE;
            }

            address = op;
        }

        internal static Cpu.Status JMP(Script vm) {
            var imm = new IMM(vm, is_jump: true);

            vm.cpu.eip = imm.address;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status JE(Script vm) {
            var imm = new IMM(vm, is_jump: true);
            if (vm.cpu.registers[Registers.EAX] == 1) {
                vm.cpu.eip = imm.address;
                return Cpu.Status.OK;
            }
            vm.cpu.eip += IMM.SIZE;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status JNE(Script vm) {
            var imm = new IMM(vm, is_jump: true);
            if (vm.cpu.registers[Registers.EAX] == 0) {
                vm.cpu.eip = imm.address;
                return Cpu.Status.OK;
            }
            vm.cpu.eip += IMM.SIZE;
            return Cpu.Status.OK;
        }

        internal static Cpu.Status PUSH(Script vm) {
            var imm = new IMM(vm, is_jump: false);
            vm.cpu.registers[Registers.ESP] -= 1;
            int stack_ptr = vm.cpu.registers[Registers.ESP];

            vm.WriteWord(imm.address, stack_ptr);
            return Cpu.Status.OK;
        }

        internal static Cpu.Status CALL(Script vm) {
            var imm = new IMM(vm, is_jump: true);

            vm.cpu.registers[Registers.ESP] -= 1;
            int stack_ptr = vm.cpu.registers[Registers.ESP];

            // Push pointer to next instrcution
            vm.WriteWord(vm.cpu.eip + IMM.SIZE, stack_ptr);
            vm.cpu.eip = imm.address; // Jump to address
            return Cpu.Status.OK;
        }

        internal static Cpu.Status CALX(Script vm) {
            var imm = new IMM(vm, is_jump: false);
            // Call extern procedure
            Value v = Runtime.Call(vm, vm.EnvironmentLookup(imm.address));

            if (!v.is_void) {
                vm.cpu.registers[Registers.EAX] = v.AsInt;
            }

            return Cpu.Status.OK;
        }
    }
}
