using System;
using System.Text;
using System.Runtime.CompilerServices;
using System.Collections.Generic;

using cell = System.Int32;

[assembly: InternalsVisibleTo("AvaDebug")]
namespace Ava
{
    internal struct Cpu
    {
        internal bool running;
        internal cell[] registers;
        internal int eip; // Instruction Pointer
        internal int dat; // Pointer to bottom of data section
        internal int heap; // data/heap
        internal int hp; // heap pointer
        internal Status status;

        internal enum Status
        {
            OK,
            BAD_MEMORY_ACCESS,
            BAD_REGISTER,
            BAD_INSTRUCTION,
            STACK_UNDERFLOW, // ESP == sizeof(memory)
            STACK_OVERFLOW   // ESP == stk
        }
    }

    internal struct Registers
    {
        internal const int NUM_REGISTERS = 4;
        internal const byte EAX = 0; // Accumulator (general purpose)
        internal const byte EDX = 1; // Accumulator (general purpose)
        internal const byte ESP = 2; // Stack pointer
        internal const byte EBP = 3; // Base pointer (frame pointer)
    }

    internal struct Header
    {
        internal const int SIZE = 0x20;
        internal short sign_magic;   // Assembler signature
        internal short sign_version; // Bytecode version
        internal int initial_stack_size;
        internal int initial_heap_size;
        internal int import_table_ptr;
        internal int export_table_ptr;
        internal int text_ptr;
        internal int dat_ptr; // End of text section, start of data section
    }

    public class Script
    {
        public bool Running {
            get { return cpu.running; }
            set { cpu.running = value; }
        }

        ///
        /// Memory usage in bytes
        ///
        public int MemoryUsage {
            get { return memory.Length * sizeof(cell); }
        }

        public string Name { get; private set; }

        internal const int MAX_MEMORY = 0xFFFFFFF;
        internal Header header;
        internal Cpu cpu;
        internal cell[] memory;

        Dictionary<string, int> locals;
        Dictionary<int, string> imports;

        internal static Func<Script, Cpu.Status>[] instruction_table = new Func<Script, Cpu.Status>[64] {
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
            /* 0x0B */    LRX.AND,
            /* 0x0C */    LRX.OR,
            /* 0x0D */    LRX.XOR,
            /* 0x0E */    LRX.CEQ,
            /* 0x0F */    LRX.CNE,
            /* 0x10 */    LRX.CLT,
            /* 0x11 */    LRX.CLE,
            /* 0x12 */    LRX.CGT,
            /* 0x13 */    LRX.CGE,
            /* 0x14 */    LRX.CLTU,
            /* 0x15 */    LRX.CLEU,
            /* 0x16 */    LRX.CGTU,
            /* 0x17 */    LRX.CGEU,
            /* 0x18 */    LRX.CLTF,
            /* 0x19 */    LRX.CLEF,
            /* 0x1A */    LRX.CGTF,
            /* 0x1B */    LRX.CGEF,
            /* 0x1C */    LRX.MULF,
            /* 0x1D */    LRX.DIVF,
            /* 0x1E */    LRX.ADDF,
            /* 0x1F */    LRX.SUBF,

            /* 0x20 */    SRX.CVTFW,
            /* 0x21 */    SRX.CVTWF,
            /* 0x22 */    SRX.ALLOC,
            /* 0x23 */    SRX.FREE,
            /* 0x24 */    SRX.PUSH,
            /* 0x25 */    SRX.POP,
            /* 0x26 */    SRX.NOT,
            /* 0x27 */    SRX.NEG,
            /* 0x28 */    SRX.NEGF,
            /* 0x29 */    SRX.RET,
            /* 0x2A */    SRX.HALT,

            /* 0x2B */    null,
            /* 0x2C */    null,

            /* 0x2D */    IRX.LDW,
            /* 0x2E */    IRX.LEA,
            /* 0x2F */    IRX.STW,
            /* 0x30 */    IRX.ADD,
            /* 0x31 */    IRX.SLL,

            /* 0x32 */    null,
            /* 0x33 */    null,
            /* 0x34 */    null,
            /* 0x35 */    null,
            /* 0x36 */    null,
            /* 0x37 */    null,
            /* 0x38 */    null,
            /* 0x39 */    null,

            /* 0x3A */    IMM.JMP,
            /* 0x3B */    IMM.JE,
            /* 0x3C */    IMM.JNE,
            /* 0x3D */    IMM.PUSH,
            /* 0x3E */    IMM.CALL,
            /* 0x3F */    IMM.CALX,

        };

        public Script(string name) {
            Name = name;
        }

        public bool Initialize(byte[] bytecode) {
            cpu = new Cpu() {
                registers = new int[Registers.NUM_REGISTERS],
                status = new Cpu.Status(),
                // ip starts out pointing to the beginning of text section
                eip = Header.SIZE
            };

            // Stack pointer starts at the top of memory
            cpu.registers[Registers.ESP] = MAX_MEMORY;

            header = new Header() {
                sign_magic         = (short)(bytecode[0] | (bytecode[1] << 8)),
                sign_version       = (short)(bytecode[2] | (bytecode[3] << 8)),
                initial_stack_size = ReadWord(bytecode, 4),
                initial_heap_size  = ReadWord(bytecode, 8),
                import_table_ptr   = ReadWord(bytecode, 12),
                export_table_ptr   = ReadWord(bytecode, 16),
                text_ptr           = ReadWord(bytecode, 20),
                dat_ptr            = ReadWord(bytecode, 24)
            };

            // @Todo: verify signature
            int memory_size =
                (bytecode.Length >> 2)
                + header.initial_heap_size
                + header.initial_stack_size
                - (header.text_ptr >> 2);

            memory = new cell[memory_size];

            imports = new Dictionary<int, string>();
            locals  = new Dictionary<string, int>();

            // @Todo import table

            for (int i = header.import_table_ptr + 4; i < header.export_table_ptr;) {
                // Assuming UTF32 encoded string
                string name = Ava.String.FromBytes(bytecode, i);
                imports.Add(i >> 2, name);

                // String length in bytes + length value
                i += (name.Length * sizeof(cell)) + sizeof(cell);
            }

            for (int i = header.export_table_ptr + 4; i < header.text_ptr;) {
                // Assuming UTF32 encoded string
                string name = Ava.String.FromBytes(bytecode, i);
                i += (name.Length * sizeof(cell));

                int ptr = Script.ReadWord(bytecode, i);
                i += sizeof(int) * 2;

                locals.Add(name, ptr);
            }

            {
                int address = 0;
                for (int i = header.text_ptr; i < bytecode.Length; i += 4) {
                    memory[address] = Script.ReadWord(bytecode, i);
                    address++;
                }
            }

            cpu.registers = new cell[Registers.NUM_REGISTERS];

            // Stack currently empty, stk points to the lowest possible
            // address for the stack pointer, which is the end of the
            // data section.
            cpu.dat  = ((header.dat_ptr >> 2) - (header.text_ptr >> 2));
            cpu.heap = cpu.dat + 1;
            cpu.hp   = (cpu.heap + header.initial_heap_size);
            cpu.eip  = 0;
            cpu.registers[Registers.ESP] = MAX_MEMORY;
            return true;
        }

        public Value Call(string procedure) {
            cell stack_ptr = cpu.registers[Registers.ESP];
            return CallHelper(procedure, stack_ptr);
        }

        public Value Call(string procedure, Value a0) {
            cell stack_ptr = cpu.registers[Registers.ESP];
            WriteWord(a0.AsInt, --stack_ptr);

            return CallHelper(procedure, stack_ptr);
        }

        public Value Call(string procedure, Value a0, Value a1) {
            cell stack_ptr = cpu.registers[Registers.ESP];
            WriteWord(a1.AsInt, --stack_ptr);
            WriteWord(a0.AsInt, --stack_ptr);

            return CallHelper(procedure, stack_ptr);
        }

        public Value Call(string procedure, Value a0, Value a1, Value a2) {
            cell stack_ptr = cpu.registers[Registers.ESP];
            WriteWord(a2.AsInt, --stack_ptr);
            WriteWord(a1.AsInt, --stack_ptr);
            WriteWord(a0.AsInt, --stack_ptr);

            return CallHelper(procedure, stack_ptr);
        }

        public Value Call(string procedure, Value a0, Value a1, Value a2, Value a3) {
            cell stack_ptr = cpu.registers[Registers.ESP];
            WriteWord(a3.AsInt, --stack_ptr);
            WriteWord(a2.AsInt, --stack_ptr);
            WriteWord(a1.AsInt, --stack_ptr);
            WriteWord(a0.AsInt, --stack_ptr);

            return CallHelper(procedure, stack_ptr);
        }

        public Value Call(string procedure, params Value[] args) {
            cell stack_ptr = cpu.registers[Registers.ESP];

            for (int i = args.Length - 1; i >= 0; i--) {
                // Arguments must be pushed to the stack in reverse order
                WriteWord(args[i].AsInt, --stack_ptr);
            }

            return CallHelper(procedure, stack_ptr);
        }

        internal Value CallHelper(string procedure, int stack_ptr) {
            ulong ins = 0;
            cpu.eip = locals[procedure];

            // Push return address 0xFFFFFFFF
            WriteWord(-1, --stack_ptr);
            cpu.registers[Registers.ESP] = stack_ptr;
            cpu.running = true;

            while (cpu.running && cpu.eip < cpu.dat) {
                Advance();
                ins += 1;
            }

            Runtime.Write(string.Format("cycles: {0} million\n", (double)ins / 1_000_000));
            return new Value(cpu.registers[Registers.EAX]);
        }

        //
        // Fetch and execute the instruction pointed by
        // the instruction pointer.
        //
        public void Advance() {
            cell instruction = memory[cpu.eip];
            cpu.status = Execute(instruction);

            if (cpu.status != Cpu.Status.OK) {
                Halt(-1);
            }
        }

        //
        // Halt the virtual machine.
        // Exit codes:
        //   -1: Halt, output error flags, reset the VM
        //    0: Halt, reset the VM
        //    1: Halt, but save the machine state
        //
        public void Halt(int exit_code = 0, int address = 0) {
            cpu.running = false;

            switch (exit_code) {
                case -1: {
                        // Issue fatal error
                        Runtime.Write(string.Format("Fatal: {0} (0x{1:X8})\n\n", cpu.status, address));
                        Runtime.Write(string.Format("    eip: {0:X8}\n", cpu.eip));
                        Runtime.Write(string.Format("    eax: {0:X8}\n", cpu.registers[0]));
                        Runtime.Write(string.Format("    edx: {0:X8}\n", cpu.registers[1]));
                        Runtime.Write(string.Format("    esp: {0:X8}\n", cpu.registers[2]));
                        Runtime.Write(string.Format("    ebp: {0:X8}\n", cpu.registers[3]));
                        Reset();
                        break;
                    }

                case 1: break; // Halt but save instruction pointer and registers

                default:
                case 0: {
                    // Halt and reset
                    Reset();
                    break;
                }
            }
        }

        public void Reset() {
            cpu.registers = new cell[Registers.NUM_REGISTERS];
            cpu.status = new Cpu.Status();
            cpu.eip = Header.SIZE;
            cpu.hp = (cpu.heap + header.initial_heap_size);


            // @Todo
            int mem_size = (int)cpu.heap
                         + header.initial_stack_size
                         + header.initial_heap_size;

            Array.Clear(memory, (int)cpu.heap, memory.Length - (int)cpu.heap);
            Array.Resize(ref memory, mem_size);
            cpu.registers[Registers.ESP] = MAX_MEMORY;
        }

        public Value Argument(int argument_index) {
            int address = cpu.registers[Registers.ESP];

            if (address > Script.MAX_MEMORY) {
                cpu.status = Cpu.Status.STACK_UNDERFLOW;
                Halt(-1);
                return Value.Void;
            }

            int value = (int)ReadWord(address + argument_index);
            return new Value(value, this);
        }

        public void WriteWord(int value, int virtual_address) {
            StackAlloc(); // Ensure there is enough memory
            int address = ConvertToRealAddress(virtual_address);

            if (SegFault(address, 1)) {
                Halt(-1, virtual_address);
                return;
            }

            memory[address] = value;
        }

        public int ReadWord(int virtual_address) {
            int address = ConvertToRealAddress(virtual_address);

            if (SegFault(address, 1)) {
                Halt(-1, virtual_address);
                return -1;
            }

            return memory[address];
        }

        internal static int ReadWord(byte[] bytes, int start) {
            byte b0  = bytes[start + 0];
            byte b1  = bytes[start + 1];
            byte b2  = bytes[start + 2];
            byte b3  = bytes[start + 3];

            return b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
        }

        internal Cpu.Status Execute(int data) {
            byte opcode = (byte)(data & 0x000000FF);

            if (opcode > instruction_table.Length) {
                // Unknown instruction
                return Cpu.Status.BAD_INSTRUCTION;
            }

            var instruction = instruction_table[opcode];

            if (instruction == null) {
                // Unavalable instruction
                return Cpu.Status.BAD_INSTRUCTION;
            }

            return instruction(this);
        }

        internal string EnvironmentLookup(int import_address) {
            return imports[import_address];
        }

        bool SegFault(int address, int size) {
            if (/*address < cpu.dat  || */ (address + size) > memory.Length) {
                // Attempt to write to text section, header or outside
                // memory bounds.
                cpu.status = Cpu.Status.BAD_MEMORY_ACCESS;
                return true;
            }
            return false;
        }

        internal void MemCopy(int src, int dst, int length) {
            Array.Copy(memory, src, memory, dst, length);
        }

        internal int ConvertToRealAddress(int virtual_address) {
            // The stack grows down from higher addresses, we can map virtual stack
            // addresses to memory locations by subtracting the virtual_address
            // offset from the total memory capacity.
            int real_address = (int)((memory.Length) - (MAX_MEMORY - virtual_address));

            if (real_address < cpu.hp) {
                // The address does not point to the stack, so the virtual_address
                // is equivalent to the real_address.
                return virtual_address;
            }

            return real_address;
        }

        internal void StackAlloc() {
            // Use esp to determine if the stack is full
            cell esp = cpu.registers[Registers.ESP];
            int address = ConvertToRealAddress(esp);

            if (address > cpu.hp + 1) {
                // We already have enough memory
                return;
            }

            // Double the stack size
            int stack_size = memory.Length - cpu.hp;

            Array.Resize(ref memory, memory.Length + stack_size);
            // Move current stack to allocated stack section
            Array.Copy(memory, cpu.hp, memory, cpu.hp + stack_size, stack_size);
        }
    }
}
