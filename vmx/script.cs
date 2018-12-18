using System;
using System.Text;
using System.Runtime.CompilerServices;

[assembly: InternalsVisibleTo("AvaDebug")]
namespace Ava
{
    internal struct Cpu
    {
        internal bool running;
        internal uint[] registers;
        internal uint ip; // Instruction Pointer
        internal uint dat; // Pointer to bottom of data section
        internal uint heap; // data/heap
        internal uint hp; // heap pointer
        //internal uint stk; // Pointer to top of stack section
        internal Status status;

        internal struct Status
        {
            internal bool CMP_ABOVE;
            internal bool CMP_EQUAL;
            internal bool CMP_BELOW;
            internal Error error;
        }

        internal enum Error
        {
            NONE,
            BAD_MEMORY_ACCESS,
            BAD_REGISTER,
            BAD_INSTRUCTION,
            STACK_UNDERFLOW, // ESP == sizeof(memory)
            STACK_OVERFLOW   // ESP == stk
        }
    }

    internal struct Registers
    {
        internal const int NUM_REGISTERS = 5;
        internal const byte EAX = 0; // Accumulator (general purpose)
        internal const byte ECX = 1; // Counter (general purpose)
        internal const byte EDX = 2; // Accumulator (general purpos)
        internal const byte ESP = 3; // Stack pointer
        internal const byte EBP = 4; // Base pointer (frame pointer)
    }

    internal struct Header
    {
        internal const int SIZE = 0xA;
        internal const int SIGN_SECTION    = 0x0;
        internal const int SIZE_SECTION    = 0x2;
        internal const int DAT_PTR_SECTION = 0x6;

        internal byte sign_magic;   // Assembler signature
        internal byte sign_version; // Bytecode version
        internal short initial_stack_size;
        internal short initial_heap_size;
        internal int dat_ptr; // End of text section, start of data section
    }

    public class Script
    {
        //
        // Allows system calls to use different input/output methods
        // ie. Using Debug.Write instead of Console.Write when the
        // print system call is executed. IO callbacks are shared
        // between abstract machines.
        //
        public struct IO
        {
            public delegate void WriteDel(string msg, params object[] args);
            public delegate string ReadDel();
            public static WriteDel Write;
            public static ReadDel Read;
        }

        public bool Running {
            get { return cpu.running; }
            set { cpu.running = value; }
        }

        public int MemoryUsage {
            get { return memory.Length; }
        }

        internal const int MAX_MEMORY = 0xFFFFFFF;
        internal Header header;
        internal Cpu cpu;
        internal byte[] memory;

        internal static Func<Script, bool>[] instruction_table = new Func<Script, bool>[64] {
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
            /* 0x19 */    SRX.HEAP,
            /* 0x1A */    SRX.PUSH,
            /* 0x1B */    SRX.POP,
            /* 0x1C */    SRX.NOT,
            /* 0x1D */    SRX.RET,
            /* 0x1E */    SRX.HALT,
            /* 0x1F */    SRX.FREE,

            /* 0x20 */    null,
            /* 0x21 */    null,
            /* 0x22 */    null,
            /* 0x23 */    null,

            /* 0x24 */    IMRX.LWA,
            /* 0x25 */    IMRX.LBA,
            /* 0x26 */    IMRX.SWA,
            /* 0x27 */    IMRX.SBA,
            /* 0x28 */    IMRX.SLL,
            /* 0x29 */    IMRX.SRL,
            /* 0x2A */    IMRX.SRLU,
            /* 0x2B */    IMRX.ADD,
            /* 0x2C */    IMRX.CMP,
            /* 0x2D */    IMRX.CMPU,
            /* 0x2E */    IMRX.LEA,

            /* 0x2F */    null,
            /* 0x30 */    null,
            /* 0x31 */    null,
            /* 0x32 */    null,
            /* 0x33 */    null,

            /* 0x34 */    IMM.JE,
            /* 0x35 */    IMM.JNE,
            /* 0x36 */    IMM.JL,
            /* 0x37 */    IMM.JLE,
            /* 0x38 */    IMM.JG,
            /* 0x39 */    IMM.JGE,
            /* 0x3A */    IMM.JMP,
            /* 0x3B */    IMM.PUSH,
            /* 0x3C */    IMM.CALL,
            /* 0x3D */    IMM.SYS,

            /* 0x3E */    null,
            /* 0x3F */    null,
        };

        public Script(byte[] bytecode) {
            memory = bytecode;
        }

        public bool Initialize() {
            cpu = new Cpu() {
                registers = new uint[Registers.NUM_REGISTERS],
                status = new Cpu.Status(),
                // ip starts out pointing to the beginning of text section
                ip = Header.SIZE
            };

            // Stack pointer starts at the top of memory
            cpu.registers[Registers.ESP] = MAX_MEMORY;

            int stack_heap = ReadWord(Header.SIZE_SECTION);

            header = new Header() {
                sign_magic = ReadByte(Header.SIGN_SECTION),
                sign_version = ReadByte(Header.SIGN_SECTION + 1),
                initial_stack_size = 16,//(short)stack_heap,
                initial_heap_size = 8, // @Temporary
                dat_ptr = ReadWord(Header.DAT_PTR_SECTION)
            };

            // @Todo: verify signature

            // Stack currently empty, stk points to the lowest possible
            // address for the stack pointer, which is the end of the
            // data section.
            //cpu.stk  = (uint)memory.Length;
            cpu.heap = (uint)memory.Length;
            cpu.dat = (uint)header.dat_ptr;
            cpu.hp = (uint)(cpu.heap + header.initial_heap_size);

            int mem_size = memory.Length
                         + header.initial_stack_size
                         + header.initial_heap_size;

            // Increase memory capacity to fit the stack and heap
            Array.Resize(ref memory, mem_size);
            return true;
        }

        //
        // Advance the virtual machine until the cpu halts
        // or the intruction pointer reaches the start of
        // the data section.
        //
        public void Run() {
            cpu.running = true;

            while (cpu.running && cpu.ip < cpu.dat) {
                Advance();
            }
        }

        //
        // Fetch and execute the instruction pointed by
        // the instruction pointer.
        //
        public void Advance() {
            byte opcode = memory[cpu.ip];
            bool error = Execute(opcode);

            if (error || cpu.status.error != 0) {
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
        public void Halt(int exit_code = 0, uint address = 0) {
            cpu.running = false;

            switch (exit_code) {
                case -1: {
                        // Issue fatal error
                        IO.Write("Fatal: {0} ({1:X8})\n", cpu.status.error, address);
                        Reset();
                        break;
                    }

                case 1: break; // Halt but save instruction pointer and registers

                default:
                case 0: Reset(); break; // Halt and reset
            }
        }

        public void Reset() {
            cpu.registers = new uint[Registers.NUM_REGISTERS];
            cpu.status = new Cpu.Status();
            cpu.ip = Header.SIZE;
            cpu.hp = (uint)(cpu.heap + header.initial_heap_size);


            int mem_size = (int)cpu.heap
                         + header.initial_stack_size
                         + header.initial_heap_size;

            Array.Clear(memory, (int)cpu.heap, memory.Length - (int)cpu.heap);
            Array.Resize(ref memory, mem_size);
            cpu.registers[Registers.ESP] = MAX_MEMORY;
        }

        public void WriteWord(uint value, uint virtual_address) {
            StackAlloc(); // Ensure there is enough memory
            uint address = ConvertToRealAddress(virtual_address);

            if (SegFault(address, sizeof(int))) {
                Halt(-1, virtual_address);
                return;
            }

            memory[address + 0] = (byte)((value & 0xff));
            memory[address + 1] = (byte)((value & 0xff00) >> 8);
            memory[address + 2] = (byte)((value & 0xff0000) >> 16);
            memory[address + 3] = (byte)((value & 0xff000000) >> 24);
        }

        public void WriteByte(byte value, uint virtual_address) {
            StackAlloc();
            uint address = ConvertToRealAddress(virtual_address);

            if (SegFault(address, sizeof(byte))) {
                Halt(-1, virtual_address);
                return;
            }

            memory[address] = value;
        }

        public int ReadWord(uint virtual_address) {
            uint address = ConvertToRealAddress(virtual_address);

            if (SegFault(address, sizeof(int))) {
                Halt(-1, virtual_address);
                return -1;
            }

            byte b0  = memory[address + 0];
            byte b1  = memory[address + 1];
            byte b2  = memory[address + 2];
            byte b3  = memory[address + 3];

            return b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
        }

        public byte ReadByte(uint virtual_address) {
            uint address = ConvertToRealAddress(virtual_address);

            if (SegFault(address, sizeof(int))) {
                Halt(-1, virtual_address);
                return 0xFF;
            }

            return memory[address];
        }

        public string ReadString(uint virtual_address) {
            uint address = ConvertToRealAddress(virtual_address);

            // First word in a string is the string size
            int size = ReadWord(address - sizeof(int));
            return Encoding.UTF8.GetString(memory, (int)address, size);
        }

        public uint PopArg() {
            uint stack_ptr = cpu.registers[Registers.ESP];

            if (stack_ptr > Script.MAX_MEMORY) {
                cpu.status.error = Cpu.Error.STACK_UNDERFLOW;
                Halt(-1);
                return 0xFFFFFFFF;
            }

            uint value = (uint)ReadWord(stack_ptr);
            cpu.registers[Registers.ESP] += sizeof(int);

            return value;
        }

        public uint ReadRegister(byte register) {
            if (register > Registers.NUM_REGISTERS) {
                cpu.status.error = Cpu.Error.BAD_REGISTER;
                return 0xFFFFFFFF;
            }
            return cpu.registers[register];
        }

        internal uint FloatToInt(float value) {
            return BitConverter.ToUInt32(BitConverter.GetBytes(value), 0);
        }

        internal float IntToFloat(uint value) {
            return BitConverter.ToSingle(BitConverter.GetBytes(value), 0);
        }

        internal bool Execute(byte opcode) {
            if (opcode > instruction_table.Length) {
                // Unknown instruction
                cpu.status.error = Cpu.Error.BAD_INSTRUCTION;
                return true;
            }

            var instruction = instruction_table[opcode];

            if (instruction == null) {
                // Unavalable instruction
                cpu.status.error = Cpu.Error.BAD_INSTRUCTION;
                return true;
            }

            return instruction(this);
        }

        internal bool SegFault(uint address, int size) {
            if (/*address < cpu.dat  || */ (address + size) > memory.Length) {
                // Attempt to write to text section, header or outside
                // memory bounds.
                cpu.status.error = Cpu.Error.BAD_MEMORY_ACCESS;
                return true;
            }
            return false;
        }

        internal uint ConvertToRealAddress(uint virtual_address) {
            // The stack grows down from higher addresses, we can map virtual stack
            // addresses to memory locations by subtracting the virtual_address
            // offset from the total memory capacity.
            int real_address = (int)((memory.Length) - (MAX_MEMORY - virtual_address));

            if (real_address < cpu.hp) {
                // The address does not point to the stack, so the virtual_address
                // is equivalent to the real_address.
                return virtual_address;
            }

            return (uint)real_address;
        }

        internal void StackAlloc() {
            // Use esp to determine if the stack is full
            uint esp = cpu.registers[Registers.ESP];
            uint address = ConvertToRealAddress(esp);

            if (address > cpu.hp) {
                // @Todo: address > cpu.hp
                // We already have enough memory
                return;
            }

            // Double the stack size
            int stack_size   = memory.Length - (int)cpu.hp;

            Array.Resize(ref memory, memory.Length + stack_size);
            // Move current stack to allocated stack section
            Array.Copy(memory, (int)cpu.hp, memory, (int)cpu.hp + stack_size, stack_size);
        }
    }
}
