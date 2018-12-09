using System;
using System.Text;

namespace VM
{
    internal struct Cpu
    {
        internal bool running;
        internal uint[] registers;
        internal uint inp; // Instruction Pointer
        internal uint dat; // Pointer to bottom of data section
        internal uint stk; // Pointer to top of stack section
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

        internal const int MAX_MEMORY = 1024;
        internal Header header;
        internal Cpu cpu;
        internal byte[] memory;

        public Script(byte[] bytecode) {
            memory = bytecode;
        }

        public bool Initialize() {
            cpu = new Cpu() {
                registers = new uint[Registers.NUM_REGISTERS],
                status    = new Cpu.Status(),
                // ip starts out pointing to the beginning of text section
                inp       = Header.SIZE
            };

            // Stack pointer starts at the top of memory
            cpu.registers[Registers.ESP] = MAX_MEMORY;

            int stack_heap = ReadWord(Header.SIZE_SECTION);

            header = new Header() {
                sign_magic         = ReadByte(Header.SIGN_SECTION),
                sign_version       = ReadByte(Header.SIGN_SECTION + 1),
                initial_stack_size = (short)stack_heap,
                initial_heap_size  = 0,
                dat_ptr            = ReadWord(Header.DAT_PTR_SECTION)
            };

            // @Todo: verify signature

            // Stack currently empty, stk points to the lowest possible
            // address for the stack pointer, which is the end of the
            // data section.
            cpu.stk = (uint)memory.Length;
            cpu.dat = (uint)header.dat_ptr;

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

            while (cpu.running && cpu.inp < cpu.dat) {
                Advance();
            }
        }

        //
        // Fetch and execute the instruction pointed by
        // the instruction pointer.
        //
        internal void Advance() {
            byte opcode = memory[cpu.inp];
            bool error = Instructions.Execute(opcode, this);

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
            cpu.status    = new Cpu.Status();
            cpu.inp       = Header.SIZE;


            int mem_size = (int)cpu.stk
                         + header.initial_stack_size
                         + header.initial_heap_size;

            Array.Resize(ref memory, mem_size);
            cpu.registers[Registers.ESP] = MAX_MEMORY;
        }

        public void WriteWord(uint value, uint virtual_address) {
            uint address = ConvertToRealAddress(virtual_address);

            // Ensure there is enough memory
            StackAlloc(sizeof(int));

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
            uint address = ConvertToRealAddress(virtual_address);

            StackAlloc(sizeof(byte));

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
            int size = ReadWord(address);
            return Encoding.UTF8.GetString(memory, (int)address+4, size);
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
            int real_address = (int)((memory.Length - 1) - (MAX_MEMORY - virtual_address));

            if (real_address < cpu.stk) {
                // The address does not point to the stack, so the virtual_address
                // is equivalent to the real_address.
                return virtual_address;
            }

            return (uint)real_address;
        }

        internal void StackAlloc(int size) {
            // Use esp to determine if the stack is full
            uint esp = cpu.registers[Registers.ESP];
            uint address = ConvertToRealAddress(esp) - sizeof(int);

            if (address > cpu.stk) {
                // We already have enough memory
                return;
            }

            // Double the stack size
            int stack_size   = memory.Length - (int)cpu.stk;
            int section_size = (int)(memory.Length - cpu.stk + address) * 2;

            Array.Resize(ref memory, section_size);

            // Move current stack to allocated stack section
            Array.Copy(memory, cpu.stk, memory, section_size - stack_size, stack_size);
        }
    }
}
