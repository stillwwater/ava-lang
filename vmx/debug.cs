using System.Reflection;


// @Temporary Debug disabled
#if false

namespace Ava
{
    public class Debug
    {
        public static string RegisterName(byte register) {
            switch (register) {
                case Registers.EAX: return "eax";
                case Registers.ECX: return "ecx";
                case Registers.EDX: return "edx";
                case Registers.ESP: return "esp";
                case Registers.EBP: return "ebp";
            }

            return "";
        }

        public static string Disassemble(Script vm, int ip, out int size) {
            byte opcode = vm.memory[ip];
            string name = Script.instruction_table[opcode].GetMethodInfo().Name.ToLower();

            uint ip_u = (uint)ip;

            if (opcode < 0x17) {
                byte op0 = vm.memory[ip + 1];
                byte op1 = vm.memory[ip + 2];
                string dst_s = RegisterName(op0);
                string src_s = RegisterName(op1);
                size = LRX.SIZE;
                return string.Format(
                    "0x{3:x8} {4:x2} {5:x2} {6:x2}           {0, -6}{1}, {2}",
                    name, dst_s, src_s, ip, opcode, op0, op1
                );
            }

            if (opcode < 0x24) {
                byte op0 = vm.memory[ip + 1];
                string dst = RegisterName(op0);
                size = SRX.SIZE;
                return string.Format(
                    "0x{2:x8} {3:x2} {4:x2}              {0, -6}{1}",
                    name, dst, ip, opcode, op0
                );
            }

            if (opcode < 0x34) {
                byte op0 = vm.memory[ip + 1];
                string dst = RegisterName(op0);
                int imm = vm.ReadWord((uint)ip + 2);
                byte b0 = vm.ReadByte(ip_u + 2);
                byte b1 = vm.ReadByte(ip_u + 3);
                byte b2 = vm.ReadByte(ip_u + 4);
                byte b3 = vm.ReadByte(ip_u + 5);
                size = IMRX.SIZE;
                return string.Format(
                    "0x{3:x8} {4:x2} {9:x2} {5:x2} {6:x2} {7:x2} {8:x2}  {0, -6}{1}, {2:X}",
                    name, dst, imm, ip, opcode, b0, b1, b2, b3, op0
                );
            }

            if (opcode < 0x40) {
                int imm = vm.ReadWord((uint)ip + 1);
                byte b0 = vm.ReadByte(ip_u + 1);
                byte b1 = vm.ReadByte(ip_u + 2);
                byte b2 = vm.ReadByte(ip_u + 3);
                byte b3 = vm.ReadByte(ip_u + 4);
                size = IMM.SIZE;
                return string.Format(
                    "0x{2:x8} {3:x2} {4:x2} {5:x2} {6:x2} {7:x2}     {0, -6}{1:X}",
                    name, imm, ip, opcode, b0, b1, b2, b3
                );
            }

            size = 0;
            return "";
        }
    }
}

#endif
