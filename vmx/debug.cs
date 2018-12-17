using System.Reflection;

namespace Ava
{
    public class Debug
    {
        public static string RegisterName(byte register) {
            switch (register) {
                case Registers.EAX: return "EAX";
                case Registers.ECX: return "ECX";
                case Registers.EDX: return "EDX";
                case Registers.ESP: return "ESP";
                case Registers.EBP: return "EBP";
            }

            return "";
        }

        public static string Disassemble(Script vm, int ip, out int size) {
            byte opcode = vm.memory[ip];
            string name = Script.instruction_table[opcode].GetMethodInfo().Name.ToLower();

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
                size = IMRX.SIZE;
                return string.Format("{3:X8}  {4:X2}  {0, -8}{1}, {2:X}", name, dst, imm, ip, opcode);
            }

            if (opcode < 0x40) {
                int imm = vm.ReadWord((uint)ip + 1);
                size = IMM.SIZE;
                return string.Format("{2:X8}  {3:X2}  {0, -8}{1:X}", name, imm, ip, opcode);
            }

            size = 0;
            return "";
        }
    }
}
