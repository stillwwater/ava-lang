using System;
using System.Text;

namespace Ava
{
    static class SysCallbacks
    {
        static Func<Script, uint>[] function_table;

        public static void AddReference(uint address, Func<Script, uint> callback) {
            if (address > function_table.Length) {
                // Error
                return;
            }

            function_table[address] = callback;
        }

        internal static void Initialize(int table_size) {
            function_table = new Func<Script, uint>[table_size];

            function_table[0] = PrintChar;
            function_table[1] = PrintInt;
            function_table[2] = PrintFloat;
            function_table[3] = PrintStr;
            function_table[4] = PrintF;
            function_table[5] = ReadStr;
        }

        internal static uint ExecuteCallback(Script vm, uint procedure_address) {
            if (procedure_address > function_table.Length) {
                // Error
                return 0;
            }

            return function_table[procedure_address](vm);
        }

        internal static uint PrintChar(Script vm) {
            // @Todo null check Script.IO.Write
            Script.IO.Write("{0}", (char)vm.PopArg());
            return 0;
        }

        internal static uint PrintInt(Script vm) {
            Script.IO.Write("{0}", (int)vm.PopArg());
            return 0;
        }

        internal static uint PrintFloat(Script vm) {
            float f = BitConverter.ToSingle(BitConverter.GetBytes(vm.PopArg()), 0);
            Script.IO.Write("{0}", f);
            return 0;
        }

        internal static uint PrintStr(Script vm) {
            uint address = vm.PopArg();
            string str = vm.ReadString(address);

            Script.IO.Write("{0}", str);
            return address;
        }

        internal static uint PrintF(Script vm) {
            // Arguments for printf should be pushed in reverse order.
            // The firt argument is pointer to a str which contains the format
            // The following arguments must be in the sequence specified by
            // the format string.
            uint address = vm.PopArg();
            string fmt = vm.ReadString(address);

            // @Performance
            var buffer = new StringBuilder();
            bool is_fmt_char = false;

            // Basic state-machine printf implementation
            foreach (char c in fmt) {
                if (c == '%') {
                    is_fmt_char = true;
                    continue;
                }

                if (is_fmt_char) {
                    switch (c) {
                        case 'd': {
                                buffer.Append(vm.PopArg());
                                break;
                            }
                        case 'f': {
                                float f = BitConverter.ToSingle(BitConverter.GetBytes(vm.PopArg()), 0);
                                buffer.Append(f);
                                break;
                            }
                        case 's': {
                                buffer.Append(vm.ReadString(vm.PopArg()));
                                break;
                            }
                        case 'c': {
                                buffer.Append(char.ConvertFromUtf32((int)vm.PopArg()));
                                break;
                            }
                        case '%': {
                                // %% means instert a % character
                                buffer.Append(c);
                                break;
                            }
                        default: break; // @Todo: error
                    }
                    is_fmt_char = false;
                    continue;
                }

                buffer.Append(c);
            }

            Script.IO.Write("{0}", buffer);
            return address;
        }

        internal static uint ReadStr(Script vm) {
            uint address = vm.PopArg();
            // First 4 bytes below the address is the size of the string
            int str_size = vm.ReadWord(address - sizeof(int));

            // Store user input in buffer
            byte[] read_buffer = Encoding.UTF8.GetBytes(Script.IO.Read());

            // Copy the buffer to the strings address.
            int buffer_size = Math.Min(read_buffer.Length, str_size);
            Array.Copy(read_buffer, 0, vm.memory, (int)address, buffer_size);

            return address;
        }
    }
}
