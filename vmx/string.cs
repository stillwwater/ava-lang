using System;
using System.Text;

namespace Ava
{
    class String
    {
        internal const int BUFFER_SIZE = 4096;
        static byte[] strbuf = new byte[BUFFER_SIZE];

        ///
        /// Create unpacked UTF32 string from byte array.
        /// start points to the first byte in the sequence
        /// and start-sizeof(int) points to the string length
        /// in cell sized characters.
        ///
        internal static string FromBytes(byte[] bytes, int start) {
            int length = Script.ReadWord(bytes, start - sizeof(int));
            return Encoding.UTF32.GetString(bytes, start, length << 2);
        }

        ///
        /// Allocate and store unpacked UTF32
        /// string on the heap.
        ///
        internal static int AllocString(Script vm, string str) {
            int address = Heap.Alloc(vm, str.Length);

            for (int i = 0; i < str.Length; i++) {
                vm.WriteWord((int)str[i], address + i);
            }

            return address;
        }

        internal static int GetLength(Script vm, int address) {
            return vm.memory[address - 1];
        }

        ///
        /// Read unpacked UTF32 string from memory, with
        /// address pointing to the first character and
        /// address-1 pointing to the string length.
        ///
        internal static string ReadString(Script vm, int address) {
            int length = vm.memory[address - 1];

            if (strbuf.Length < length) {
                // @Performance: Free large buffer back to
                // default size.
                Array.Resize(ref strbuf, length);
            }

            for (int i = address; i < address + length; i++) {
                int value = vm.memory[i];
                strbuf[((i - address) << 2) + 0] = (byte)((value & 0xff));
                strbuf[((i - address) << 2) + 1] = (byte)((value & 0xff00) >> 8);
                strbuf[((i - address) << 2) + 2] = (byte)((value & 0xff0000) >> 16);
                strbuf[((i - address) << 2) + 3] = (byte)((value & 0xff000000) >> 24);
            }
            return Encoding.UTF32.GetString(strbuf, 0, length << 2);
        }

        ///
        /// Print formatted string from the current stack pointer.
        /// Format must be called with arguments pushed in reverse
        /// order to the stack and ESP pointing to a format string
        /// in memory.
        ///
        internal static string Format(Script vm) {
            // Arguments for printf should be pushed in reverse order.
            // The first argument is pointer to a str which contains the format
            // The following arguments must be in the sequence specified by
            // the format string.
            string fmt = vm.Argument(0).AsString;

            // @Performance
            var buffer = new StringBuilder();
            bool is_fmt_char = false;

            // Basic state-machine printf implementation
            int arg = 0;

            foreach (char c in fmt) {
                if (c == '%') {
                    is_fmt_char = true;
                    continue;
                }

                if (is_fmt_char) {
                    switch (c) {
                        case 'd': {
                                int d = vm.Argument(++arg).AsInt;
                                buffer.Append(d);
                                break;
                            }
                        case 'f': {
                                float f = vm.Argument(++arg).AsFloat;
                                buffer.Append(f);
                                break;
                            }
                        case 's': {
                                string s = vm.Argument(++arg).AsString;
                                buffer.Append(s);
                                break;
                            }
                        case 'n': {
                                buffer.AppendLine();
                                break;
                            }
                        case 'c': {
                                int i = vm.Argument(++arg).AsInt;
                                buffer.Append(char.ConvertFromUtf32(i));
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

            return buffer.ToString();
        }
    }
}
