using System;
using System.Text;
using System.Collections.Generic;
using System.Collections;

namespace Ava
{
    ///
    /// Used to call C# functions from Ava.
    /// Environment is shared between all Ava machine instances.
    /// Restrictions made on which env functions can be called
    /// are done by the Ava compiler.
    ///
    static class Runtime
    {
        //
        // Allows env calls to use different input/output methods
        // ie. Using Debug.Write instead of Console.Write when the
        // print call is executed. IO callbacks are shared
        // between abstract machines.
        //
        public delegate void WriteDel(object o);
        public delegate string ReadDel();

        public static WriteDel Write = Console.Write;
        public static ReadDel Read = Console.ReadLine;

        static Dictionary<string, Func<Script, Value>> function_map = new Dictionary<string, Func<Script, Value>>() {
            { "print_int",   (vm) => { Write(vm.Argument(0).AsInt); return Value.Void; } },
            { "print_float", (vm) => { Write(vm.Argument(0).AsFloat); return Value.Void; } },
            { "print_char",  (vm) => { Write((char)vm.Argument(0).AsInt); return Value.Void; } },
            { "print_str",   (vm) => { Write(vm.Argument(0).AsString); return Value.Void; } },
            { "printf",      (vm) => { Write(String.Format(vm)); return Value.Void; } },
            { "read_str",    (vm) => new Value(Runtime.Read(), vm) },
            { "read_int",    (vm) => new Value(int.Parse(Runtime.Read())) },
            { "push",        Sequence.Push },
            { "pop",         Sequence.Pop },
            { "clear",       Sequence.Clear },
            { "fill",        Sequence.Fill }
        };

        internal static bool Register(string name, Func<Script, Value> function) {
            if (function_map.ContainsKey(name)) {
                // Error
                return false;
            }

            function_map[name] = function;
            return true;
        }

        internal static bool Register(string[] names, Func<Script, Value>[] functions) {
            if (names.Length != functions.Length) {
                // @Todo: Error
                return false;
            }

            bool ok = true;

            for (int i = 0; i < names.Length; i++) {
                if (!Register(names[i], functions[i])) {
                    // Recover and keep checking for more errors
                    ok = false;
                }
            }

            return ok;
        }

        internal static Value Call(Script vm, string name) {
            if (!function_map.ContainsKey(name)) {
                // Error
                return Value.Void;
            }

            return function_map[name](vm);
        }
    }
}
