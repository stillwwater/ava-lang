using System;
using System.IO;

namespace VM
{
    class Program
    {
        static void Main(string[] args) {
            byte[] byte_code = File.ReadAllBytes("out.bgx");

            var vm = new Script(byte_code);

            Script.IO.Write = Debugger.Write;
            Script.IO.Read  = Debugger.Read;

            ConsoleKeyInfo response;

            do {
                Console.Clear();
                var db = new Debugger();

                vm.Initialize();
                SysCallbacks.Initialize(8);

                Console.Write("== VM Debug ==\nSpeed (i/s): ");
                int speed = int.Parse(Console.ReadLine());

                while (true) {
                    db.Debug(vm, speed);
                    response = Console.ReadKey();

                    if (response.Key == ConsoleKey.Escape) {
                        break;
                    }
                }
            } while (response.Key == ConsoleKey.R);
        }
    }
}
