using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;

namespace VM
{
    public class Debugger
    {
        struct Token
        {
            internal string value;
            internal int position;

            public override string ToString() => value;
        }

        Token[] tokens;
        List<int> token_lookup;
        int[] last_draw = new int[7] {-1, -1, -1, -1, -1, -1, -1};

        public static void Write(string format, params object[] args) {
            Console.SetCursorPosition(56, 12);
            Console.WriteLine(format, args);
        }

        public static string Read() {
            Console.SetCursorPosition(56, 12);
            return Console.ReadLine();
        }

        public void Debug(Script vm, double speed) {
            var sw = new Stopwatch();

            if (tokens == null) {
                InitBuffers(vm);
            }

            // Convert instructions per second to milisecond delay
            // A speed of 0 runs in immediate time
            int delay = (int)(speed > 0 ? (1 / speed) * 1000 : 0);
            int instruction_count = 0;
            double cpu_time = 0;

            vm.Reset();
            vm.cpu.running = true;

            for (int i = 0; true; i++) {
                sw.Restart();
                vm.Advance();
                sw.Stop();

                double time = (double)sw.ElapsedTicks / 10_000;
                cpu_time += time;
                instruction_count++;

                if (!vm.cpu.running) {
                    break;
                }

                Draw(vm, time);

                if (speed == 0) {
                    if (Console.ReadKey().Key == ConsoleKey.Escape) {
                        break;
                    }
                } else {
                    Thread.Sleep(delay);
                }
            }

            Console.SetCursorPosition(8, tokens[tokens.Length - 1].position);
            System.Console.WriteLine("Time: {0:0.0000}ms ({1} cycles)", cpu_time, instruction_count);
        }

        void InitBuffers(Script vm) {
            // @Todo: use cpu.dat
            tokens = new Token[vm.memory.Length];

            // 3 lines taken up by header
            token_lookup = new List<int>() {0, 0, 0};

            Console.CursorVisible = false;
            Console.Clear();

            int ip = Header.SIZE;

            for (int i = 3; ; i++) {
                string src = Instructions.Disassemble(vm, ip, out int size);

                tokens[ip] = new Token() {
                    value = src,
                    position = i
                };

                token_lookup.Add(ip);

                if (vm.memory[ip] == 0x1e) {
                    break;
                }
                ip += size;
            }

            token_lookup.AddRange(new int[] { 0, 0, 0, 0, 0 });

            Console.WriteLine("=======================================================================");
            Console.WriteLine("{0,-2}  {1,-40}  {2,-8}| CPU Registers |", "", "CPU", "Latency");
            Console.WriteLine("=======================================================================");

            for (int i = 0; i < tokens.Length; i++) {
                if (tokens[i].position > 0) {
                    Console.WriteLine("    {0, -50}|{1, -15}", tokens[i].value, "");
                }
            }
        }

        void Draw(Script vm, double time) {
            int ip = (int)vm.cpu.ip;

            int x = 0;
            int y = tokens[ip].position;

            for (int i = 0; i < last_draw.Length; i++) {
                if (last_draw[i] < 0) continue;

                Console.SetCursorPosition(x, tokens[last_draw[i]].position);
                Console.WriteLine("    {0,-50}|{1,-15}", tokens[last_draw[i]], "");
            }

            int top_y = Console.WindowTop + 4;

            for (int i = 0; i < 5; i++) {
                uint value = vm.cpu.registers[i];
                int cip = token_lookup[top_y + i];
                Console.SetCursorPosition(x, top_y + i);
                Console.WriteLine("    {0, -50}| {1}: {2:X}", tokens[cip], Instructions.RegisterName((byte)i), value);
                last_draw[i + 1] = cip;
            }

            Console.SetCursorPosition(x, top_y + 6);
            Console.WriteLine("    {0, -50}| mem: {1} B", tokens[token_lookup[top_y + 6]], vm.memory.Length);
            last_draw[6] = token_lookup[top_y + 6];

            Console.SetCursorPosition(x, y);
            Console.BackgroundColor = ConsoleColor.DarkCyan;
            Console.ForegroundColor = ConsoleColor.White;
            Console.WriteLine("--> {0, -36}<-- [{1:0.0000}ms]", tokens[ip], time);
            Console.ResetColor();
            last_draw[0] = ip;

            int center = y;
        }
    }
}
