using System;
using System.Collections.Generic;
using System.Text;
using Ava;
using Terminal.Gui;

namespace AvaDebug
{
    internal class SrcRenderer
    {
        struct Token
        {
            internal string value;
            internal int position;

            public override string ToString() => value;
        }

        internal readonly List<string> backbuffer;

        Script vmx;
        Token last_draw;
        Token[] tokens;

        internal SrcRenderer(Script debug_vmx) {
            vmx = debug_vmx;
            backbuffer = new List<string>();
            SetupBuffer();
        }

        internal void Draw(ListView view, double time) {
            int ip = (int)vmx.cpu.ip;
            Token selected = tokens[ip];

            // Restore last drawn item
            DrawLine(last_draw.position, "{0,-64} {1,-15}", last_draw, "");

            // Draw current instruction
            DrawLine(selected.position, "{0, -50} {1:0.0000}ms", selected, time);
            view.SelectedItem = selected.position;
            last_draw = selected;
        }

        void SetupBuffer() {
            tokens = new Token[vmx.cpu.dat];
            backbuffer.Add("");

            int ip = Header.SIZE;

            for (int i = 1; ; i++) {
                string src = Ava.Debug.Disassemble(vmx, ip, out int size);

                tokens[ip] = new Token() {
                    value = src,
                    position = i
                };

                backbuffer.Add(string.Format("{0, -64} {1, -15}", tokens[ip].value, ""));

                if (vmx.memory[ip] == 0x1e) {
                    break;
                }

                ip += size;
            }
        }

        void DrawLine(int y, string fmt, params object[] args) {
            backbuffer[y] = string.Format(fmt, args);
        }
    }
}
