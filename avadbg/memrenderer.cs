using System;
using System.Collections.Generic;
using System.Text;
using Ava;
using Terminal.Gui;

namespace AvaDebug
{
    internal class MemRenderer
    {
        internal readonly List<string> backbuffer;
        Script vmx;

        internal MemRenderer(Script debug_vmx) {
            vmx = debug_vmx;
            backbuffer = new List<string>();
        }

        internal void Draw(int section_start, int section_end) {
            if (section_end == section_start) {
                // Empty buffer
                backbuffer.Clear();
                return;
            }

            int j = 0;

            for (int i = section_start; i < section_end; i += 4, j++) {
                if (j >= backbuffer.Count) {
                    backbuffer.Add(DrawLine((uint)i));
                    continue;
                }
                backbuffer[j] = DrawLine((uint)i);
            }

            j--;

            if (j >= backbuffer.Count - 1) {
                return;
            }

            for (; j < backbuffer.Count; j++) {
                // end-start is now smaller, some excess items in the
                // buffer must be removed
                backbuffer.RemoveAt(j);
            }
        }

        string DrawLine(uint address) {
            address = vmx.ConvertToRealAddress(address);
            byte b0 = vmx.memory[address+0];
            byte b1 = vmx.memory[address+1];
            byte b2 = vmx.memory[address+2];
            byte b3 = vmx.memory[address+3];

            char c0   = RenderChar(b0);
            char c1   = RenderChar(b1);
            char c2   = RenderChar(b2);
            char c3   = RenderChar(b3);

            return string.Format(
                "0x{0:x8} {1:x2} {2:x2} {3:x2} {4:x2} {5}{6}{7}{8}",
                address, b0, b1, b2, b3, c0, c1, c2, c3
            );
        }

        char RenderChar(byte b) {
            if (b < 32) {
                return '.';
            }

            return char.ConvertFromUtf32(b)[0];
        }
    }
}
