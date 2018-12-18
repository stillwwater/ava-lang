using System;
using System.Collections.Generic;
using System.Text;
using Ava;

namespace AvaDebug
{
    internal class CpuRenderer
    {
        internal readonly List<string> backbuffer_left;
        internal readonly List<string> backbuffer_right;
        Script vmx;

        internal CpuRenderer(Script debug_vmx) {
            vmx = debug_vmx;
            backbuffer_left   = new List<string>();
            backbuffer_right  = new List<string>();
            SetupBuffers();
        }

        internal void Draw() {
            for (int i = 0; i < Registers.NUM_REGISTERS; i++) {
                backbuffer_left[i] = DrawLine((byte)i);
            }

            backbuffer_right[0] = (DrawLine("ip", vmx.cpu.ip));
            backbuffer_right[1] = (DrawLine("dat", vmx.cpu.dat));
            backbuffer_right[2] = (DrawLine("heap", vmx.cpu.heap));
            backbuffer_right[3] = (DrawLine("hp", vmx.cpu.hp));

            uint esp = vmx.ConvertToRealAddress(vmx.cpu.registers[Registers.ESP]);
            backbuffer_right[4] = (DrawLine("sp", esp));
        }

        void SetupBuffers() {
            for (int i = 0; i < Registers.NUM_REGISTERS; i++) {
                backbuffer_left.Add(DrawLine((byte)i));
            }

            backbuffer_right.Add(DrawLine("ip", vmx.cpu.ip));
            backbuffer_right.Add(DrawLine("dat", vmx.cpu.dat));
            backbuffer_right.Add(DrawLine("heap", vmx.cpu.heap));
            backbuffer_right.Add(DrawLine("hp", vmx.cpu.hp));

            uint esp = vmx.ConvertToRealAddress(vmx.cpu.registers[Registers.ESP]);
            backbuffer_right.Add(DrawLine("sp", esp));
        }

        string DrawLine(byte register) {
            string name = Ava.Debug.RegisterName(register);
            uint value = vmx.cpu.registers[register];
            return string.Format("{0}: 0x{1:x8}", name, value);
        }

        string DrawLine(string name, uint value) {
            return string.Format("{0,-4} 0x{1:x8}", name, value);
        }
    }
}
