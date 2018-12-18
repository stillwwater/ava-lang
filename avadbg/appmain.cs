using System;
using System.Collections.Generic;
using System.Diagnostics;
using Ava;
using Terminal.Gui;

namespace AvaDebug
{
    internal class AppMain
    {
        internal enum RenderState
        {
            STEP,
            CONTINUOUS,
            PAUSE,
            HALT
        }

        internal enum Layout
        {
            TINY,
            TALL,
            WIDE,
        }

        internal RenderState state;
        internal Layout layout;
        internal readonly string app_name;
        static List<string> io_buffer = new List<string>();

        ListView src_view;
        ListView mem_view;
        ListView stk_view;
        ListView hea_view;
        ListView[] cpu_views;

        Window src_win;
        Window mem_win;
        Window stk_win;
        Window hea_win;

        int[] offsets = new int[] { 2, 1, 0 };

        internal AppMain(string name) {
            app_name = name;
        }

        internal static void Write(string format, params object[] args) {
            io_buffer.Add(string.Format(format, args));
        }

        internal void Run(Toplevel top, Script vmx) {
            var src = new SrcRenderer(vmx);
            var cpu = new CpuRenderer(vmx);
            var stk = new MemRenderer(vmx);
            var hea = new MemRenderer(vmx);
            var mem = new MemRenderer(vmx);

            src_win = SetupSrcWindow(top, src.backbuffer);
            stk_win = SetupMemWindow(top, stk.backbuffer, "Stack", offsets[0], out stk_view);
            hea_win = SetupMemWindow(top, hea.backbuffer, "Heap", offsets[1], out hea_view);
            mem_win = SetupMemWindow(top, mem.backbuffer, "Data", offsets[2], out mem_view);
            SetupCpuWindow(top, cpu.backbuffer_left, cpu.backbuffer_right);
            SetupConsoleWindow(top, io_buffer);

            var usage_label = new Label(
                1, top.Frame.Height-2,
                string.Format("Memory: {0} bytes", vmx.MemoryUsage)
            );
            top.Add(usage_label);

            var sw = new Stopwatch();
            int instruction_count = 0;
            double cpu_time = 0;
            bool real_stack = false;

            var appstate = Application.Begin(top);
            vmx.Running  = true;

            Application.Iteration += (s, e) => {
                switch (state) {
                    case RenderState.CONTINUOUS: break;
                    case RenderState.PAUSE: return;
                    case RenderState.STEP: {
                        var input = Console.ReadKey();

                        switch (input.Key) {
                            case ConsoleKey.Escape:
                                // Close
                                state = RenderState.HALT;
                                Application.End(appstate);
                                return;
                            case ConsoleKey.P:
                                // Pause execution, free input
                                state = RenderState.PAUSE;
                                return;
                            case ConsoleKey.Enter:
                                // Execute without input
                                state = RenderState.CONTINUOUS;
                                break;
                            case ConsoleKey.R: {
                                // Reset virtual machine
                                vmx.Reset();
                                vmx.Running = true;

                                Write("Time: {0:0.0000}ms ({1} cycles)", cpu_time,
                                instruction_count);
                                instruction_count = 0;
                                cpu_time = 0;

                                stk.backbuffer.Clear();
                                hea.backbuffer.Clear();
                                mem.backbuffer.Clear();
                                break;
                            }
                            case ConsoleKey.M: {
                                // Shift memory window locations
                                ShiftOffsets();
                                RedrawLayout(top);
                                Application.Refresh();
                                return;
                            }
                            case ConsoleKey.L: {
                                // Change main layout
                                int layout_index = (int)layout + 1;

                                if (layout_index < 0) {
                                    layout_index = 2;
                                } else if (layout_index > 2) {
                                    layout_index = 0;
                                }

                                layout = (Layout)layout_index;
                                RedrawLayout(top);
                                Application.Refresh();
                                return;
                            }
                            case ConsoleKey.S: {
                                // Change stack viewing mode
                                real_stack = !real_stack;
                                return;
                            }
                            default: break;
                        }
                        break;
                    }
                }

                if (!vmx.Running) {
                    state = RenderState.STEP;
                    return;
                }

                sw.Restart();
                vmx.Advance();
                sw.Stop();

                double time = (double)sw.ElapsedTicks / 10_000;
                cpu_time += time;
                instruction_count++;

                src.Draw(src_view, time);
                cpu.Draw();

                mem.Draw((int)vmx.cpu.dat, (int)vmx.cpu.heap);
                hea.Draw((int)vmx.cpu.heap, (int)vmx.cpu.hp);
                usage_label.Text = string.Format("Memory: {0} bytes", vmx.MemoryUsage);

                if (real_stack) {
                    stk.Draw((int)vmx.cpu.hp+4, vmx.memory.Length);
                } else {
                    uint esp = vmx.cpu.registers[Registers.ESP];
                    stk.Draw((int)vmx.ConvertToRealAddress(esp), vmx.memory.Length);
                }

                Application.Refresh();
            };

            if (top != null) {
                Application.RunLoop(appstate);
            }
        }

        Window SetupSrcWindow(Toplevel top, List<string> buffer) {
            int mid_y = top.Frame.Height / 4;
            int mid_x = top.Frame.Width / 3;
            var rect = GetSrcWindowLayout(top);

            var win   = new Window(rect, string.Format("Assemby: {0}", app_name));
            src_view  = new ListView(buffer);

            top.Add(win);
            win.Add(src_view);
            return win;
        }

        Window SetupCpuWindow(Toplevel top, List<string> left, List<string> right) {
            int mid_y = top.Frame.Height / 4;
            int third_x = top.Frame.Width / 6;
            var rect  = new Rect((third_x*4)+2, (mid_y*3)+3, third_x*2+2, mid_y-2);
            var win   = new Window(rect, "CPU");

            var rect_left  = new Rect(0, 0, third_x, 5);
            var rect_right = new Rect(third_x, 0, third_x, 5);

            cpu_views  = new ListView[] {
                new ListView(rect_left, left),
                new ListView(rect_right, right)
            };

            top.Add(win);
            win.Add(cpu_views[0]);
            win.Add(cpu_views[1]);
            return win;
        }

        Window SetupMemWindow(Toplevel top, List<string> buffer, string name, int offset, out ListView view) {
            var rect  = GetMemWindowLayout(top, offset);
            var win   = new Window(rect, name);
            view      = new ListView(buffer);

            top.Add(win);
            win.Add(view);
            return win;
        }

        Window SetupConsoleWindow(Toplevel top, List<string> buffer) {
            int mid_y = top.Frame.Height / 4;
            int third_x = top.Frame.Width / 6;
            var rect  = new Rect(0, (mid_y*3)+3, third_x*4+2, mid_y-2);

            var win   = new Window(rect, "Console");
            var view  = new ListView(buffer);

            top.Add(win);
            win.Add(view);
            return win;
        }

        void ShiftOffsets() {
            for (int i = 0; i < offsets.Length; i++) {
                offsets[i]++;
                if (offsets[i] < 0) {
                    offsets[i] = offsets.Length - 1;
                } else if (offsets[i] >= offsets.Length) {
                    offsets[i] = 0;
                }
            }
        }

        void RedrawLayout(Toplevel top) {
            src_win.Frame = GetSrcWindowLayout(top);
            stk_win.Frame = GetMemWindowLayout(top, offsets[0]);
            hea_win.Frame = GetMemWindowLayout(top, offsets[1]);
            mem_win.Frame = GetMemWindowLayout(top, offsets[2]);

            Rect rect;
            rect = stk_view.Frame;
            rect.Height = stk_win.Frame.Height - 2;
            rect.Width  = stk_win.Frame.Width - 2;
            stk_view.Frame = rect;

            rect = hea_view.Frame;
            rect.Height = hea_win.Frame.Height - 2;
            rect.Width  = hea_win.Frame.Width - 2;
            hea_view.Frame = rect;

            rect = mem_view.Frame;
            rect.Height = mem_win.Frame.Height - 2;
            rect.Width  = hea_win.Frame.Width - 2;
            mem_view.Frame = rect;
        }

        Rect GetSrcWindowLayout(Toplevel top) {
            int mid_y = top.Frame.Height / 4;
            int mid_x = top.Frame.Width / 3;

            switch (layout) {
                case Layout.WIDE: return new Rect(0, 1, top.Frame.Width, mid_y*2);
                case Layout.TINY: return new Rect(0, 1, mid_x*2, mid_y*2);
                default:
                case Layout.TALL: return new Rect(0, 1, mid_x*2, mid_y*3+2);
            }
        }

        Rect GetMemWindowLayout(Toplevel top, int offset) {
            int mid_y = top.Frame.Height / 4;
            int mid_x = top.Frame.Width / 3;

            Rect rect;

            switch (layout) {
                case Layout.WIDE: {
                    rect = new Rect(offset*mid_x, (mid_y*2)+1, mid_x, mid_y+2);
                    if (offset == 2) {
                        // Fixes width / 3 rounding error
                        rect.Width += 1;
                    }
                    break;
                }
                case Layout.TINY: {
                    if (offset == 2) {
                        rect = new Rect(offset*mid_x, 1, mid_x+1, mid_y*3+2);
                    } else {
                        rect = new Rect(offset*mid_x, (mid_y*2)+1, mid_x, mid_y+2);
                    }
                    break;
                }
                default:
                case Layout.TALL: {
                    rect = new Rect(mid_x*2, offset*mid_y+1, mid_x+1, mid_y);
                    if (offset == 2) {
                        rect.Height += 2;
                    }
                    break;
                }
            }
            return rect;
        }
    }
}
