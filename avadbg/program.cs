using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Ava;
using Terminal.Gui;

namespace AvaDebug
{
    class Program
    {
        static void Main(string[] args) {
            Application.Init();
            Console.OutputEncoding = Encoding.UTF8;
            var top = Application.Top;

            string filename;

            if (args.Length > 0) {
                filename = args[0];
            } else {
                filename = "out.bgx";
            }

            var renderer = new AppMain(filename);
            byte[] byte_code = File.ReadAllBytes(filename);

            var vmx = new Script(byte_code);
            vmx.Initialize();

            SysCallbacks.Initialize(8);
            Script.IO.Write = AppMain.Write;

            renderer.Run(top, vmx);
        }
    }
}
