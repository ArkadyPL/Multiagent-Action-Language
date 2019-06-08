using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;

namespace MultiAgentLanguageGUI
{
    public class Output
    {
        public static TextBox Target { get; set; } = null;

        public static void PrintSeparator()
        {
            Print("-------------------------------");
        }

        public static void PrintNLine()
        {
            Print("\n");
        }

        public static void Print(string text)
        {
            if(Target != null)
                Target.Text += text + '\n';
        }

        public static void Clear()
        {
            if(!(Target is null))
                Target.Text = "";
        }
    }
}
