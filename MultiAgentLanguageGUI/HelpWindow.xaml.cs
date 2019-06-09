using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace MultiAgentLanguageGUI
{
    /// <summary>
    /// Logika interakcji dla klasy HelpWindow.xaml
    /// </summary>
    public partial class HelpWindow : Window
    {
        public HelpWindow()
        {
            InitializeComponent();

            try
            {
                TextBox_Help.Text = File.ReadAllText("help.txt");
            }
            catch (IOException e)
            {
                TextBox_Help.Text =
                    "Could not read help.txt. Please ensure the program has been properly unpacked.";
            }
        }

        private void Button_Okay_Click(object sender, RoutedEventArgs e)
        {
            this.Close();
        }
    }
}
