using System;
using System.Collections.Generic;
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
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace MultiAgentLanguageGUI
{
    /// <summary>
    /// Logika interakcji dla klasy MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            Output.Target = TextBox_Output;

            TextBox_Story.Text =
                "Agent a\n" +
                "Agent b\n" +
                "Agent c\n" +
                "Fluent f1\n" +
                "Fluent f2\n" +
                "Action x\n" +
                "Action y\n" +
                "f1 && f2 after (x,[a,b]),(y,[b])\n" +
                "initially f1\n" +
                "x not by [a,b] if f1 && f2" +
                "y not by [b]" +
                "y by [a,b] causes ~f1 if f2\n" +
                "y causes ~f1 if f2\n" +
                "y by [a,b] causes ~f1\n" +
                "y causes f1\n" +
                "x by [a,b] releases ~f1 if f2\n" +
                "x releases ~f1 if f2\n" +
                "x by [a,b] releases ~f1\n" +
                "x causes f1\n" +
                "impossible x by [a] if f1 && f2\n" +
                "impossible x if f1 && f2\n" +
                "always f1 || f2\n" +
                "noninertial f3\n" +
                "observable f3 after (x,[a,b]),(y,[b])";
        }

        private void TextBox_Story_TextChanged(object sender, TextChangedEventArgs e)
        {

        }

        private void Button_StoryParse_Click(object sender, RoutedEventArgs e)
        {
            Output.PrintSeparator();
            Output.Print("Attempting a tokenize test on the current story text...");
            try
            {
                List<Token> list = Tokenizer.Tokenize(TextBox_Story.Text);
                Output.Print("Tokenize procedure finished without explicit failure.");
                Output.Print("Tokens created:");
                for(int i = 0; i < list.Count; i ++)
                {
                    Output.Print($"{i + 1} - {list[i].Type.ToString()}: {list[i].Name}");
                }
                Output.PrintNLine();
                Output.Print("Now attempting to parse the received token list...");
                ParserState parsed = Parser.Parse(list);
                Output.Print($"Created agents: {parsed.AgentList()}");
                Output.Print($"Created fluents: {parsed.FluentList()}");
                Output.Print($"Created actions: {parsed.ActionList()}");
                Output.Print("Done.");
            }
            catch(Exception ex)
            {
                Output.Print($"an error occurred\n{ex.ToString()}");
            }
        }

        private void Button_StoryClear_Click(object sender, RoutedEventArgs e)
        {
            TextBox_Story.Text = "";
        }

        private void Button_StoryLoad_Click(object sender, RoutedEventArgs e)
        {

        }

        private void Button_StorySave_Click(object sender, RoutedEventArgs e)
        {

        }

        private void TextBox_Query_TextChanged(object sender, TextChangedEventArgs e)
        {

        }

        private void Button_QueryExecute_Click(object sender, RoutedEventArgs e)
        {
        }

        private void TextBox_Output_TextChanged(object sender, TextChangedEventArgs e)
        {
            TextBox_Output.ScrollToEnd();
        }
    }
}
