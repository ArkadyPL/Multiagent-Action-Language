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
                "// komentarz" + "\n" +
                "Agent Agent_1;" + "\n" +
                "Agent a2;" + "\n" +
                "Fluent f1; // inny komentarz" + "\n" +
                "Agent a3;" + "\n" +
                "Agent a4 Agent a5 // średniki są ozdobne" + "\n" +
                "Action ac1" + "\n" +
                "Action a1; // tu się wykrzaczy ze względu na duplikat nazwy" + "\n";
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
