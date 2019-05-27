using Microsoft.Win32;
using MultiAgentLanguageModels.Queries;
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
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace MultiAgentLanguageGUI
{
    /// <summary>
    /// Logika interakcji dla klasy MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        ParserState state;
        bool parsed = false;
        public MainWindow()
        {
            InitializeComponent();
            state = new ParserState(null);
            Output.Target = TextBox_Output;

            TextBox_Story.Text =
                "Agent a\n" +
                "Agent b\n" +
                "Agent c\n" +
                "Fluent f1\n" +
                "Fluent f2\n" +
                "Action x\n" +
                "Action y\n" +
                "[f1 && f2] after (x,[a,b]),(y,[b])\n" +
                "initially [f1]\n" +
                "y by [a,b] causes [~f1] if [f2]\n" +
                "y causes [~f1] if [f2]\n" +
                "y by [a,b] causes [~f1]\n" +
                "y causes [f1]\n" +
                "x by [a,b] releases [~f1] if [f2]\n" +
                "x releases [~f1] if [f2]\n" +
                "x by [a,b] releases [~f1]\n" +
                "x causes [f1]\n" +
                "impossible x by [a] if [f1 && f2]\n" +
                "impossible x if [f1 && f2]\n" +
                "always [f1 || f2]\n" +
                "noninertial f3\n" +
                "observable [f3] after (x,[a,b]),(y,[b])";
        }

        private void TextBox_Story_TextChanged(object sender, TextChangedEventArgs e)
        {
            parsed = false;
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
                state = Parser.Parse(list);
                Output.Print($"Created agents: {state.AgentList()}");
                Output.Print($"Created fluents: {state.FluentList()}");
                Output.Print($"Created actions: {state.ActionList()}");
                Output.Print($"Created noninertial fluents: {state.NoninertialList()}\n");
                Output.Print($"Created expressions: {state.ExpressionList()}");
                Output.Print("Done.");
                parsed = true;
            }
            catch(Exception ex)
            {
                Output.Print($"an error occurred\n{ex.ToString()}");
            }
        }

        private void Button_StoryClear_Click(object sender, RoutedEventArgs e)
        {
            TextBox_Story.Text = "";
            parsed = false;
            state = new ParserState(null);
        }

        private void Button_StoryLoad_Click(object sender, RoutedEventArgs e)
        {
            OpenFileDialog saveFileDialog = new OpenFileDialog() { Filter = "MAR Language|*.mar" };
            if (saveFileDialog.ShowDialog() == true)
                TextBox_Story.Text = File.ReadAllText(saveFileDialog.FileName);
        }

        private void Button_StorySave_Click(object sender, RoutedEventArgs e)
        {
            SaveFileDialog saveFileDialog = new SaveFileDialog() { Filter="MAR Language|*.mar" };
            if (saveFileDialog.ShowDialog() == true)
                File.WriteAllText(saveFileDialog.FileName, TextBox_Story.Text);
        }

        private void TextBox_Query_TextChanged(object sender, TextChangedEventArgs e)
        {
            
        }

        private void Button_QueryExecute_Click(object sender, RoutedEventArgs e)
        {
            if(parsed == false)
            {
                Output.Print("First parse the story.");
                return;
            }
            Output.PrintSeparator();
            Output.Print("Attempting a tokenize the query...");
            try
            {
                List<Token> list = Tokenizer.Tokenize(TextBox_Query.Text);
                Output.Print("Tokenize procedure finished without explicit failure.");
                Output.Print("Tokens created:");
                for (int i = 0; i < list.Count; i++)
                {
                    Output.Print($"{i + 1} - {list[i].Type.ToString()}: {list[i].Name}");
                }
                Output.PrintNLine();
                Output.Print("Now attempting to parse the received token list...");
                Query q = Parser.ParseQuerry(list, state);
                state.Q = q;
                foreach(var str in q.ToProlog())
                {
                    Output.Print($"Created query: {str}");
                }
                Output.Print("Done.");
            }
            catch (Exception ex)
            {
                Output.Print($"an error occurred\n{ex.ToString()}");
            }
        }

        private void TextBox_Output_TextChanged(object sender, TextChangedEventArgs e)
        {
            TextBox_Output.ScrollToEnd();
        }
    }
}
