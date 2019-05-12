using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MultiAgentLanguageModels;

namespace MultiAgentLanguageGUI
{
    public class ParserState
    {
        public List<Token> TokenList { get; set; }
        public Dictionary<string, MultiAgentLanguageModels.Agent> Agent { get; set; }
        public Dictionary<string, MultiAgentLanguageModels.Fluent> Fluent { get; set; }
        public Dictionary<string, MultiAgentLanguageModels.Action> Action { get; set; }

        public ParserState(List<Token> tokenList)
        {
            TokenList = tokenList;
            Agent = new Dictionary<string, MultiAgentLanguageModels.Agent>();
            Fluent = new Dictionary<string, MultiAgentLanguageModels.Fluent>();
            Action = new Dictionary<string, MultiAgentLanguageModels.Action>();
        }

        public Token PopToken()
        {
            if(TokenList.Count == 0)
            {
                return null;
            }
            Token a = TokenList[0];
            TokenList.RemoveAt(0);
            return a;
        }

        public void AddAgent(string name)
        {
            Agent.Add(name, new MultiAgentLanguageModels.Agent(name));
        }
        public void AddFluent(string name)
        {
            Fluent.Add(name, new MultiAgentLanguageModels.Fluent(name));
        }
        public void AddAction(string name)
        {
            Action.Add(name, new MultiAgentLanguageModels.Action(name));
        }

        public bool NameAvailable(string name)
        {
            if (Agent.ContainsKey(name))
                return false;
            if (Fluent.ContainsKey(name))
                return false;
            if (Action.ContainsKey(name))
                return false;
            return true;
        }

        public string AgentList()
        {
            string output = "";
            foreach (string key in Agent.Keys)
            {
                output += key + ", ";
            }
            return output;
        }
        public string FluentList()
        {
            string output = "";
            foreach (string key in Fluent.Keys)
            {
                output += key + ", ";
            }
            return output;
        }
        public string ActionList()
        {
            string output = "";
            foreach (string key in Action.Keys)
            {
                output += key + ", ";
            }
            return output;
        }
    }

    public class Parser
    {
        public static readonly Dictionary<TokenType, System.Action<ParserState, Token>> TokenTypeHandle = new Dictionary<TokenType, System.Action<ParserState, Token>>
        {
            { TokenType.Agent, ParseAgent },
            { TokenType.Fluent, ParseFluent },
            { TokenType.Action, ParseAction }
        };

        public static void ParseAgent(ParserState state, Token firstToken)
        {
            Token a = state.PopToken();
            if (a == null)
            {
                firstToken.ThrowException("No agent name");
            }
            if (state.NameAvailable(a.Name))
            {
                state.AddAgent(a.Name);
            }
            else
            {
                firstToken.ThrowException("Attempt to use an already used name");
            }
        }
        public static void ParseFluent(ParserState state, Token firstToken)
        {
            Token a = state.PopToken();
            if (a == null)
            {
                firstToken.ThrowException("No fluent name");
            }
            if (state.NameAvailable(a.Name))
            {
                state.AddFluent(a.Name);
            }
            else
            {
                firstToken.ThrowException("Attempt to use an already used name");
            }
        }
        public static void ParseAction(ParserState state, Token firstToken)
        {
            Token a = state.PopToken();
            if (a == null)
            {
                firstToken.ThrowException("No action name");
            }
            if (state.NameAvailable(a.Name))
            {
                state.AddAction(a.Name);
            }
            else
            {
                firstToken.ThrowException("Attempt to use an already used name");
            }
        }

        public static ParserState Parse(List<Token> tokenList)
        {
            ParserState state = new ParserState(tokenList);

            while (tokenList.Count > 0)
            {
                Token token = state.PopToken();
                if (TokenTypeHandle.ContainsKey(token.Type))
                {
                    Action<ParserState, Token> action = TokenTypeHandle[token.Type];
                    action(state, token);
                }
                else
                {
                    token.ThrowException("Illegal token at position");
                }
            }

            return state;
        }
    }
}
