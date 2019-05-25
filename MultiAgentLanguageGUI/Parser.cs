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
        public Dictionary<string, MultiAgentLanguageModels.Expressions.Noninertial> Noninertial { get; set; }

        public ParserState(List<Token> tokenList)
        {
            TokenList = tokenList;
            Agent = new Dictionary<string, MultiAgentLanguageModels.Agent>();
            Fluent = new Dictionary<string, MultiAgentLanguageModels.Fluent>();
            Action = new Dictionary<string, MultiAgentLanguageModels.Action>();
            Noninertial = new Dictionary<string, MultiAgentLanguageModels.Expressions.Noninertial>();
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

        public Token PeepToken()
        {
            if (TokenList.Count == 0)
            {
                return null;
            }
            return TokenList[0];
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

        public void AddNoninertial(string name)
        {
            Fluent f = new Fluent(name);
            Noninertial.Add(name, new MultiAgentLanguageModels.Expressions.Noninertial(f));
        }

        public bool NameAvailable(string name)
        {
            if (Agent.ContainsKey(name))
                return false;
            if (Fluent.ContainsKey(name))
                return false;
            if (Action.ContainsKey(name))
                return false;
            if (Noninertial.ContainsKey(name))
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
            { TokenType.Action, ParseAction },
            { TokenType.Keyword, ParseKeyword }
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

        public static void ParseNoninertial(ParserState state, Token firstToken)
        {
            Token n = state.PopToken();
            if (n == null)
            {
                firstToken.ThrowException("No noninertial fluent name");
            }
            if (state.NameAvailable(n.Name))
            {
                state.AddNoninertial(n.Name);
            }
            else
            {
                firstToken.ThrowException("Attempt to use an already used name");
            }
        }
        public static AgentsList GetAgentList(ParserState state)  
        {
            AgentsList al = new AgentsList();
            Token open = state.PopToken();
            if (open.Name != "[")
            {
                open.ThrowException("Expected '[' at the beginning of agents list.");
            }
            bool correctSyntax = false;
            while(state.TokenList.Count > 0)
            {
                Token t = state.PopToken();
                if(state.Agent.ContainsKey(t.Name))
                {
                    al.Add(new Agent(t.Name));
                }
                else
                {
                    t.ThrowException("Agent name doesn't exist.");
                }
                if (state.TokenList.Count == 0) return null;

                Token x = state.PopToken();
                if (x.Name == "]")
                {
                    correctSyntax = true;
                    break;
                }
                if (x.Name != ",")
                {
                    x.ThrowException("',' should separate agents' names.");
                }
            }
            if(correctSyntax)
            {
                return al;
            }
            return null;
        }

        public static LogicElement C1 (ParserState state)
        {
            LogicElement fluent = C2(state);
            if (fluent == null) return null;
            LogicElement exp = C1prime(state);
            if (exp != null)
            {
                exp.Left = fluent;
                return exp;
            }
            return fluent;
        }

        public static LogicElement C2(ParserState state)
        {
            LogicElement fluent = C3(state);
            if (fluent == null) return null;
            LogicElement exp = C2prime(state);
            if (exp != null)
            {
                exp.Left = fluent;
                return exp;
            }
            return fluent;
        }

        public static LogicElement C3(ParserState state)
        {
            LogicElement fluent = C4(state);
            if (fluent == null) return null;
            LogicElement exp = C3prime(state);
            if (exp != null)
            {
                exp.Left = fluent;
                return exp;
            }
            return fluent;
        }

        public static LogicElement C4(ParserState state)
        {
            LogicElement fluent = C5(state);
            if (fluent == null) return null;
            LogicElement exp = C4prime(state);
            if (exp != null)
            {
                exp.Left = fluent;
                return exp;
            }
            return fluent;
        }

        public static LogicElement C5(ParserState state)
        {
            Token t = state.PopToken();
            if (t.Name == "(")
            {
                return C1(state);
            }
            else if (t.Name == "~")
            {
                Token name = state.PopToken();
                if (!state.Fluent.ContainsKey(name.Name)) name.ThrowException("Expected fluent name");
                Fluent f = new Fluent(name.Name);
                f.Value = false;
                return f;
            }
            else if (state.Fluent.ContainsKey(t.Name))
            {
                Fluent f = new Fluent(t.Name);
                f.Value = true;
                return f;
            }
            else
            {
                t.ThrowException("Error in logical expression. Mismatched bracekts or operator in wrong places.");
            }
            return null;
        }

        public static LogicElement C1prime(ParserState state)
        {
            Token t = state.PeepToken();
            if (t == null) return null;
            if (t.Name == "&&")
            {
                state.PopToken();
                LogicElement exp = C2(state);
                if (exp.Right == null) exp.Right = C1prime(state);
                And and = new And(null, exp);
                return and;
            }
            else if (t.Type == TokenType.Operator)
            {
                return null;
            }
            else if (t.Type != TokenType.Keyword)
            {
                t.ThrowException("Expected keyword token.");
            }
            return null;
            
        }
        public static LogicElement C2prime(ParserState state)
        {
            Token t = state.PeepToken();
            if (t == null) return null;
            if (t.Name == "||")
            {
                state.PopToken();
                LogicElement exp = C3(state);
                if (exp.Right == null) exp.Right = C2prime(state);
                Or or = new Or(null, exp);
                return or;
            }
            else if (t.Type == TokenType.Operator)
            {
                return null;
            }
            else if (t.Type != TokenType.Keyword)
            {
                t.ThrowException("Expected keyword token.");
            }
            return null;
        }

        public static LogicElement C3prime(ParserState state)
        {
            Token t = state.PeepToken();
            if (t == null) return null;
            if (t.Name == "<->")
            {
                state.PopToken();
                LogicElement exp = C4(state);
                if (exp.Right == null) exp.Right = C3prime(state);
                Iff iff = new Iff(null, exp);
                return iff;
            }
            else if (t.Type == TokenType.Operator)
            {
                return null;
            }
            else if (t.Type != TokenType.Keyword)
            {
                t.ThrowException("Expected keyword token.");
            }
            return null;
        }

        public static LogicElement C4prime(ParserState state)
        {
            Token t = state.PeepToken();
            if (t == null) return null;
            if (t.Name == "->")
            {
                state.PopToken();
                LogicElement exp = C5(state);
                if (exp.Right == null) exp.Right = C4prime(state);
                If if_exp = new If(null, exp);
                return if_exp;
            }
            else if(t.Type == TokenType.Operator)
            {
                return null;
            }
            else if (t.Type != TokenType.Keyword)
            {
                t.ThrowException("Expected keyword token.");
            }
            return null;
        }
    
        public static void ParseKeyword(ParserState state, Token firstToken)
        {
            switch(firstToken.Name)
            {
                case "initially":
                    break;
                case "noninertial":
                    ParseNoninertial(state, firstToken);
                    break;
                case "by":                    
                    AgentsList al = GetAgentList(state);
                    if(al == null)
                    {
                        firstToken.ThrowException("Expected ']' at the end of agents list.");
                    }
                    Token t = state.PopToken();
                    if (t == null) firstToken.ThrowException("Expected 'causes' or 'releases'");
                    if(t.Name != "causes" && t.Name != "releases")
                    {
                        t.ThrowException("Expected 'causes' or 'releases'");
                    }
                    LogicElement cause = C1(state);

                    break;
                case "causes":
                    break;
                case "releases":
                    break;
                case "if":
                    break;
                case "impossible":
                    break;
                case "always":
                    break;
                case "not":
                    break;
                case "after":
                    break;
                case "observable":
                    break;
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
                else if(state.Action.ContainsKey(token.Name))
                {
                    Token keyword = state.PopToken();
                    ParseKeyword(state, keyword);
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
