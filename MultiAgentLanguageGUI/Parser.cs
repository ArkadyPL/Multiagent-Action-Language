﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;

namespace MultiAgentLanguageGUI
{
    public class ParserState
    {
        public List<Token> TokenList { get; set; }
        public Dictionary<string, MultiAgentLanguageModels.Agent> Agent { get; set; }
        public Dictionary<string, MultiAgentLanguageModels.Fluent> Fluent { get; set; }
        public Dictionary<string, MultiAgentLanguageModels.Action> Action { get; set; }
        public Dictionary<string, MultiAgentLanguageModels.Expressions.Noninertial> Noninertial { get; set; }
        public List<MultiAgentLanguageModels.Expressions.Expression> Expression { get; set; }

        public Query Q { get; set; }

        public ExpressionsList Story
        { get
            {
                var s = new ExpressionsList(Agent.Values, Fluent.Values);
                s.AddRange(Noninertial.Values);
                s.AddRange(Expression);
                return s;
            }
        }

        public ParserState(List<Token> tokenList)
        {
            TokenList = tokenList;
            Agent = new Dictionary<string, MultiAgentLanguageModels.Agent>();
            Fluent = new Dictionary<string, MultiAgentLanguageModels.Fluent>();
            Action = new Dictionary<string, MultiAgentLanguageModels.Action>();
            Noninertial = new Dictionary<string, MultiAgentLanguageModels.Expressions.Noninertial>();
            Expression = new List<MultiAgentLanguageModels.Expressions.Expression>();
        }

        public Token PopToken()
        {
            if (TokenList.Count == 0)
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

        public bool NameAvailable(Token fallbackToken, string name)
        {
            if (Tokenizer.Keyword.ContainsKey(name))
            {
                fallbackToken.ThrowException("Attempting to use a special keyword as a name.");
            }
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

        public string NoninertialList()
        {
            string output = "";
            foreach (string key in Noninertial.Keys)
            {
                output += key + ", ";
            }
            return output;
        }

        public string ExpressionList()
        {
            string output = "";
            foreach (var exp in Expression)
            {
                //output += exp.ToProlog() + "\n";
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
            if (state.NameAvailable(a,a.Name))
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
            if (state.NameAvailable(a,a.Name))
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
            if (state.NameAvailable(a,a.Name))
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
            if (state.Fluent.ContainsKey(n.Name))
            {
                state.AddNoninertial(n.Name);
            }
            else
            {
                firstToken.ThrowException("Attempting to use non declared fluent in noninertial");
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
            Token close = state.PeepToken();
            if (close == null) open.ThrowException("Expected empty list of agents '[]' or agent name.");
            if(close.Name == "]")
            {
                state.PopToken();
                return al;
            }
            while (state.TokenList.Count > 0)
            {
                Token t = state.PopToken();
                if (state.Agent.ContainsKey(t.Name))
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
            if (correctSyntax)
            {
                return al;
            }
            return null;
        }

        public static LogicElement EntryC1(ParserState state)
        {
            Token a;
            a = state.PopToken();
            if (a.Type != TokenType.Operator || a.Name != "[")
            {
                a.ThrowException("Expected '[' at the beginning of a logic expression.");
            }
            LogicElement c = C1(state);
            a = state.PopToken();
            if (a.Type != TokenType.Operator || a.Name != "]")
            {
                a.ThrowException("Expected ']' at the end of a logic expression.");
            }
            return c;
        }

        public static LogicElement C1(ParserState state)
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
                LogicElement inside = C1(state);
                Token close = state.PopToken();
                if (close == null || close.Name != ")")
                {
                    t.ThrowException("No closing brackets");
                }
                return inside;
            }
            else if (t.Name == "~")
            {
                Token name = state.PopToken();
                if (!state.Fluent.ContainsKey(name.Name) && !state.Noninertial.ContainsKey(name.Name))
                    name.ThrowException("Expected fluent name");
                Fluent f = new Fluent(name.Name);
                Not retVal = new Not(f);
                return retVal;
            }
            else if (state.Fluent.ContainsKey(t.Name) || state.Noninertial.ContainsKey(t.Name))
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
            else if (t.Type != TokenType.Keyword && !state.Action.ContainsKey(t.Name))
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
            else if (t.Type != TokenType.Keyword && !state.Action.ContainsKey(t.Name))
            {
                t.ThrowException("Expected keyword token or action.");
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
            else if (t.Type != TokenType.Keyword && !state.Action.ContainsKey(t.Name))
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
            else if (t.Type == TokenType.Operator)
            {
                return null;
            }
            else if (t.Type != TokenType.Keyword && !state.Action.ContainsKey(t.Name))
            {
                t.ThrowException("Expected keyword token.");
            }
            return null;
        }

        public static void ParseKeyword(ParserState state, Token firstToken)
        {
            switch (firstToken.Name)
            {
                case "initially":
                    LogicElement le = EntryC1(state);
                    Initially st = new Initially(le);
                    state.Expression.Add(st);
                    break;
                case "noninertial":
                    ParseNoninertial(state, firstToken);
                    break;
                case "by":
                    Token action = state.TokenList[state.TokenList.Count - 1];
                    state.TokenList.RemoveAt(state.TokenList.Count - 1);
                    AgentsList al = GetAgentList(state);
                    if (al == null)
                    {
                        firstToken.ThrowException("Expected ']' at the end of agents list.");
                    }
                    Token t = state.PopToken();
                    if (t == null) firstToken.ThrowException("Expected 'causes' or 'releases'.");

                    LogicElement result = null;
                    if (t.Name == "releases")
                    {
                        Token fT = state.PopToken();
                        if (fT == null)
                            firstToken.ThrowException("Expected fluent after release.");
                        else if (!state.Fluent.ContainsKey(fT.Name)) firstToken.ThrowException("Attempting to use undeclared fluent.");
                        result = state.Fluent[fT.Name];
                    }
                    else
                    if (t.Name == "causes")
                    {
                        result = EntryC1(state);
                    }
                    else
                    {
                        t.ThrowException("Expected 'causes' or 'releases'.");
                    }

                    if (t.Name == "releases" && (result is Fluent) == false)
                    {
                        t.ThrowException("Expected fluent after release.");
                    }

                    LogicElement condition = null;
                    Token if_token = state.PeepToken();
                    if (if_token != null && if_token.Name == "if")
                    {
                        state.PopToken();
                        condition = EntryC1(state);
                    }
                    if (t.Name == "causes")
                    {
                        if (condition == null)
                        {
                            state.Expression.Add(new MultiAgentLanguageModels.Expressions.ByCauses(
                                new MultiAgentLanguageModels.Action(action.Name),
                                al, result));
                        }
                        else
                        {
                            state.Expression.Add(new MultiAgentLanguageModels.Expressions.ByCausesIf(
                                new MultiAgentLanguageModels.Action(action.Name),
                                al, result, condition));
                        }
                    }
                    if (t.Name == "releases")
                    {
                        if (condition == null)
                        {
                            state.Expression.Add(new MultiAgentLanguageModels.Expressions.ByReleases(
                                new MultiAgentLanguageModels.Action(action.Name),
                                al, (Fluent)result));
                            state.Expression.Add(new MultiAgentLanguageModels.Expressions.ByCauses(
                                new MultiAgentLanguageModels.Action(action.Name),
                                al, new Or(result, new Not(result))));
                        }
                        else
                        {
                            state.Expression.Add(new MultiAgentLanguageModels.Expressions.ByReleasesIf(
                                new MultiAgentLanguageModels.Action(action.Name),
                                al, (Fluent)result, condition));
                            state.Expression.Add(new MultiAgentLanguageModels.Expressions.ByCausesIf(
                                new MultiAgentLanguageModels.Action(action.Name),
                                al, new Or(result, new Not(result)), condition));
                        }
                    }
                    break;
                case "causes":
                    MultiAgentLanguageModels.Action act =
                        new MultiAgentLanguageModels.Action(state.TokenList[state.TokenList.Count - 1].Name);
                    state.TokenList.RemoveAt(state.TokenList.Count - 1);
                    LogicElement effect = EntryC1(state);
                    Token if_exp = state.PeepToken();
                    if (if_exp != null && if_exp.Name == "if")
                    {
                        state.PopToken();
                        LogicElement con = EntryC1(state);
                        state.Expression.Add(new CausesIf(act, effect, con));
                    }
                    else
                    {
                        state.Expression.Add(new Causes(act, effect));
                    }
                    break;
                case "releases":
                    MultiAgentLanguageModels.Action act1 =
                        new MultiAgentLanguageModels.Action(state.TokenList[state.TokenList.Count - 1].Name);
                    state.TokenList.RemoveAt(state.TokenList.Count - 1);
                    Token eff1 = state.PopToken();
                    if (eff1 == null)
                        firstToken.ThrowException("Expected fluent after release.");
                    else if (!state.Fluent.ContainsKey(eff1.Name)) firstToken.ThrowException("Attempting to use undeclared fluent.");
                    Token if_expr = state.PeepToken();
                    if (if_expr != null && if_expr.Name == "if")
                    {
                        state.PopToken();
                        LogicElement con = EntryC1(state);
                        state.Expression.Add(new ReleasesIf(act1, state.Fluent[eff1.Name], con));
                        state.Expression.Add(new CausesIf(act1, new Or(state.Fluent[eff1.Name], new Not(state.Fluent[eff1.Name])), con));
                    }
                    else
                    {
                        state.Expression.Add(new Releases(act1, state.Fluent[eff1.Name]));
                        state.Expression.Add(new Causes(act1, new Or(state.Fluent[eff1.Name], new Not(state.Fluent[eff1.Name]))));
                    }
                    break;
                case "if":
                    firstToken.ThrowException("Unexpected 'if' token.");
                    break;
                case "impossible":
                    Token token = state.PopToken();
                    if (token == null) firstToken.ThrowException("Expected action name.");
                    if (!state.Action.ContainsKey(token.Name)) token.ThrowException("Unknown action name.");
                    MultiAgentLanguageModels.Action ac = new MultiAgentLanguageModels.Action(token.Name);
                    Token key = state.PopToken();
                    if (key == null) firstToken.ThrowException("Expected 'by' or 'if' token.");
                    AgentsList agentsList = null;
                    if (key.Name == "by")
                    {
                        agentsList = GetAgentList(state);
                        Token cond_st = state.PeepToken();
                        if (cond_st == null || cond_st.Name != "if")
                        {
                            state.Expression.Add(new ImpossibleBy(ac, agentsList));
                        }
                        else
                        {
                            state.PopToken();
                            LogicElement c = EntryC1(state);
                            state.Expression.Add(new ImpossibleByIf(ac, agentsList, c));
                        }
                    }
                    else if (key.Name == "if")
                    {
                        //Token cond_st = state.PopToken();
                        //if (cond_st == null || cond_st.Name != "if")
                        //key.ThrowException("Expected if after the list of agents.");
                        LogicElement c = EntryC1(state);
                        state.Expression.Add(new ImpossibleIf(ac, c));
                    }
                    else
                    {
                        firstToken.ThrowException("Expected 'by' or 'if' token.");
                    }
                    break;
                case "always":
                    LogicElement cond = EntryC1(state);
                    state.Expression.Add(new Always(cond));
                    break;
                case "not":
                    Token act2 = state.TokenList[state.TokenList.Count - 1];
                    MultiAgentLanguageModels.Action actt = new MultiAgentLanguageModels.Action(act2.Name);
                    state.TokenList.RemoveAt(state.TokenList.Count - 1);
                    Token by = state.PopToken();
                    if (by == null || by.Name != "by")
                    {
                        firstToken.ThrowException("Expected 'by' after 'not'.");
                    }
                    AgentsList agents = GetAgentList(state);
                    Token if_st = state.PeepToken();
                    if (if_st != null && if_st.Name == "if")
                    {
                        state.PopToken();
                        condition = EntryC1(state);
                        foreach(Agent a in agents)
                        {
                            state.Expression.Add(new ImpossibleByIf(actt, new AgentsList() { a }, condition));
                            Output.Print($"{actt.Name} not by {a.Name} under cond {condition.ToString()}");
                        }
                    }
                    else
                    {
                        foreach (Agent a in agents)
                        {
                            state.Expression.Add(new ImpossibleBy(actt, new AgentsList() { a }));
                        }
                    }
                    break;
                case "after":
                    LogicElement observable = EntryC1(state);
                    Token aft = state.PopToken();
                    if (aft == null || aft.Name != "after")
                    {
                        firstToken.ThrowException("Expected 'after' after logic expression.");
                    }
                    Instruction instr = GetInstructions(state, aft);
                    After after_exp = new After(observable, instr);
                    state.Expression.Add(after_exp);
                    break;
                case "observable":
                    LogicElement obs = EntryC1(state);
                    Token after = state.PopToken();
                    if (after == null || after.Name != "after")
                    {
                        firstToken.ThrowException("Expected 'after' after logic expression.");
                    }
                    Instruction inst = GetInstructions(state, after);
                    ObservableAfter obsAfter = new ObservableAfter(obs, inst);
                    state.Expression.Add(obsAfter);
                    break;
            }
        }

        private static Instruction GetInstructions(ParserState state, Token previous)
        {
            Instruction inst = new Instruction();
            do
            {
                if (state.PeepToken().Name == ",") state.PopToken();
                Token open = state.PopToken();
                if (open == null || open.Name != "(")
                {
                    previous.ThrowException("Expected program format: (A1,G1),(A2,G2),...,(An,Gn)");
                }
                Token a = state.PopToken();
                if (a == null) open.ThrowException("Expected action");
                if (a.Name == ")") return inst;
                if(state.Action.ContainsKey(a.Name) == false)
                {
                    open.ThrowException("Expected action");
                }
                Token comma = state.PopToken();
                if (comma == null || comma.Name != ",")
                {
                    a.ThrowException("Comma should separate an action and a set of agents");
                }
                MultiAgentLanguageModels.Action a1 = new MultiAgentLanguageModels.Action(a.Name);
                AgentsList g = GetAgentList(state);
                Token close = state.PopToken();
                if (close == null || close.Name != ")")
                {
                    a.ThrowException("Expected )");
                }
                inst.Add(new Tuple<MultiAgentLanguageModels.Action, AgentsList>(a1, g));
            } while (state.PeepToken() != null && state.PeepToken().Name == ",");
            return inst;
        }

        private static List<MultiAgentLanguageModels.Action> GetActionList(ParserState state)
        {
            List<MultiAgentLanguageModels.Action> actions = new List<MultiAgentLanguageModels.Action>();
            do
            {
                if (state.PeepToken().Name == ",") state.PopToken();               
                Token a = state.PopToken();
                if (a == null || state.Action.ContainsKey(a.Name) == false)
                {
                    throw new Exception("Expected action.");
                }
                actions.Add(new MultiAgentLanguageModels.Action(a.Name));
            } while (state.PeepToken() != null && state.PeepToken().Name == ",");
            return actions;
        }

        public static Query ParseQuery(List<Token> tokenList, ParserState story)
        {
            ParserState state = new ParserState(tokenList);
            state.Action = story.Action;
            state.Agent = story.Agent;
            state.Noninertial = story.Noninertial;
            state.Fluent = story.Fluent;
            if(tokenList.Count == 0)
            {
                throw new Exception("Empty query");
            }

            Token first = state.PopToken();
            if(first.Name == "necessary" || first.Name == "possibly")
            {
                Token t = state.PopToken();
                Token next = state.PeepToken();
                if (t == null) first.ThrowException("Expected: executable, agents list or logic expression.");
                if(t.Name == "executable") // necessary executable
                {
                    if (state.PeepToken() == null) t.ThrowException("Expected program.");
                    Instruction inst = GetInstructions(state, t);
                    Token from = state.PopToken();
                    if (from == null)
                    {
                        if (first.Name == "necessary") return new NecessaryExecutable(inst);
                        else return new PossiblyExecutable(inst);
                    }
                    if(from.Name != "from") t.ThrowException("Expected from after program.");
                    LogicElement cond = EntryC1(state);
                    if (first.Name == "necessary") return new NecessaryExecutableFrom(inst,cond);
                    else return new PossiblyExecutableFrom(inst,cond);
                }
                else if(state.Agent.ContainsKey(next.Name)) // necessary engaged
                {
                    state.TokenList.Insert(0, t);
                    AgentsList agents = GetAgentList(state);
                    Token engaged = state.PopToken();
                    if (engaged == null || engaged.Name != "engaged") 
                    {
                        t.ThrowException("Expected engaged after agents list.");
                    }
                    Token in_token = state.PopToken();
                    if (in_token == null || in_token.Name != "in")
                    {
                        t.ThrowException("Expected in after engaged.");
                    }
                    Instruction inst = GetInstructions(state,in_token);
                    Token from = state.PopToken();
                    if (from == null)
                    {
                        if (first.Name == "necessary") return new NecessaryEngaged(agents, inst); 
                        else return new PossiblyEngaged(agents, inst); 
                    }
                    if (from.Name != "from") t.ThrowException("Expected from after action list.");
                    LogicElement cond = EntryC1(state);
                    if (first.Name == "necessary") return new NecessaryEngagedFrom(agents, inst, cond); 
                    else return new PossiblyEngagedFrom(agents, inst, cond); 
                }
                else if(next != null && (state.Fluent.ContainsKey(next.Name) || 
                    state.Noninertial.ContainsKey(next.Name) || next.Name == "(" || next.Name == "~")) // necessary value
                {
                    state.TokenList.Insert(0, t);
                    LogicElement result = EntryC1(state);
                    Token after = state.PopToken();
                    if (after == null || after.Name != "after")
                    {
                        t.ThrowException("Expected 'after' after result.");
                    }
                    Instruction inst = GetInstructions(state, t);
                    Token from = state.PopToken();
                    if (from == null)
                    {
                        if (first.Name == "necessary") return new NecessaryAfter(inst,result);
                        else return new PossiblyAfter(inst, result);
                    }
                    if (from.Name != "from") t.ThrowException("Expected from after program.");
                    LogicElement cond = EntryC1(state);
                    if (first.Name == "necessary") return new NecessaryAfterFrom(inst,result,cond);
                    else return new PossiblyAfterFrom(inst, result, cond);
                }
                else
                {
                    throw new Exception("Incorrect query.");
                }
            }
            else
            {
                first.ThrowException("Expected 'necessary' or 'possibly'.");
            }
            return null;
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
                else if (state.Action.ContainsKey(token.Name))
                {
                    Token keyword = state.PopToken();
                    if (keyword.Name == "by" || keyword.Name == "causes" || keyword.Name == "releases" || keyword.Name == "not")
                        state.TokenList.Add(token);
                    ParseKeyword(state, keyword);
                }
                else if (token.Name == "(" || token.Name == "~" || token.Name == "[" ||
                    state.Fluent.ContainsKey(token.Name) || state.Noninertial.ContainsKey(token.Name))
                {
                    state.TokenList.Insert(0, token);
                    Token kw = new Token(token.LineNumber, token.ColumnNumber);
                    kw.Name = "after";
                    ParseKeyword(state, kw);
                }
                //else if(token.n)
                else
                {
                    token.ThrowException("Illegal token at position.");
                }
            }

            return state;
        }
    }
}
