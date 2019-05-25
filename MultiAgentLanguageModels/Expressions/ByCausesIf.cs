using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels.Expressions
{
    public class ByCausesIf : Expression
    {
        public Action A { get; }

        public AgentsList G { get; }

        public LogicExpression Pi { get; }

        public LogicExpression Alpha { get; }

        public ByCausesIf(Action action, AgentsList agents, LogicExpression result, LogicExpression condition)
        {
            A = action;
            G = agents;
            Pi = condition;
            Alpha = result;
        }

        public override string ToProlog()
        {
            var possibleAlpha = Alpha.EvaluateLogicExpression().ToListOfStrings();
            var possiblePi = Pi.EvaluateLogicExpression().ToListOfStrings();
            var results = possibleAlpha.Select(
                alpha => possiblePi.Select(pi =>
                $"by_causes_if({A.ToProlog()}, {G.ToProlog()}, {alpha}, {pi}).")
                .Aggregate((a,b) => a+"\n"+b)).Aggregate((a,b) => a+"\n"+b);
            return results;
        }
    }

    public class ByCauses : ByCausesIf
    {
        public ByCauses(Action action, AgentsList agents, LogicExpression result)
            :base(action, agents, result, null)
        {  
        }

        public override string ToProlog()
        {
            var possibleAlpha = Alpha.EvaluateLogicExpression().ToListOfStrings();
            var results = possibleAlpha.Select(
                alpha => $"by_causes({A.ToProlog()}, {G.ToProlog()}, {alpha}).")
                .Aggregate((a, b) => a + "\n" + b);
            return results;
        }
    }

    public class CausesIf : ByCausesIf
    {
        public CausesIf(Action action, AgentsList agents, LogicExpression result, LogicExpression condition) : base(action, agents, result, condition)
        {
        }

        public override string ToProlog()
        {
            throw new NotImplementedException();
        }
    }

    public class ImpossibleByIf : ByCausesIf
    {
        public ImpossibleByIf(Action action, AgentsList agents, LogicExpression condition) : base(action, agents, null, condition)
        {
        }

        public override string ToProlog()
        {
            var possiblePi = Pi.EvaluateLogicExpression().ToListOfStrings();
            var results = possiblePi.Select(pi =>
                $"impossible_by_if({A.ToProlog()}, {G.ToProlog()}, {pi}).")
                .Aggregate((a, b) => a + "\n" + b);
            return results;
        }
    }

    public class ImpossibleBy : ByCausesIf
    {
        public ImpossibleBy(Action action, AgentsList agents) : base(action, agents, null, null)
        {
        }

        public override string ToProlog()
        {
            return $"impossible_by({A.ToProlog()}, {G.ToProlog()}).";
        }
    }

    public class ImpossibleIf : ByCausesIf
    {
        public ImpossibleIf(Action action, LogicExpression condition) : base(action, null, null, condition)
        {
        }

        public override string ToProlog()
        {
            return Pi.EvaluateLogicExpression().ToListOfStrings().Select(pi => $"impossible_if({A.ToProlog()}, {pi}).").Aggregate((a, b) => a + "\n" + b);
        }
    }
}