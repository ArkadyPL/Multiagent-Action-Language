using System;
using System.Collections.Generic;

namespace MultiAgentLanguageModels.Expressions
{
    public class ByCausesIf : Expression
    {
        public Action A { get; }

        public List<Agent> G { get; }

        public LogicExpression Pi { get; }

        public LogicExpression Alpha { get; }

        public ByCausesIf(Action action, List<Agent> agents, LogicExpression result, LogicExpression condition)
        {
            A = action;
            G = agents;
            Pi = condition;
            Alpha = result;
        }

        public override string ToProlog()
        {
            return $"by_causes_if({A.ToProlog()}, {G.ToProlog()}, {Alpha.ToProlog()}, {Pi.ToProlog()}).";
        }
    }

    public class ByCauses : ByCausesIf
    {
        public ByCauses(Action action, List<Agent> agents, LogicExpression result)
            :base(action, agents, result, null)
        {  
        }

        public override string ToProlog()
        {
            return $"by_causes({A.ToProlog()}, {G.ToProlog()}, {Alpha.ToProlog()}).";
        }
    }

    public class CausesIf : ByCausesIf
    {
        public CausesIf(Action action, List<Agent> agents, LogicExpression result, LogicExpression condition) : base(action, agents, result, condition)
        {
        }

        public override string ToProlog()
        {
            throw new NotImplementedException();
        }
    }

    public class ImpossibleByIf : ByCausesIf
    {
        public ImpossibleByIf(Action action, List<Agent> agents, LogicExpression result, LogicExpression condition) : base(action, agents, result, condition)
        {
        }
    }

    public class ImpossibleBy : ByCausesIf
    {
        public ImpossibleBy(Action action, List<Agent> agents, LogicExpression result, LogicExpression condition) : base(action, agents, result, condition)
        {
        }
    }

    public class ImpossibleIf : ByCausesIf
    {
        public ImpossibleIf(Action action, List<Agent> agents, LogicExpression result, LogicExpression condition) : base(action, agents, result, condition)
        {
        }
    }
}