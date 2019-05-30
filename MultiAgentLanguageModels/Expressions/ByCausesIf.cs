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
    }

    public class ByCauses : ByCausesIf
    {
        public ByCauses(Action action, AgentsList agents, LogicExpression result)
            :base(action, agents, result, new True())
        {
        }
    }

    public class CausesIf : ByCausesIf
    {
        public CausesIf(Action action, LogicExpression result, LogicExpression condition)
            : base(action, null, result, condition)
        {
        }
    }

    public class Causes : ByCausesIf
    {
        public Causes(Action action, LogicExpression result)
            : base(action, null, result, null)
        {
        }
    }

    public class ImpossibleByIf : ByCausesIf
    {
        public ImpossibleByIf(Action action, AgentsList agents, LogicExpression condition) : base(action, agents, new False(), condition)
        {
        }
    }

    public class ImpossibleBy : ByCausesIf
    {
        public ImpossibleBy(Action action, AgentsList agents) : base(action, agents, new False(), new True())
        {
        }
    }

    public class ImpossibleIf : ByCausesIf
    {
        public ImpossibleIf(Action action, LogicExpression condition) : base(action, null, new False(), condition)
        {
        }
    }
}