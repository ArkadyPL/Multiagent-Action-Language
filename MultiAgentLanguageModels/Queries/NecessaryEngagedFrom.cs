﻿using System.Collections.Generic;

namespace MultiAgentLanguageModels.Queries
{
    public class NecessaryEngagedFrom : Query, IProlog
    {
        public AgentsList Agents { get; }
        public List<Action> Actions { get; }
        public LogicExpression Condition { get; }

        public NecessaryEngagedFrom(AgentsList agents, List<Action> actions, LogicExpression condition)
        {
            Agents = agents;
            Actions = actions;
            Condition = condition;
        }

        public override string ToProlog()
        {
            return $"necessary_engaged_from({Agents.ToProlog()}, {Actions.ToProlog()}, {Condition.ToProlog()}).";
        }
    }

    public class NecessaryEngaged : NecessaryEngagedFrom
    {
        public NecessaryEngaged(AgentsList agents, List<Action> actions)
            : base(agents, actions, LogicExpression.Empty)
        {
        }

        public override string ToProlog()
        {
            return $"necessary_engaged({Agents.ToProlog()}, {Actions.ToProlog()}).";
        }
    }
}