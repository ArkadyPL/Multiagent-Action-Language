using System.Collections.Generic;

namespace MultiAgentLanguageModels.Queries
{
    public class PossiblyEngagedFrom : Query,  IProlog
    {
        public AgentsList Agents { get; }
        public List<Action> Actions { get; }
        public LogicExpression Condition { get; }

        public PossiblyEngagedFrom(AgentsList agents, List<Action> actions, LogicExpression condition)
        {
            Agents = agents;
            Actions = actions;
            Condition = condition;
        }

        public override string ToProlog()
        {
            return $"possibly_engaged_from({Agents.ToProlog()}, {Actions.ToProlog()}, {Condition.ToProlog()}).";
        }
    }

    public class PossiblyEngaged : PossiblyEngagedFrom
    {
        public PossiblyEngaged(AgentsList agents, List<Action> actions)
            : base(agents, actions, LogicExpression.Empty)
        {
        }

        public override string ToProlog()
        {
            return $"possibly_engaged({Agents.ToProlog()}, {Actions.ToProlog()}).";
        }
    }
}