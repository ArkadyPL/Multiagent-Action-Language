using System.Collections.Generic;

namespace MultiAgentLanguageModels.Queries
{
    public class PossiblyEngagedFrom : Query,  IProlog
    {
        public List<Agent> Agents { get; }
        public List<Action> Actions { get; }
        public LogicExpression Condition { get; }

        public PossiblyEngagedFrom(List<Agent> agents, List<Action> actions, LogicExpression condition)
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
        public PossiblyEngaged(List<Agent> agents, List<Action> actions) 
            : base(agents, actions, null)
        {
        }

        public override string ToProlog()
        {
            return $"possibly_engaged({Agents}, {Actions}).";
        }
    }
}