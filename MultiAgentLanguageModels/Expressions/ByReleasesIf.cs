using System.Linq;

namespace MultiAgentLanguageModels.Expressions
{
    public class ByReleasesIf : Expression
    {
        public Action Action { get; }
        public AgentsList Agents { get; }
        public Fluent Fluent { get; }
        public LogicExpression Condition { get; }
        public ByReleasesIf(Action action, AgentsList agents, Fluent fluent, LogicExpression condition)
        {
            Action = action;
            Agents = agents;
            Fluent = fluent;
            Condition = condition;
        }

        public override string ToProlog()
        {
            return Condition.EvaluateLogicExpression()
                .ToListOfStrings()
                .Select(pi => $"by_releases_if({Action.ToProlog()}, {Agents.ToProlog()}, {Fluent.ToProlog()}, {pi}).")
                .Aggregate((a,b) => a+"\n"+b);
        }
    }

    public class ByReleases : ByReleasesIf
    {
        public ByReleases(Action action, AgentsList agents, Fluent fluent)
            : base(action, agents, fluent, null)
        {

        }
        public override string ToProlog()
        {
            return $"by_releases({Action.ToProlog()}, {Agents.ToProlog()}, {Fluent.ToProlog()}).";
        }
    }

    public class ReleasesIf : ByReleasesIf
    {
        public ReleasesIf(Action action, Fluent fluent, LogicExpression condition) 
            : base(action, null, fluent, condition) 
        {

        }
        public override string ToProlog()
        {
            return Condition.EvaluateLogicExpression()
                .ToListOfStrings()
                .Select(pi => $"releases_if({Action.ToProlog()}, {Fluent.ToProlog()}, {pi}).")
                .Aggregate((a, b) => a + "\n" + b);
        }
    }

    public class Releases : ByReleasesIf
    {
        public Releases(Action action, Fluent fluent)
            : base(action, null, fluent, null)
        {

        }
        public override string ToProlog()
        {
            return $"releases({Action.ToProlog()}, {Fluent.ToProlog()}).";
        }
    }
}