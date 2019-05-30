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
    }

    public class ByReleases : ByReleasesIf
    {
        public ByReleases(Action action, AgentsList agents, Fluent fluent)
            : base(action, agents, fluent, new True())
        {

        }
    }

    public class ReleasesIf : ByReleasesIf
    {
        public ReleasesIf(Action action, Fluent fluent, LogicExpression condition)
            : base(action, null, fluent, condition)
        {

        }
    }

    public class Releases : ByReleasesIf
    {
        public Releases(Action action, Fluent fluent)
            : base(action, null, fluent, null)
        {

        }
    }
}