using System.Linq;

namespace MultiAgentLanguageModels.Expressions
{
    public class NotByIf : Expression
    {
        public Action Action { get; }
        public AgentsList Agents { get; }
        LogicExpression Condition { get; }
        public NotByIf(Action action, AgentsList agents, LogicExpression condition)
        {
            Action = action;
            Agents = agents;
            Condition = condition;
        }
        public override string ToProlog()
        {
            return Agents.Select(a => new AgentsList() { a })
                .Select(al => new ImpossibleByIf(Action, al, Condition))
                .Select(impossible => impossible.ToProlog())
                .Aggregate((a, b) => a + "\n" + b);
        }
    }

    public class NotBy : NotByIf
    {
        public NotBy(Action action, AgentsList agents)
            : base(action, agents, new True())
        {
        }

        public override string ToProlog()
        {
            return Agents.Select(a => new AgentsList() { a })
                .Select(al => new ImpossibleBy(Action, al))
                .Select(impossible => impossible.ToProlog())
                .Aggregate((a, b) => a + "\n" + b);
        }
    }
}
