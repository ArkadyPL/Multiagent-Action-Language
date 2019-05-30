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
    }

    public class NotBy : NotByIf
    {
        public NotBy(Action action, AgentsList agents)
            : base(action, agents, new True())
        {
        }
    }
}
