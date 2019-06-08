using System.Collections.Generic;
using System.Linq;
using MultiAgentLanguageModels.Expressions;

namespace MultiAgentLanguageModels.Queries
{
    public class NecessaryEngagedFrom : Query
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

        public override bool Solve(ExpressionsList expressions)
        {
            throw new System.NotImplementedException();
        }
    }

    public class NecessaryEngaged : NecessaryEngagedFrom
    {
        public NecessaryEngaged(AgentsList agents, List<Action> actions)
            : base(agents, actions, LogicExpression.Empty)
        {
        }
    }
}