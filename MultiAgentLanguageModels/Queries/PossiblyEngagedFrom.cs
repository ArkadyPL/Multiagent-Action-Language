using System.Collections.Generic;
using System.Linq;
using MultiAgentLanguageModels.Expressions;

namespace MultiAgentLanguageModels.Queries
{
    public class PossiblyEngagedFrom : Query
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

        public override bool Solve(ExpressionsList expressions)
        {
            throw new System.NotImplementedException();
        }
    }

    public class PossiblyEngaged : PossiblyEngagedFrom
    {
        public PossiblyEngaged(AgentsList agents, List<Action> actions)
            : base(agents, actions, LogicExpression.Empty)
        {
        }
    }
}