using System.Collections.Generic;
using System.Linq;

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

        public override List<string> ToProlog()
        {
            var possibleConditions = Condition.EvaluateLogicExpression().ToListOfStrings();
            var result = possibleConditions.Select(pi => $"necessary_engaged_from({Agents.ToProlog()}, {Actions.ToProlog()}, {pi}).").ToList();
            return result;
        }
        public override bool Interpret(IEnumerable<bool> allPossibilities)
        {
            return allPossibilities.All(x => x);
        }
    }

    public class NecessaryEngaged : NecessaryEngagedFrom
    {
        public NecessaryEngaged(AgentsList agents, List<Action> actions)
            : base(agents, actions, LogicExpression.Empty)
        {
        }

        public override List<string> ToProlog()
        {
            return new List<string>() { $"necessary_engaged({Agents.ToProlog()}, {Actions.ToProlog()})." };
        }
    }
}