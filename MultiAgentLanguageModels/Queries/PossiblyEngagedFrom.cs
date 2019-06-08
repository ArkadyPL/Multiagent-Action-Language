using System.Collections.Generic;
using System.Linq;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Reasoning;

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
            var reasoningEngine = new ReasoningEngine();
            var res = reasoningEngine.Res(expressions);
            var initialStates = reasoningEngine.InitialStates(expressions);
            var allStates = reasoningEngine.PossibleStates(expressions);
            var piCondition = Condition.EvaluateLogicExpression();

            //we want that list to hold result of query for each initial state
            //could be done prettier but it's easier to read
            List<bool> resultsForEachInitiallState = new List<bool>();

            //for each initiall state
            foreach (var initialState in initialStates)
            {
                HashSet<State> currentStates = new HashSet<State>();
                //if condition is always true then our current state is initial state
                if (Condition.Element is True)
                {
                    currentStates.Add(initialState);
                }
                //else we have to find all states that are ok
                else
                {
                    foreach (var state in allStates)
                    {
                        if (piCondition.Any(x => state.Values.HasSubset(x)))
                        {
                            currentStates.Add(state);
                        }
                    }
                }

                //now we iterate through actions to see if there exists path "with Agents"
                for (int i = 0; i < Actions.Count; i++)
                {
                    var action = Actions[i];
                    HashSet<State> newCurrentStates = new HashSet<State>();
                    //for each state in current states we want to move forward in graph
                    foreach(var state in currentStates)
                    {
                        foreach(var agents in expressions.AgentsGroups())
                        {
                            var triple = new Triple(action, state, agents);
                            if (res.ContainsKey(triple))
                            {
                                res[triple].ToList().ForEach(x => newCurrentStates.Add(x));
                            }
                        }
                    }
                    currentStates = newCurrentStates;
                }
                var endPathWithAgents = currentStates;

                //if condition is always true then our current state is initial state
                if (Condition.Element is True)
                {
                    currentStates.Add(initialState);
                }
                //else we have to find all states that are ok
                else
                {
                    foreach (var state in allStates)
                    {
                        if (piCondition.Any(x => state.Values.HasSubset(x)))
                        {
                            currentStates.Add(state);
                        }
                    }
                }
                for (int i = 0; i < Actions.Count; i++)
                {
                    var action = Actions[i];
                    HashSet<State> newCurrentStates = new HashSet<State>();
                    //for each state in current states we want to move forward in graph
                    foreach (var state in currentStates)
                    {
                        foreach (var agents in expressions.AgentsGroups())
                        {
                            var agentsWithout = new AgentsList(agents.Except(Agents).ToList());
                            var triple = new Triple(action, state, agentsWithout);
                            if (res.ContainsKey(triple))
                            {
                                res[triple].ToList().ForEach(x => newCurrentStates.Add(x));
                            }
                        }
                    }
                    currentStates = newCurrentStates;
                }
                resultsForEachInitiallState.Add()
            }
            return resultsForEachInitiallState.All(x => x);
        }
    }

    public class PossiblyEngaged : PossiblyEngagedFrom
    {
        public PossiblyEngaged(AgentsList agents, List<Action> actions)
            : base(agents, actions, new True())
        {
        }
    }
}