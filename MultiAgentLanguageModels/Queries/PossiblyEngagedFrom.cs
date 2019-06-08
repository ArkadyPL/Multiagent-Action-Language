using System.Collections.Generic;
using System.Linq;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Reasoning;

namespace MultiAgentLanguageModels.Queries
{
    public class PossiblyEngagedFrom : Query
    {
        public AgentsList Agents { get; }
        public Instruction Instructions { get; }
        public LogicExpression Condition { get; }

        public PossiblyEngagedFrom(AgentsList agents, Instruction instructions, LogicExpression condition)
        {
            Agents = agents;
            Instructions = instructions;
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
                //now we iterate through instructions
                for (int i = 0; i < Instructions.Count; i++)
                {
                    var action = Instructions[i].Item1;
                    var agents = Instructions[i].Item2;
                    HashSet<State> newCurrentStates = new HashSet<State>();
                    //for each state in current states we want to move forward in graph
                    foreach (var currentState in currentStates)
                    {
                        var triple = new Triple(action, currentState, agents);
                        //if we can find good edge in graph 
                        //from currentState, specific action and agents group then
                        if (res.ContainsKey(triple))
                        {
                            //add all next states to the newCurrentStates
                            res[triple].ToList().ForEach(s => newCurrentStates.Add(s));
                        }
                    }
                    //do it again for new action and agents group
                    currentStates = newCurrentStates;
                }
                resultsForEachInitiallState.Add(currentStates.Count != 0);
            }
            return resultsForEachInitiallState.All(x => x);
        }
    }

    public class PossiblyEngaged : PossiblyEngagedFrom
    {
        public PossiblyEngaged(AgentsList agents, Instruction instructions)
            : base(agents, instructions, new True())
        {
        }
    }
}