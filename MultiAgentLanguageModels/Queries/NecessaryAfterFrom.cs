using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Reasoning;

namespace MultiAgentLanguageModels.Queries
{
    public class NecessaryAfterFrom : Query
    {
        public LogicExpression Result { get; }
        public Instruction Instructions { get; }
        public LogicExpression Condition { get; }

        public NecessaryAfterFrom(Instruction instructions, LogicExpression finaly, LogicExpression condition)
        {
            Instructions = instructions;
            Result = finaly;
            Condition = condition;
        }

        public override bool Solve(ExpressionsList expressions)
        {
            var reasoningEngine = new ReasoningEngine();
            var structure = reasoningEngine.GenerateStructure(expressions);
            var res = structure.Res;
            var initialStates = structure.InitialStates;
            var allStates = structure.PossibleStates; 
            var piCondition = Condition.EvaluateLogicExpression();
            var alphaCondition = Result.EvaluateLogicExpression();

            //we want that list to hold result of query for each initial state
            //could be done prettier but it's easier to read
            List<bool> resultsForEachInitiallState = new List<bool>();

            //for each initiall state
            foreach(var initialState in initialStates)
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
                //finally we are in the last nodes after the whole instruction
                //for each initial state we want see if all of states (last nodes) are compliant with alpha
                //therefore we need to add result to resultsForEachInitiallState list
                resultsForEachInitiallState.Add(
                    currentStates
                    .All(finallState => alphaCondition
                                        .Any(aCon => finallState.Values.HasSubset(aCon))
                    )
                );
            }
            return resultsForEachInitiallState.All(x => x);
        }
    }

    public class NecessaryAfter : NecessaryAfterFrom
    {
        public NecessaryAfter(Instruction instructions, LogicExpression finaly) : base(instructions, finaly, new True())
        {
        }
    }
}