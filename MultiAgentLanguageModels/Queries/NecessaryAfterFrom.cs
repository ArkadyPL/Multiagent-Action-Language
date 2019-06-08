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
            var res = reasoningEngine.Res(expressions);
            var initialStates = reasoningEngine.InitialStates(expressions);
            var allStates = reasoningEngine.PossibleStates(expressions);
            var piCondition = Condition.EvaluateLogicExpression();
            var alphaCondition = Result.EvaluateLogicExpression();

            foreach(var initialState in initialStates)
            {
                foreach(var state in allStates)
                {
                    if(piCondition.Any(x => state.Values.HasSubset(x)))
                    {
                        HashSet<State> currentStates = new HashSet<State>();
                        currentStates.Add(state);
                        for(int i = 0; i< Instructions.Count; i++)
                        {
                            var action = Instructions[i].Item1;
                            var agents = Instructions[i].Item2;
                            HashSet<State> newCurrentStates = new HashSet<State>();
                            foreach (var currentState in currentStates)
                            {
                                var triple = new Triple(action, currentState, agents);
                                if (res.ContainsKey(triple))
                                {
                                    res[triple].ToList().ForEach(s => newCurrentStates.Add(s));
                                }
                            }
                            currentStates = newCurrentStates;
                        }
                        return currentStates
                            .All(finallState => 
                                alphaCondition.Any(aCon => finallState.Values.HasSubset(aCon)));
                    }
                }
            }
            return true;
        }
    }

    public class NecessaryAfter : NecessaryAfterFrom
    {
        public NecessaryAfter(Instruction instructions, LogicExpression finaly) : base(instructions, finaly, new True())
        {
        }
    }
}