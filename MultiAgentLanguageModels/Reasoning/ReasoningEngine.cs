using MultiAgentLanguageModels.Expressions;
using System.Collections.Generic;
using System.Linq;
using System;

namespace MultiAgentLanguageModels.Reasoning
{
    public class ReasoningEngine
    {
        public List<string> InertialFluents(ExpressionsList expressions)
        {
            return expressions.Fluents.Except(expressions.Noninertial.Select(x => x.Fluent.Name)).ToList();
        }

        public HashSet<State> PossibleStates(ExpressionsList expressions)
        {
            HashSet<State> result = new HashSet<State>();
            var fluents = expressions.Fluents;
            for (int i = 0; i < Math.Pow(2, fluents.Count); i++)
            {
                var binary = Convert.ToString(i, 2).PadLeft(fluents.Count, '0').Select(x => x == '1' ? true : false).ToArray();
                Dictionary<string, bool> stateValues = new Dictionary<string, bool>();
                for (int j = 0; j < fluents.Count; j++)
                {
                    stateValues[fluents[j]] = binary[j];
                }
                result.Add(new State(stateValues));
            }
            if(expressions.Always.Count != 0)
            {
                var always = expressions.Always.Select(x => x.Condition).Aggregate((a, b) => new And(a, b)).EvaluateLogicExpression();
                result = new HashSet<State>(result.Where(s => always.Any(a => a.Intersect(s.Values).Count() == a.Count())));
            }
            return result;
        }

        public Dictionary<Tuple<Action, State, AgentsList>, HashSet<State>> Res0(ExpressionsList expressions)
        {
            Dictionary<Tuple<Action, State, AgentsList>, HashSet<State>> result = new Dictionary<Tuple<Action, State, AgentsList>, HashSet<State>>();
            foreach (var causes in expressions.Causes)
            {
                foreach (var state in PossibleStates(expressions))
                {
                    foreach(var group in expressions.AgentsGroups())
                    {
                        if(causes.Pi.EvaluateLogicExpression().Any(x => state.Values.HasSubset(x)) &&
                            group.HasSubset(causes.G) && causes.Alpha.EvaluateLogicExpression().Count !=0 )
                        {
                            Tuple<Action, State, AgentsList> tuple = new Tuple<Action, State, AgentsList>(
                                causes.A, state, group);
                            var subsetOfFinalStates = new HashSet<State>();
                            var final = causes.Alpha.EvaluateLogicExpression();
                            foreach (var s in PossibleStates(expressions))
                            {
                                if(final.Any(x => s.Values.HasSubset(x)))
                                {
                                    subsetOfFinalStates.Add(s);
                                }
                            }
                            result.Add(tuple, subsetOfFinalStates);
                        }
                    }
                    
                }
            }
            return result;
        }


    }
}
