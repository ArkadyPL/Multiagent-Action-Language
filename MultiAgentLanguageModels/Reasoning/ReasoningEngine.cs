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
            //get all fluents
            var fluents = expressions.Fluents;
            //with brute force method create every possible combination of 0 and 1 fluents
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
            //now if there is at least one always statement cut impossible states
            if(expressions.Always.Count != 0)
            {
                            //get all always statements, then get conditions
                var always = expressions.Always.Select(x => x.Condition)
                    //aggregate them to one uber-always condition and finally evaluate logic expression
                    .Aggregate((a, b) => new And(a, b)).EvaluateLogicExpression();
                //if state is compliant to the always logic expression then add it to the set
                result = new HashSet<State>(result.Where(s => always.Any(a => a.Intersect(s.Values).Count() == a.Count())));
            }
            return result;
        }

        public Dictionary<Triple, HashSet<State>> Res0(ExpressionsList expressions)
        {
            Dictionary<Triple, HashSet<State>> result = new Dictionary<Triple, HashSet<State>>();
            //For each group of Cause statements e.g.
            //A causes alpha if pi
            //A causes beta
            //we want to group them together, so that we can examine them wrt to state and agents group
            foreach (var causesGroup in expressions.Causes.GroupBy(x => x.A))
            {
                //for each state in possible states
                foreach (var state in PossibleStates(expressions))
                {
                    //for each group in possible agents group
                    foreach(var group in expressions.AgentsGroups())
                    {
                        //we want to create list of cause statements that works with specific state and agent group
                        List<ByCausesIf> workingCauses = new List<ByCausesIf>();
                        //now we iterate through the cause statements to find those
                        foreach(var cause in causesGroup)
                        {
                            var w1 = cause.Pi.EvaluateLogicExpression().Any(x => state.Values.HasSubset(x));
                            var w2 = group.HasSubset(cause.G);
                            var w3 = cause.Alpha.EvaluateLogicExpression().Count != 0;
                                //if pi condition is ok with current state
                            if (cause.Pi.EvaluateLogicExpression().Any(x => state.Values.HasSubset(x))
                                //if group is superset
                                && group.HasSubset(cause.G))
                                //if alpha condition is not false
                                //&& cause.Alpha.EvaluateLogicExpression().Count != 0)
                            {
                                //then we add new cause expression
                                workingCauses.Add(cause);
                            }
                        }
                        if (workingCauses.Count == 0)
                            continue;
                        //now we need to create uber-alpha condition
                        var uberAlpha = workingCauses.Select(x => x.Alpha).Aggregate((a, b) => new And(a, b));
                        //final states should be compliant with our uberAlpha
                        var final = uberAlpha.EvaluateLogicExpression();
                        //create our A x sigma x G triple
                        Triple tuple = new Triple(
                                causesGroup.Key, state, group);
                        //look through all possible states to find the ones that can end this action, and create set
                        var setOfFinalStates = new HashSet<State>();
                        foreach (var s in PossibleStates(expressions))
                        {
                            //if state s is compliant then add it to the set
                            if (final.Any(x => s.Values.HasSubset(x)))
                            {
                                setOfFinalStates.Add(s);
                            }
                        }
                        //add triple and set to dictionary
                        result.Add(tuple, setOfFinalStates);
                    }
                }
            }
            return result;
        }

        public HashSet<string> New(ExpressionsList expressions, State from, State to, AgentsList agents, Action action)
        {
            //find all fluents that differs
            var diff = to.Values.Where(x => !from.Values.Contains(x)).Select(x => x.Key);
            //except noninertial ones
            diff = diff.Except(expressions.Noninertial.Select(x => x.Fluent.Name));
            //add fluents from release statements
            diff = diff.Concat(expressions.Releases.Where(
                x => x.Action.Name == action.Name && 
                x.Agents.HasSubset(agents) &&
                x.Condition.EvaluateLogicExpression().Any(e => from.Values.HasSubset(e))).Select(x => x.Fluent.Name));

            return new HashSet<string>(diff);
            //maybe except should be at the end of the query?
        }

        public Dictionary<Triple, HashSet<State>> Res(ExpressionsList expressions)
        {
            var results = new Dictionary<Triple, HashSet<State>>();
            var res0 = Res0(expressions);
            var temp = res0.GroupBy(x => x.Key);
            foreach(var key in res0.Keys)
            {
                var state = key.Item2;
                var action = key.Item1;
                var agents = key.Item3;
                var newBasedOnRes0 = res0[key].Select(x => New(expressions, state, x, agents, action)).ToList();
                if (newBasedOnRes0.Count != 0)
                {
                    //should be minimal with respect to set inclusions.
                    var min = newBasedOnRes0.Min(x => x.Count); //res0[key].Select(x => New(expressions, state, x, agents, action).Count).Min();
                    //var temp2 = res0[key].Select(x => New(expressions, state, x, agents, action));
                    var res = res0[key].Where(x => New(expressions, state, x, agents, action).Count == min);
                    results.Add(key, new HashSet<State>(res));
                }
            }
            
            return results;
        }

        public HashSet<State> InitialStates(ExpressionsList expressions)
        {
            var allStates = PossibleStates(expressions);
            if (expressions.Initially.Count == 0)
                return allStates;

            var initialConditions = expressions.Initially
                .Select(x => x.Condition)
                .Aggregate((a, b) => new And(a, b))
                .EvaluateLogicExpression();

            HashSet<State> initialStates = new HashSet<State>();
            foreach(var state in allStates)
            {
                if(initialConditions.Any(x => state.Values.HasSubset(x))){
                    initialStates.Add(state);
                }
            }
            return initialStates;
        }
    }
}
