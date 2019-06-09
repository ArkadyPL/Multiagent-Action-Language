using MultiAgentLanguageModels.Expressions;
using System.Collections.Generic;
using System.Linq;
using System;

namespace MultiAgentLanguageModels.Reasoning
{
    public class ReasoningEngine
    {
        private List<string> InertialFluents(ExpressionsList expressions)
        {
            return expressions.Fluents.Except(expressions.Noninertial.Select(x => x.Fluent.Name)).ToList();
        }

        private HashSet<State> PossibleStates(ExpressionsList expressions)
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

        private Dictionary<Triple, HashSet<State>> Res0(ExpressionsList expressions)
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
                        List<bool> piConditions = new List<bool>();
                        //now we iterate through the cause statements to find those
                        foreach(var cause in causesGroup)
                        {
                            var w1 = cause.Pi.EvaluateLogicExpression().Any(x => state.Values.HasSubset(x));
                            piConditions.Add(w1);
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
                        //create our A x sigma x G triple
                        Triple tuple = new Triple(
                                causesGroup.Key, state, group);
                        if (workingCauses.Count == 0)
                        {
                            if(piConditions.All(x => !x))
                                result.Add(tuple, PossibleStates(expressions));
                            continue;
                        } 
                        //now we need to create uber-alpha condition
                        var uberAlpha = workingCauses.Select(x => x.Alpha).Aggregate((a, b) => new And(a, b));
                        //final states should be compliant with our uberAlpha
                        var final = uberAlpha.EvaluateLogicExpression();
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
                        if (result.ContainsKey(tuple))
                        {
                            setOfFinalStates.ToList().ForEach(x => result[tuple].Add(x));
                        }
                        else
                        {
                            result.Add(tuple, setOfFinalStates);
                        }
                    }
                }
            }
            return result;
        }

        private HashSet<string> New(ExpressionsList expressions, State from, State to, AgentsList agents, Action action)
        {
            //find all fluents that differs
            var diff = to.Values.Where(x => !from.Values.Contains(x)).Select(x => x.Key);
            //except noninertial ones
            diff = diff.Except(expressions.Noninertial.Select(x => x.Fluent.Name));
            //add fluents from release statements
            diff = diff.Concat(expressions.Releases.Where(
                x => x.Action.Equals(action) && 
                x.Agents.HasSubset(agents) &&
                x.Condition.EvaluateLogicExpression().Any(e => from.Values.HasSubset(e))).Select(x => x.Fluent.Name));

            return new HashSet<string>(diff);
            //maybe except should be at the end of the query?
        }

        private Dictionary<Triple, HashSet<State>> Res(ExpressionsList expressions)
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

        private HashSet<State> InitialStates(ExpressionsList expressions)
        {
            var allStates = PossibleStates(expressions);
            HashSet<State> initialStates = new HashSet<State>();

            if (expressions.Initially.Count == 0)
            {
                initialStates = allStates;
            }
            else
            {
                var initialConditions = expressions.Initially
                .Select(x => x.Condition)
                .Aggregate((a, b) => new And(a, b))
                .EvaluateLogicExpression();

                foreach (var state in allStates)
                {
                    if (initialConditions.Any(x => state.Values.HasSubset(x)))
                    {
                        initialStates.Add(state);
                    }
                }
            }
            
            return initialStates;
        }

        public Structure GenerateStructure(ExpressionsList expressions)
        {
            var res = Res(expressions);
            var possibleStates = PossibleStates(expressions);
            var initialStates = InitialStates(expressions);

            var resWithAfter = new Dictionary<Triple, HashSet<State>>();
            #region After statements
            //now lets get to the part where we intersect 
            //initial states with after statements
            var afterExpressions = expressions.AfterExpressions;
            if (afterExpressions.Count != 0)
            {
                foreach (var after in afterExpressions)
                {
                    Dictionary<Triple, HashSet<State>> changesInRes = new Dictionary<Triple, HashSet<State>>();
                    HashSet<State> currentStates = new HashSet<State>();
                    //find final states
                    foreach (var state in possibleStates)
                    {
                        if (after.FinalCondition.EvaluateLogicExpression().Any(x => state.Values.HasSubset(x)))
                        {
                            currentStates.Add(state);
                        }
                    }
                    //we are going backward
                    after.Instructions.Reverse();
                    //now we iterate through instructions
                    for (int i = 0; i < after.Instructions.Count; i++)
                    {
                        var action = after.Instructions[i].Item1;
                        var agents = after.Instructions[i].Item2;
                        HashSet<State> newCurrentStates = new HashSet<State>();
                        //for each state in current states we want to move backward in graph
                        //we have action name, agents group and final state of edge
                        foreach (var currentState in currentStates)
                        {
                            var previousStates = res.Where(kv => kv.Value.Contains(currentState)
                                    && kv.Key.Item1.Equals(action)
                                    && kv.Key.Item3.Equals(agents)).ToList();

                            foreach(var kv in previousStates)
                            {
                                if (!resWithAfter.ContainsKey(kv.Key))
                                {
                                    var a = res[kv.Key];
                                    resWithAfter[kv.Key] = new HashSet<State>();
                                }
                                resWithAfter[kv.Key].Add(currentState);
                                newCurrentStates.Add(kv.Key.Item2);
                            }
                        }

                        currentStates = newCurrentStates;
                    }
                    //now we must get only initial states that are possible
                    initialStates.IntersectWith(currentStates);
                }
                foreach (var kv in resWithAfter)
                {
                    res[kv.Key].Clear();
                    res[kv.Key] = kv.Value;
                }
            }
            #endregion

            #region Observable After statements
            var observableAfterExpressions = expressions.ObservableAfterExpressions;
            if (observableAfterExpressions.Count != 0)
            {
                foreach (var observableAfter in observableAfterExpressions)
                {
                    HashSet<State> currentStates = new HashSet<State>();
                    //find final states
                    foreach (var state in possibleStates)
                    {
                        if (observableAfter.FinalCondition.EvaluateLogicExpression().Any(x => state.Values.HasSubset(x)))
                        {
                            currentStates.Add(state);
                        }
                    }
                    //we are going backward
                    observableAfter.Instructions.Reverse();
                    //now we iterate through instructions
                    for (int i = 0; i < observableAfter.Instructions.Count; i++)
                    {
                        var action = observableAfter.Instructions[i].Item1;
                        var agents = observableAfter.Instructions[i].Item2;
                        HashSet<State> newCurrentStates = new HashSet<State>();
                        //for each state in current states we want to move backward in graph
                        //we have action name, agents group and final state of edge
                        foreach (var currentState in currentStates)
                        {
                            foreach (var kv in res)
                            {
                                if (kv.Value.Contains(currentState)
                                    && kv.Key.Item1.Equals(action)
                                    && kv.Key.Item3.Equals(agents))
                                {
                                    newCurrentStates.Add(kv.Key.Item2);
                                }
                            }
                        }
                        currentStates = newCurrentStates;
                    }
                    //observable after expression really does nothing, except when there are not any path - then the whole model is false -> don't have any initial nodes
                    if (currentStates.Count == 0)
                    {
                        initialStates = new HashSet<State>();
                    }
                }
            }
            #endregion

            return new Structure(initialStates, possibleStates, res);
        }
    }
}
