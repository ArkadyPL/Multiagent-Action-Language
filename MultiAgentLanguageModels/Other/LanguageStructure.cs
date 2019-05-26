using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Other;
using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class LanguageStructure : List<Expression>
    {
        private List<List<Tuple<string,bool>>> ConstFluents
        {
            get
            {
                List<List<Tuple<string, bool>>> result = new List<List<Tuple<string, bool>>>();
                foreach (Expression ex in this)
                {
                    if (ex as Always != null)
                    {
                        var temp = ex as Always;
                        result.AddRange(temp.Condition.EvaluateLogicExpression());
                    }
                }
                return result;
            }
        }
        private List<string> Fluents {
            get
            {
                List<string> fluents = new List<string>();
                foreach (Expression ex in this)
                {
                    if (ex as ByCausesIf != null)
                    {
                        var temp = ex as ByCausesIf;
                        fluents.AddRange(temp.Pi.Fluents.Select(x => x.Key));
                        fluents.AddRange(temp.Alpha.Fluents.Select(x => x.Key));
                    }
                    else if (ex as ByReleasesIf != null)
                    {
                        var temp = ex as ByReleasesIf;
                        fluents.AddRange(temp.Condition.Fluents.Select(x => x.Key));
                        fluents.Add(temp.Fluent.Name);
                    }
                    else if (ex as Always != null)
                    {
                        var temp = ex as Always;
                        fluents.AddRange(temp.Condition.Fluents.Select(x => x.Key));
                    }
                    else if (ex as Initially != null)
                    {
                        var temp = ex as Initially;
                        fluents.AddRange(temp.Condition.Fluents.Select(x => x.Key));
                    }
                    else if (ex as ObservableAfter != null)
                    {
                        var temp = ex as ObservableAfter;
                        fluents.AddRange(temp.FinalCondition.Fluents.Select(x => x.Key));
                    }
                    else
                    {
                        var temp = ex as Noninertial;
                        fluents.Add(temp.Fluent.Name);
                    }
                }
                return fluents.Distinct().ToList(); 
            }
        }
        private IEnumerable<Initially> Initiallies { get
            {
                return this.Where(x => x.GetType() == typeof(Initially)).Select(x => x as Initially);
            }
        }
        public List<string> StringExpression { get => ToProlog(); }
        public List<string> ToProlog()
        {
            var initiallies = Initiallies;
            if(initiallies.Count() == 0)
            {
                return new List<string>() { this.Select(x => x.ToProlog()).Aggregate((a, b) => a + "\n" + b) };
            }
            var restOfTheStory = this.Except(initiallies);
            var initialStatesExpression = initiallies.Select(x => x.Condition).Aggregate((a, b) => new And(a, b));
            var initialStates = initialStatesExpression.EvaluateLogicExpression().ToListOfStrings();
            var stories = initialStates.Select(s => $"initially({s}).\n" + restOfTheStory.Select(x => x.ToProlog()).Aggregate((a, b) => a + "\n" + b));
            return stories.ToList();
        }
        public List<Node> GenerateNodes()
        {
            var fluents = Fluents.ToList();
            List<Node> temp = new List<Node>();
            for (int i = 0; i < Math.Pow(2, fluents.Count); i++)
            {
                var binary = Convert.ToString(i, 2).PadLeft(fluents.Count, '0').Select(x => x == '1' ? true : false).ToArray();
                temp.Add(new Node(fluents.Zip(binary, (f, b) => new Tuple<string, bool>(f, b))));
            }
            List<Node> result = new List<Node>();
            foreach(var i in ConstFluents)
            {
                foreach(var node in temp)
                {
                    if (i.All(p => node.FluentsValues.Contains(p)))
                        result.Add(node);
                }
            }
            return result;
        }
        public List<Edge> GenerateEdges()
        {
            List<Edge> edges = new List<Edge>();
            var nodes = GenerateNodes();
            foreach (Expression ex in this)
            {
                if (ex as ByCausesIf != null)
                {
                    var temp = ex as ByCausesIf;
                    var froms = new List<Node>();
                    if (temp.Pi.Element as True != null)
                    {
                        froms = nodes.ToList();
                    }
                    else
                    {
                        foreach (var pi in temp.Pi.EvaluateLogicExpression())
                        {
                            foreach (var node in nodes)
                            {
                                if (pi.All(p => node.FluentsValues.Contains(p)))
                                    froms.Add(node);
                            }
                        }
                    }

                    var tos = new List<Node>();
                    if(temp.Alpha.Element as False != null)
                    {
                        tos.Add(Node.ForbiddenNode);
                    }
                    else
                    {
                        foreach (var alpha in temp.Alpha.EvaluateLogicExpression())
                        {
                            foreach (var node in nodes)
                            {
                                if (alpha.All(p => node.FluentsValues.Contains(p)))
                                    tos.Add(node);
                            }
                        }
                    }
                    
                    edges.AddRange(froms.Select(from => tos.Select(to => new Edge(from, to, temp.A, temp.G))).Aggregate((a,b) => a.Concat(b)));
                }
                else if (ex as ByReleasesIf != null)
                {
                    var temp = ex as ByReleasesIf;
                    var froms = new List<Node>();
                    if (temp.Condition.Element as True != null)
                    {
                        froms = nodes.ToList();
                    }
                    else
                    {
                        foreach (var pi in temp.Condition.EvaluateLogicExpression())
                        {
                            foreach (var node in nodes)
                            {
                                if (pi.All(p => node.FluentsValues.Contains(p)))
                                    froms.Add(node);
                            }
                        }
                    }

                    var tos = nodes;

                    edges.AddRange(froms.Select(from => tos.Select(to => new Edge(from, to, temp.Action, temp.Agents))).Aggregate((a, b) => a.Concat(b)));
                }
                else if (ex as ObservableAfter != null)
                {
                    var temp = ex as ObservableAfter;
                    
                }
                else if (ex as After != null)
                {
                    var temp = ex as After;

                }
            }
            return edges;
        }

        
    }
}
