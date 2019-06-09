using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels.Expressions
{
    public class ExpressionsList : List<Expression>
    {
        public ExpressionsList()
        {
        }
        public ExpressionsList(IEnumerable<Agent> agents, IEnumerable<Fluent> fluents)
        {
            Agent = agents;
            Fluent = fluents;
        }
        private IEnumerable<Agent> Agent { get; set; }
        private IEnumerable<Fluent> Fluent { get; set; }
        public List<Action> Actions
        {
            get
            {
                List<Action> actions = new List<Action>();
                foreach (Expression ex in this)
                {
                    if (ex as ByCausesIf != null)
                    {
                        var temp = ex as ByCausesIf;
                        actions.Add(temp.A);
                    }
                    else if (ex as ByReleasesIf != null)
                    {
                        var temp = ex as ByReleasesIf;
                        actions.Add(temp.Action);
                    }
                    else if (ex as ObservableAfter != null)
                    {
                        var temp = ex as ObservableAfter;
                        actions.AddRange(temp.Instructions.Select(x => x.Item1));
                    }
                    else if (ex as After != null)
                    {
                        var temp = ex as After;
                        actions.AddRange(temp.Instructions.Select(x => x.Item1));
                    }
                }
                return actions.Distinct().ToList();
            }
        }
        public List<string> Fluents
        {
            get
            {
                List<string> fluents = new List<string>();
                if(!(Fluent is null))
                {
                    fluents.AddRange(Fluent.Select(x => x.Name));
                    return fluents.Distinct().ToList();
                }
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
                    else if (ex as After != null)
                    {
                        var temp = ex as After;
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
        public List<Initially> Initially
        {
            get
            {
                return this.Where(x => x as Initially != null).Select(x => x as Initially).ToList();
            }
        }
        public List<Noninertial> Noninertial
        {
            get
            {
                return this.Where(x => x as Noninertial != null).Select(x => x as Noninertial).ToList();
            }
        }
        public List<ByCausesIf> Causes
        {
            get
            {
                return this.Where(x => x as ByCausesIf != null).Select(x => x as ByCausesIf).ToList();
            }
        }
        public List<ByReleasesIf> Releases
        {
            get
            {
                return this.Where(x => x as ByReleasesIf != null).Select(x => x as ByReleasesIf).ToList();
            }
        }
        public List<Always> Always
        {
            get
            {
                return this.Where(x => x as Always != null).Select(x => x as Always).ToList();
            }
        }
        public List<AgentsList> AgentsGroups()
        {
            AgentsList agents;
            if (Agent is null)
            {
                agents = new AgentsList();
            }
            else
            {
                agents = new AgentsList(Agent.ToList());
            }
            
            foreach (Expression ex in this)
            {
                if (ex as ByCausesIf != null)
                {
                    var temp = ex as ByCausesIf;
                    agents.AddRange(temp.G);
                }
                else if (ex as ByReleasesIf != null)
                {
                    var temp = ex as ByReleasesIf;
                    agents.AddRange(temp.Agents);
                }
                else if (ex as ObservableAfter != null)
                {
                    var temp = ex as ObservableAfter;
                    agents.AddRange(temp.Instructions.SelectMany(x => x.Item2));
                }
                else if (ex as After != null)
                {
                    var temp = ex as After;
                    agents.AddRange(temp.Instructions.SelectMany(x => x.Item2));
                }
            }
            agents = new AgentsList(agents.Distinct().ToList());
            var result = new List<AgentsList>();
            for (int i = 0; i < Math.Pow(2, agents.Count); i++)
            {
                var binary = Convert.ToString(i, 2).PadLeft(agents.Count, '0').Select(x => x == '1' ? true : false).ToArray();
                AgentsList temp = new AgentsList();
                for (int j = 0; j < agents.Count; j++)
                {
                    if (binary[j])
                    {
                        temp.Add(agents[j]);
                    }
                }
                result.Add(temp);
            }
            return result;

        }
        public List<After> AfterExpressions
        {
            get
            {
                return this.Select(x => x as After).Where(x => !(x is null)).ToList();
            }
        }
        public List<ObservableAfter> ObservableAfterExpressions
        {
            get
            {
                return this.Select(x => x as ObservableAfter).Where(x => !(x is null)).ToList();
            }
        }
    }
}
