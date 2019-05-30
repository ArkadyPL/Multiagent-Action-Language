using MultiAgentLanguageModels.Other;
using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels.Expressions
{
    public class ExpressionsList : List<Expression>
    {
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
                }
                return actions.Distinct().ToList();
            }
        }
        public List<string> Fluents
        {
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
    }
}
