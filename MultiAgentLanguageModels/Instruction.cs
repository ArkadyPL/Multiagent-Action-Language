using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class Instruction : List<Tuple<Action, AgentsList>>, IProlog
    {
        public string ToProlog()
        {
            return $"[{this.Select(x => x.ToProlog()).Aggregate((a,b) => a + ", " + b)}]";
        }
    }

    public static class TupleExt
    {
        public static string ToProlog(this Tuple<Action, AgentsList> tuple)
        {
            return $"[{tuple.Item1.ToProlog()}, {tuple.Item2.ToProlog()}]";
        }
    }
}