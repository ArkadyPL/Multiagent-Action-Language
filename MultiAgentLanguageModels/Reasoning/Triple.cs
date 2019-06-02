using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MultiAgentLanguageModels.Reasoning
{
    public class Triple : Tuple<Action, State, AgentsList>, IEquatable<Triple>
    {
        public Triple(Action action, State state, AgentsList agents) : base(action, state, agents)
        {       
        }

        public bool Equals(Triple other)
        {
            return Item1.Equals(other.Item1) && Item2.Equals(other.Item2) && Item3.Equals(other.Item3);
        }

        public override int GetHashCode()
        {
            return (Item1.ToString() + Item2.ToString() + Item3.ToString()).GetHashCode();
        }

        public override string ToString()
        {
            return Item1.ToString() + " " + Item2.ToString() + " " + Item3.ToString();
        }
    }
}
