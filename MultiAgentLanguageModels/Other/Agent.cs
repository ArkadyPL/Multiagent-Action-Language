using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class Agent : IEquatable<Agent>
    {
        public string Name { get; }

        public Agent(string name)
        {
            Name = name;
        }

        public static implicit operator Agent(string str)
        {
            return new Agent(str);
        }

        public bool Equals(Agent other)
        {
            return Name == other.Name;
        }
    }

    public class AgentsList : List<Agent>
    {
        public AgentsList() : base()
        {
        }

        public AgentsList(List<Agent> agents) : base()
        {
            this.Clear();
            agents.ForEach(x => this.Add(x));
        }

        public string ToProlog()
        {
            return $"[{this.OrderBy(x => x.Name).Select(x => x.Name).Aggregate((a,b) =>a+ ", " +b )}]";
        }

        public bool HasSubset(AgentsList subset)
        {
            return subset.All(x => this.Contains(x));
        }
    }
}
