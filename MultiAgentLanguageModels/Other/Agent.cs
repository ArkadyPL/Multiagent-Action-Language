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

        public new string ToString()
        {
            return Name;
        }
    }

    public class AgentsList : List<Agent>, IEquatable<AgentsList>
    {
        public AgentsList() : base()
        {
        }

        public AgentsList(List<Agent> agents) : base()
        {
            this.Clear();
            agents.ForEach(x => this.Add(x));
        }
       
        public new string ToString()
        {
            return $"[{this.OrderBy(x => x.Name).Select(x => x.Name).Aggregate((a,b) =>a+ ", " +b )}]";
        }

        public bool HasSubset(AgentsList subset)
        {
            return subset.All(x => this.Contains(x));
        }

        public bool Equals(AgentsList other)
        {
            return this.Select(x => x.Name).All(t => other.Select(x => x.Name).Contains(t)) && other.Select(x => x.Name).All(t => this.Select(x => x.Name).Contains(t));
        }
    }
}
