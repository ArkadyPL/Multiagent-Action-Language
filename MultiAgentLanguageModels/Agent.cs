using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class Agent
    {
        public string Name { get; }

        public Agent(string name)
        {
            Name = name;
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
    }
}
