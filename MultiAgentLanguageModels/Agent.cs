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

    public static class AgentsList
    {
        public static string ToProlog(this List<Agent> list)
        {
            return $"[{list.OrderBy(x => x.Name).Select(x => x.Name).Aggregate((a,b) =>a+ ", " +b )}]";
        }
    }
}
