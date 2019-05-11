using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class Action : IProlog
    {
        public string Name { get; }
        public Action(string name)
        {
            Name = name;
        }

        public string ToProlog()
        {
            return Name;
        }
    }

    public static class ActionsList 
    {
        public static string ToProlog(this List<Action> list)
        {
            return $"[{list.OrderBy(x => x.Name).Select(x => x.Name).Aggregate((a, b) => a + ", " + b)}]";
        }
    }
}