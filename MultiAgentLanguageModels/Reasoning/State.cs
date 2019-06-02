using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels.Reasoning
{
    public class State : IEquatable<State>
    {
        public Dictionary<string,bool> Values { get; }
        public State(Dictionary<string, bool> fluentsValues)
        {
            Values = fluentsValues;
        }

        public bool Equals(State other)
        {
            return other.Values.Keys.All(x => Values.ContainsKey(x) && Values[x] == other.Values[x]) &&
                Values.Keys.All(x => other.Values.ContainsKey(x) && Values[x] == other.Values[x]);
        }

        public new string ToString()
        {
            return $"[{Values.Keys.Select(t => Values[t] ? t : $"\\{t}").Aggregate((a, b) => a + ", " + b)}]";
        }
    }

    public static class DicExt
    {
        public static bool HasSubset<TKey, TValue>(this Dictionary<TKey, TValue> first, Dictionary<TKey, TValue> subset)
        {
            return subset.All(x => first.ContainsKey(x.Key) && first[x.Key].Equals(subset[x.Key]));
        }
    }
}
