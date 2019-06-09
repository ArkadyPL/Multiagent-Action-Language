using System.Collections.Generic;

namespace MultiAgentLanguageModels.Reasoning
{
    public class Structure
    {
        public HashSet<State> InitialStates { get; }
        public HashSet<State> PossibleStates { get; }
        public Dictionary<Triple, HashSet<State>> Res { get; }
        public Structure(HashSet<State> initialStates, HashSet<State> possibleStates, Dictionary<Triple, HashSet<State>> res)
        {
            InitialStates = initialStates;
            PossibleStates = possibleStates;
            Res = res;
        }
    }
}
