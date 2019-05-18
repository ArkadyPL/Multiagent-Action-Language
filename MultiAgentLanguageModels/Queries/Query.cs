using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace MultiAgentLanguageModels.Queries
{
    public abstract class Query
    {
        public string StringExpression { get => ToProlog().Aggregate((a,b) => a+"\n"+b); }
        public abstract List<string> ToProlog();
        public abstract bool Interpret(IEnumerable<bool> allPossibilities);
    }
}
