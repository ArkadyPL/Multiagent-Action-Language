using MultiAgentLanguageModels.Expressions;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class Story : List<Expression>
    {
        public string StringExpression { get => ToProlog(); }
        public string ToProlog()
        {
            string story = this.Select(x => x.ToProlog()).Aggregate((a, b) => a + "\n" + b);
            return story;
        }
    }
}
