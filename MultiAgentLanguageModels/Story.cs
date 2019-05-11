using MultiAgentLanguageModels.Expressions;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class Story : List<Expression>, IProlog
    {
        public string ToProlog()
        {
            string story = string.Empty;
            this.ForEach(e => story = story + e.ToProlog() + "\n");
            return story;
        }
    }
}
