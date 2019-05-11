using MultiAgentLanguageModels.Expressions;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class Story : IProlog
    {
        List<Expression> Expressions { get; }

        public Story(List<Expression> expressions)
        {
            Expressions = expressions;
        }

        public string ToProlog()
        {
            string story = string.Empty;
            Expressions.ForEach(e => story = story + e.ToProlog() + "\n");

            //if (!Expressions.Exists(exp => exp.GetType() == typeof(ByCauses)))
            //    story = story + "by_causes([],[],[])." + "\n";

            //if (!Expressions.Exists(exp => exp.GetType() == typeof(ImpossibleIf)))
            //    story = story + "impossible_if([],[])." + "\n";

            //if (!Expressions.Exists(exp => exp.GetType() == typeof(CausesIf)))
            //    story = story + "causes_if([],[],[])." + "\n";

            //if (!Expressions.Exists(exp => exp.GetType() == typeof(ImpossibleBy)))
            //    story = story + "impossible_by([],[])." + "\n";

            return story;
        }
    }
}
