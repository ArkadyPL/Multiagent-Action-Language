using System.Linq;

namespace MultiAgentLanguageModels.Expressions
{
    public class Always : Expression
    {
        public LogicExpression Condition { get; }

        public Always(LogicExpression condition)
        {
            Condition = condition;
        }
    }
}
