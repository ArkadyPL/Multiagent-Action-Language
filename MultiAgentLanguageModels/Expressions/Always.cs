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

        public override string ToProlog()
        {
            return Condition.EvaluateLogicExpression()
                .ToListOfStrings()
                .Select(x => $"always({x}).")
                .Aggregate((a, b) => a + "\n" + b);
        }
    }
}
