namespace MultiAgentLanguageModels.Expressions
{
    public class Initially : Expression
    {
        public LogicExpression Condition { get; }

        public Initially(LogicExpression condition)
        {
            Condition = condition;
        }
    }
}
