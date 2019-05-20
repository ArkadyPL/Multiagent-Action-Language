namespace MultiAgentLanguageModels.Expressions
{
    public abstract class Expression
    {
        public string StringExpression { get => ToProlog(); }
        public abstract string ToProlog();
    }
}