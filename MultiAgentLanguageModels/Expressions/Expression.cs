namespace MultiAgentLanguageModels.Expressions
{
    public abstract class Expression : IProlog
    {
        public abstract string ToProlog();
    }
}