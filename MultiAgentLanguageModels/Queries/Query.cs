namespace MultiAgentLanguageModels.Queries
{
    public abstract class Query : IProlog
    {
        public abstract string ToProlog();
    }
}
