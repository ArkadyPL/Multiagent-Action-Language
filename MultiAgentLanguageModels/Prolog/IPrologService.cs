using MultiAgentLanguageModels.Queries;

namespace MultiAgentLanguageModels
{
    public interface IPrologService
    {
        bool GetSolution(LanguageStructure story, Query query);
    }
}