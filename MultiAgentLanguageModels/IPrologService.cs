using MultiAgentLanguageModels.Queries;

namespace MultiAgentLanguageModels
{
    public interface IPrologService
    {
        bool GetSolution(Story story, Query query);
    }
}