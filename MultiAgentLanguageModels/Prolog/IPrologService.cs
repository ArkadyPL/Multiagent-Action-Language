using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;

namespace MultiAgentLanguageModels
{
    public interface IPrologService
    {
        bool GetSolution(ExpressionsList story, Query query);
    }
}