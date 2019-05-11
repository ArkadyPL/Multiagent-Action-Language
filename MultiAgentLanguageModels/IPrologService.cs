using MultiAgentLanguageModels.Queries;
using System.Threading.Tasks;

namespace MultiAgentLanguageModels
{
    public interface IPrologService
    {
        Task<bool> GetSolution(Story story, Query query);
    }
}