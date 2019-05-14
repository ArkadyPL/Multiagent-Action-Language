using MultiAgentLanguageModels.Queries;
using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace MultiAgentLanguageModels
{
    public class PrologService : IPrologService
    {
        readonly string logicPath;
        readonly string storyPath;
        private TaskCompletionSource<bool> taskCompletionSource;

        public PrologService()
        {
            storyPath = @"story.pl";
            logicPath = @"logic.pl";
            taskCompletionSource = new TaskCompletionSource<bool>();
        }

        public async Task<bool> GetSolution(Story story, Query query)
        {
            if (!File.Exists(logicPath))
            {
                throw new Exception("Can't find logic.pl file.");
            }

            SaveStory(story);

            using (var prologEngine = new PrologEngine())
            {
                if(!await prologEngine.ConsultAsync(logicPath))
                {
                    throw new Exception("Something went wrong with logic.pl file.");
                }
                if(!await prologEngine.ConsultAsync(storyPath))
                {
                    throw new Exception("Something went wrong with story.pl file.");
                }
                return await prologEngine.AskQuery(query);
            }
        }

        private void SaveStory(Story story)
        {
            if (File.Exists(storyPath))
            {
                File.Delete(storyPath);
            }
            File.WriteAllText(storyPath, story.ToProlog());
            if (!File.Exists(storyPath))
            {
                throw new Exception("Can't create story file.");
            }
        }
    }
}
