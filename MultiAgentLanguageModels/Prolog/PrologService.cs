using MultiAgentLanguageModels.Queries;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace MultiAgentLanguageModels
{
    public class PrologService : IPrologService
    {
        const string logicPath = @"logic.pl";
        const string storyPath = @"story.pl";

        public bool GetSolution(LanguageStructure languageStructure, Query query)
        {
            return query.Interpret(languageStructure.ToProlog().Select(x => GetSolutionOfStory(x, query)));
        }

        private bool GetSolutionOfStory(string story, Query query)
        {
            if (!File.Exists(logicPath))
            {
                throw new Exception("Can't find logic.pl file.");
            }

            SaveStory(story);

            using (var prologEngine = new PrologEngine())
            {
                var logicConsult= prologEngine.ConsultAsync(logicPath);
                logicConsult.Wait();
                if (!logicConsult.Result)
                {
                    throw new Exception("Something went wrong with logic.pl file.");
                }
                var storyConsult = prologEngine.ConsultAsync(storyPath);
                storyConsult.Wait();
                if (!storyConsult.Result)
                {
                    throw new Exception("Something went wrong with story.pl file.");
                }
                IEnumerable<bool> allPossibilities = query.ToProlog().Select(q => { var temp = prologEngine.WriteLineAsync(q); temp.Wait(); return temp.Result; });

                return query.Interpret(allPossibilities);
            }
        }

        private void SaveStory(string story)
        {
            if (File.Exists(storyPath))
            {
                File.Delete(storyPath);
            }
            File.WriteAllText(storyPath, story);
            if (!File.Exists(storyPath))
            {
                throw new Exception("Can't create story file.");
            }
        }
    }
}
