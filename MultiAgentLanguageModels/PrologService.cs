using MultiAgentLanguageModels.Queries;
using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace MultiAgentLanguageModels
{
    public class PrologService : IPrologService
    {
        readonly string[] paths;
        readonly string logicPath;
        readonly string storyPath;
        private TaskCompletionSource<bool> taskCompletionSource;
        private StreamWriter streamWriter;
        private Process prologProcess;
        private Query query;

        public PrologService()
        {
            storyPath = @"story.pl";
            logicPath = @"logic.pl";
            paths = new string[]
            {
                @"C:\Program Files\swipl\bin\swipl.exe",
                @"C:\Program Files (x86)\swipl\bin\swipl.exe"
            };
            taskCompletionSource = new TaskCompletionSource<bool>();
        }

        public async Task<bool> GetSolution(Story story, Query query)
        {
            this.query = query;

            if (!File.Exists(logicPath))
            {
                throw new Exception("Can't find logic.pl file.");
            }

            SaveStory(story);

            using (prologProcess = new Process())
            {
                prologProcess.StartInfo = CreatePrologStartInfoInstance();

                prologProcess.OutputDataReceived += LogicConsultHandler;

                prologProcess.Start();

                streamWriter = prologProcess.StandardInput;

                prologProcess.BeginOutputReadLine();

                Consult(logicPath);

                return await taskCompletionSource.Task;
            }
        }

        private void Consult(string path)
        {
            streamWriter?.WriteLine($"consult('{path}').");
        }

        private void AskQuery(Query query)
        {
            var q = query.ToProlog();
            streamWriter?.WriteLine(q);
        }

        private ProcessStartInfo CreatePrologStartInfoInstance()
        {
            var path = paths.FirstOrDefault(x => File.Exists(x));
            ProcessStartInfo result = new ProcessStartInfo();
            result.FileName = path ?? throw new Exception("Can't find swipl.exe file.");
            result.Arguments = "";
            result.UseShellExecute = false;
            result.CreateNoWindow = true;
            result.RedirectStandardOutput = true;
            result.RedirectStandardInput = true;

            return result;
        }

        private void LogicConsultHandler(object sender, DataReceivedEventArgs e)
        {
            if (!string.IsNullOrEmpty(e.Data))
            {
                if (!e.Data.Contains("true"))
                {
                    throw new Exception($"Something went wrong with logic.pl file. Message: {e.Data}");
                }
                prologProcess.OutputDataReceived -= LogicConsultHandler;
                prologProcess.OutputDataReceived += StoryConsultHandler;
                Consult(storyPath);
            }
        }

        private void StoryConsultHandler(object sender, DataReceivedEventArgs e)
        {
            if (!string.IsNullOrEmpty(e.Data))
            {
                if (!e.Data.Contains("true"))
                {
                    throw new Exception($"Something went wrong with story.pl file. Message: {e.Data}");
                }
                prologProcess.OutputDataReceived -= StoryConsultHandler;
                prologProcess.OutputDataReceived += QueryHandler;
                AskQuery(query);
            }
        }

        private void QueryHandler(object sender, DataReceivedEventArgs e)
        {
            if (!string.IsNullOrEmpty(e.Data))
            {
                if (!(new string[] { "true", "false"}).Any(x => e.Data.Contains(x)))
                {
                    throw new Exception($"Something went wrong with answer. Message: {e.Data}");
                }
                prologProcess.OutputDataReceived -= QueryHandler;
                taskCompletionSource.SetResult(e.Data.Contains("true") ? true : false);
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
