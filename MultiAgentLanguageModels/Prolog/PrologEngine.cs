using MultiAgentLanguageModels.Queries;
using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace MultiAgentLanguageModels
{
    public class PrologEngine : IDisposable
    {
        readonly string[] paths;
        private TaskCompletionSource<bool> taskCompletionSource;
        private StreamWriter streamWriter;
        private Process prologProcess;

        public PrologEngine(string prologPath=null)
        {
            paths = new string[]
            {
                @"C:\Program Files\swipl\bin\swipl.exe",
                @"C:\Program Files (x86)\swipl\bin\swipl.exe",
                prologPath
            };

            prologProcess = new Process
            {
                StartInfo = CreatePrologStartInfoInstance()
            };

            prologProcess.Start();

            prologProcess.ErrorDataReceived += PrologProcess_ErrorDataReceived;

            streamWriter = prologProcess.StandardInput;

            prologProcess.BeginOutputReadLine();
            prologProcess.BeginErrorReadLine();
        }

        private void PrologProcess_ErrorDataReceived(object sender, DataReceivedEventArgs e)
        {
            if(!string.IsNullOrEmpty(e.Data) && !e.Data.StartsWith("Warning:") && !e.Data.Trim().StartsWith("Singleton variables"))
                throw new Exception(e.Data);
        }

        public async Task<bool> ConsultAsync(string filePath)
        {
            return await WriteLineAsync($"consult('{filePath}').");
        }

        public async Task<bool> WriteLineAsync(string line)
        {
            taskCompletionSource = new TaskCompletionSource<bool>();

            prologProcess.OutputDataReceived += OutputDataHandler;

            streamWriter?.WriteLine($"{line.TrimEnd('.')}, !.");

            //streamWriter?.WriteLine(line);

            return await taskCompletionSource.Task;
        }

        private void OutputDataHandler(object sender, DataReceivedEventArgs e)
        {
            if (!string.IsNullOrEmpty(e.Data))
            {
                if (!(e.Data.Contains("true") || e.Data.Contains("false")))
                {
                    taskCompletionSource.SetException(new Exception($"Something went wrong. Message: {e.Data}"));
                }

                prologProcess.OutputDataReceived -= OutputDataHandler;

                taskCompletionSource.SetResult(e.Data.Contains("true"));
            }
        }

        public void Dispose()
        {

            streamWriter.Dispose();
            prologProcess.Dispose();
        }

        private ProcessStartInfo CreatePrologStartInfoInstance()
        {
            var path = paths.FirstOrDefault(x => File.Exists(x)) ?? throw new Exception("Can't find swipl.exe file.");
            ProcessStartInfo result = new ProcessStartInfo
            {
                FileName = path,
                Arguments = "--quiet",
                UseShellExecute = false,
                CreateNoWindow = true,
                RedirectStandardOutput = true,
                RedirectStandardInput = true,
                RedirectStandardError = true
            };

            return result;
        }
    }
}