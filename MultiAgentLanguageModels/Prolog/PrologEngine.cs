﻿using MultiAgentLanguageModels.Queries;
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
        private StreamWriter streamWriter;
        private StreamReader streamReader;
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
            streamReader = prologProcess.StandardOutput;

            //prologProcess.BeginErrorReadLine();
        }

        private void PrologProcess_ErrorDataReceived(object sender, DataReceivedEventArgs e)
        {
            if(!string.IsNullOrEmpty(e.Data) && !e.Data.StartsWith("Warning:") && !e.Data.Trim().StartsWith("Singleton variables"))
                throw new Exception(e.Data);
        }

        public bool ConsultAsync(string filePath)
        {
            return WriteLine($"consult('{filePath}').");
        }

        public bool WriteLine(string line)
        {
            streamWriter?.WriteLine($"{line.TrimEnd('.')}, !.");
            string data;
            do
            {
                data = streamReader.ReadLine();
            } while (string.IsNullOrEmpty(data));

            if (!(data.Contains("true") || data.Contains("false")))
            {
                throw new Exception($"Something went wrong. Message: {data}");
            }

            return data.Contains("true");
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