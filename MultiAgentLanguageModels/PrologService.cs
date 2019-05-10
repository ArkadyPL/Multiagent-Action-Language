using MultiAgentLanguageModels.Queries;
using Prolog;
using System.IO;
using System.Linq;
using static Prolog.PrologEngine;

namespace MultiAgentLanguageModels
{
    public class PrologService : IPrologService
    {
        readonly string logic;
        
        public PrologService()
        {
            logic = GetPrologLogic();
        }

        public bool GetSolution(Story story, Query query)
        {
            Prolog.PrologEngine prolog = new Prolog.PrologEngine(new Prolog.DosIO(), false);

            prolog.ConsultFromString(story.ToProlog());

            prolog.ConsultFromString(logic);

            var solution = prolog.GetFirstSolution(query.ToProlog());

            return solution.Solved;
        }

        private string GetPrologLogic()
        {
            var path = @"logic.pl";
            if (!File.Exists(path))
                throw new System.Exception("No logic.pl file.");
            var file = File.ReadLines(path).Skip(4).Aggregate((a,b) => a+"\n"+b);
            return file;
        }
    }
}
