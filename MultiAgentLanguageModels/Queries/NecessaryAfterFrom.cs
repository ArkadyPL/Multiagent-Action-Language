using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace MultiAgentLanguageModels.Queries
{
    public class NecessaryAfterFrom : Query
    {
        public LogicExpression Result { get; }
        public Instruction Instructions { get; }
        public LogicExpression Condition { get; }

        public NecessaryAfterFrom(Instruction instructions, LogicExpression finaly, LogicExpression condition)
        {
            Instructions = instructions;
            Result = finaly;
            Condition = condition;
        }

        public override List<string> ToProlog()
        {
            var possibleResults = Result.EvaluateLogicExpression().ToListOfStrings();
            var possibleConditions = Condition.EvaluateLogicExpression().ToListOfStrings();
            var results = possibleResults.Select(
                alpha => possibleConditions.Select(pi =>
                $"necessary_after_from({alpha}, {Instructions.ToProlog()}, {pi}).")
                ).Aggregate((a,b) => a.Concat(b)).ToList();
            return results;
        }

        public override bool Interpret(IEnumerable<bool> allPossibilities)
        {
            return allPossibilities.All(x => x);
        }
    }

    public class NecessaryAfter : NecessaryAfterFrom
    {
        public NecessaryAfter(Instruction instructions, LogicExpression finaly) : base(instructions, finaly, null)
        {
        }

        public override List<string> ToProlog()
        {
            return Result.EvaluateLogicExpression().ToListOfStrings().Select(alpha => $"necessary_after({alpha}, {Instructions.ToProlog()}).").ToList();
        }
    }
}