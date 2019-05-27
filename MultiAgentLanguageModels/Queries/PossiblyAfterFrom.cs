using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels.Queries
{
    public class PossiblyAfterFrom : Query
    {
        public LogicExpression Result { get; }
        public Instruction Instructions { get; }
        public LogicExpression Condition { get; }

        public PossiblyAfterFrom(Instruction instructions, LogicExpression finaly, LogicExpression condition)
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
                $"possibly_after_from({alpha}, {Instructions.ToProlog()}, {pi}).")
                ).Aggregate((a, b) => a.Concat(b)).ToList();
            return results;
        }

        public override bool Interpret(IEnumerable<bool> allPossibilities)
        {
            return allPossibilities.Any(x => x);
        }
    }
    public class PossiblyAfter : PossiblyAfterFrom
    {
        public PossiblyAfter(Instruction instructions, LogicExpression finaly)
            : base(instructions, finaly, new True())
        {
        }

        public override List<string> ToProlog()
        {
            var possibleResults = Result.EvaluateLogicExpression().ToListOfStrings();
            var results = possibleResults.Select(
                alpha => $"possibly_after({alpha}, {Instructions.ToProlog()}).").ToList();
            return results;
        }
    }
}
