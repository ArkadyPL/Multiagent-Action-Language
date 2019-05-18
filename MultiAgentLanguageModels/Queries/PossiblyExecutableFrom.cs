using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels.Queries
{
    public class PossiblyExecutableFrom : Query
    {
        public Instruction Instructions { get; }
        public LogicExpression Condition { get; }
        public PossiblyExecutableFrom(Instruction instructions, LogicExpression condition)
        {
            Instructions = instructions;
            Condition = condition;
        }
        public override List<string> ToProlog()
        {
            var possibleConditions = Condition.EvaluateLogicExpression().ToListOfStrings();
            var result = possibleConditions.Select(pi => $"possibly_executable_from({Instructions.ToProlog()}, {pi}).").ToList();
            return result;
        }
        public override bool Interpret(IEnumerable<bool> allPossibilities)
        {
            return allPossibilities.Any(x => x);
        }
    }

    public class PossiblyExecutable : PossiblyExecutableFrom
    {
        public PossiblyExecutable(Instruction instructions)
            : base(instructions, LogicExpression.Empty)
        {
        }

        public override List<string> ToProlog()
        {
            return new List<string>() { $"possibly_executable({Instructions.ToProlog()})." };
        }
    }
}
