using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels.Queries
{
    public class NecessaryExecutableFrom : Query
    {
        public Instruction Instructions { get; }
        public LogicExpression Condition { get; }
        public NecessaryExecutableFrom(Instruction instructions, LogicExpression condition)
        {
            Instructions = instructions;
            Condition = condition;
        }
        public override List<string> ToProlog()
        {
            var possibleCondition = Condition.EvaluateLogicExpression().ToListOfStrings();
            var result = possibleCondition.Select(pi => $"necessary_executable_from({Instructions.ToProlog()}, {pi}).").ToList();
            return result;
        }

        public override bool Interpret(IEnumerable<bool> allPossibilities)
        {
            return allPossibilities.All(x => x);
        }
    }

    public class NecessaryExecutable : NecessaryExecutableFrom
    {
        public NecessaryExecutable(Instruction instructions)
            : base(instructions, LogicExpression.Empty)
        {
        }

        public override List<string> ToProlog()
        {
            return new List<string>() { $"necessary_executable({Instructions.ToProlog()})." };
        }
    }
}