using System.Linq;

namespace MultiAgentLanguageModels.Expressions
{
    public class After : Expression
    {
        LogicExpression FinalCondition { get; }
        Instruction Instructions { get; }
        public After(LogicExpression finalCondition, Instruction instructions)
        {
            FinalCondition = finalCondition;
            Instructions = instructions;
        }
        public override string ToProlog()
        {
            return FinalCondition.EvaluateLogicExpression()
                .ToListOfStrings()
                .Select(x => $"after({x},{Instructions.ToProlog()}).")
                .Aggregate((a, b) => a + "\n" + b);
        }
    }

    public class ObservableAfter : Expression
    {
        public LogicExpression FinalCondition { get; }
        public Instruction Instructions { get; }
        public ObservableAfter(LogicExpression finalCondition, Instruction instructions)
        {
            FinalCondition = finalCondition;
            Instructions = instructions;
        }
        public override string ToProlog()
        {
            return FinalCondition.EvaluateLogicExpression()
                .ToListOfStrings()
                .Select(x => $"observable_after({x},{Instructions.ToProlog()}).")
                .Aggregate((a, b) => a + "\n" + b);
        }
    }
}