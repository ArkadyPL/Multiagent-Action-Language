using System.Linq;

namespace MultiAgentLanguageModels.Expressions
{
    public class After : Expression
    {
        public LogicExpression FinalCondition { get; }
        public Instruction Instructions { get; }
        public After(LogicExpression finalCondition, Instruction instructions)
        {
            FinalCondition = finalCondition;
            Instructions = instructions;
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
    }
}