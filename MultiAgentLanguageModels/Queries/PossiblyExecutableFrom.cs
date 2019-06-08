using System.Collections.Generic;
using System.Linq;
using MultiAgentLanguageModels.Expressions;

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

        public override bool Solve(ExpressionsList expressions)
        {
            throw new System.NotImplementedException();
        }
    }

    public class PossiblyExecutable : PossiblyExecutableFrom
    {
        public PossiblyExecutable(Instruction instructions)
            : base(instructions, LogicExpression.Empty)
        {
        }
    }
}
