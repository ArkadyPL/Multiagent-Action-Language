using System.Collections.Generic;
using System.Linq;
using MultiAgentLanguageModels.Expressions;

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

        public override bool Solve(ExpressionsList expressions)
        {
            throw new System.NotImplementedException();
        }
    }

    public class PossiblyAfter : PossiblyAfterFrom
    {
        public PossiblyAfter(Instruction instructions, LogicExpression finaly)
            : base(instructions, finaly, new True())
        {
        }
    }
}
