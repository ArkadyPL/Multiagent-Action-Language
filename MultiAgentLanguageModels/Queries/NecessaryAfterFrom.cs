using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using MultiAgentLanguageModels.Expressions;

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

        public override bool Solve(ExpressionsList expressions)
        {
            throw new System.NotImplementedException();
        }
    }

    public class NecessaryAfter : NecessaryAfterFrom
    {
        public NecessaryAfter(Instruction instructions, LogicExpression finaly) : base(instructions, finaly, null)
        {
        }
    }
}