﻿using System.Collections.Generic;
using System.Linq;
using MultiAgentLanguageModels.Expressions;

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

        public override bool Solve(ExpressionsList expressions)
        {
            throw new System.NotImplementedException();
        }
    }

    public class NecessaryExecutable : NecessaryExecutableFrom
    {
        public NecessaryExecutable(Instruction instructions)
            : base(instructions, LogicExpression.Empty)
        {
        }
        
    }
}