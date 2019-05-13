﻿namespace MultiAgentLanguageModels.Queries
{
    public class NecessaryExecutableFrom : Query, IProlog
    {
        public Instruction Instructions { get; }
        public LogicExpression Condition { get; }
        public NecessaryExecutableFrom(Instruction instructions, LogicExpression condition)
        {
            Instructions = instructions;
            Condition = condition;
        }
        public override string ToProlog()
        {
            return $"necessary_executable_from({Instructions.ToProlog()}, {Condition.ToProlog()}).";
        }
    }

    public class NecessaryExecutable : NecessaryExecutableFrom
    {
        public NecessaryExecutable(Instruction instructions)
            : base(instructions, LogicExpression.Empty)
        {
        }

        public override string ToProlog()
        {
            return $"necessary_executable({Instructions.ToProlog()}).";
        }
    }
}