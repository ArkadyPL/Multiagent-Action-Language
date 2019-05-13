namespace MultiAgentLanguageModels.Queries
{
    public class PossiblyExecutableFrom : Query, IProlog
    {
        public Instruction Instructions { get; }
        public LogicExpression Condition { get; }
        public PossiblyExecutableFrom(Instruction instructions, LogicExpression condition)
        {
            Instructions = instructions;
            Condition = condition;
        }
        public override string ToProlog()
        {
            return $"possibly_executable_from({Instructions.ToProlog()}, {Condition.ToProlog()}).";
        }
    }

    public class PossiblyExecutable : PossiblyExecutableFrom
    {
        public PossiblyExecutable(Instruction instructions)
            : base(instructions, LogicExpression.Empty)
        {
        }

        public override string ToProlog()
        {
            return $"possibly_executable({Instructions.ToProlog()}).";
        }
    }
}
