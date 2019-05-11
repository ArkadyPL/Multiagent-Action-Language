namespace MultiAgentLanguageModels.Queries
{
    public class NecessaryExecutableFrom : Query, IProlog
    {
        public Instruction Instructions;
        public LogicExpression Condition;
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
}