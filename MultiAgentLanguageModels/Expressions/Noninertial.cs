namespace MultiAgentLanguageModels.Expressions
{
    public class Noninertial : Expression
    {
        Fluent Fluent { get; }

        public Noninertial(Fluent fluent)
        {
            Fluent = fluent;
        }

        public override string ToProlog()
        {
            return $"noninertial({Fluent.Name})";
        }
    }
}
