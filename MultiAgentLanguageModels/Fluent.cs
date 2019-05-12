namespace MultiAgentLanguageModels
{
    public class Fluent : LogicElement
    {
        public string Name { get; set; }

        public bool Value { get; set; }

        public Fluent(string name)
        {
            Name = name;
            Left = null;
            Right = null;
        }

        public override string ToString()
        {
            return Name;
        }

        public override bool GetValue()
        {
            return Value;
        }
    }
}