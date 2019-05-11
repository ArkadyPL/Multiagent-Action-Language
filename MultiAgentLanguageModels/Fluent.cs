namespace MultiAgentLanguageModels
{
    public class Fluent : LogicElement
    {
        public string Name { get; set; }

        public Fluent(string name)
        {
            Name = name;
        }

        public override string ToString()
        {
            return Name;
        }
    }
}