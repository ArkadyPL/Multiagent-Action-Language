namespace MultiAgentLanguageModels
{
    public class Fluent : LogicElement
    {
        public string Name { get; set; }
        public override string ToString()
        {
            return Name;
        }
    }
}