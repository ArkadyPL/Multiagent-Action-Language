using System;

namespace MultiAgentLanguageModels
{
    public class Fluent : LogicElement, IEquatable<Fluent>
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

        public string ToProlog()
        {
            return $"[{Name}]";
        }

        public override bool GetValue()
        {
            return Value;
        }
        
        public bool Equals(Fluent other)
        {
            return Name == other.Name;
        }
        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }
    }
}