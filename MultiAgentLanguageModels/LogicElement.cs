namespace MultiAgentLanguageModels
{
    public abstract class LogicElement
    {
        public LogicElement Left { get; set; }
        public LogicElement Right { get; set; }
    }

    public class Not : LogicElement
    {
        public Not(LogicElement logicElement)
        {
            Left = logicElement;
            Right = null;
        }
        public override string ToString()
        {
            return $"(\\{Left.ToString()})";
        }
    }

    public class Or : LogicElement
    {
        public Or(LogicElement left, LogicElement right)
        {
            Left = left;
            Right = right;
        }
        public override string ToString()
        {
            return $"({Left.ToString()}+{Right.ToString()})";
        }
    }

    public class And : LogicElement
    {
        public And(LogicElement left, LogicElement right)
        {
            Left = left;
            Right = right;
        }
        public override string ToString()
        {
            return $"({Left.ToString()}*{Right.ToString()})";
        }
    }

    public class If : LogicElement
    {
        public If(LogicElement left, LogicElement right)
        {
            Left = left;
            Right = right;
        }
        public override string ToString()
        {
            Not notLeft = new Not(Left);
            Or or = new Or(notLeft, Right);
            return or.ToString();
        }
    }

    public class Iff : LogicElement
    {
        public Iff(LogicElement left, LogicElement right)
        {
            Left = left;
            Right = right;
        }
        public override string ToString()
        {
            If leftToRight = new If(Left, Right);
            If rightToLeft = new If(Right, Left);
            And both = new And(leftToRight, rightToLeft);
            return both.ToString();
        }
    }
}