namespace MultiAgentLanguageModels
{
    public class LogicExpression : IProlog
    {
        public LogicElement Element { get; }

        private readonly bool empty = false;

        private static LogicExpression _empty;
        public static LogicExpression Empty {
            get {
                if (_empty == null)
                    _empty = new LogicExpression();
                return _empty;
            } }

        private LogicExpression()
        {
            empty = true;
        }

        public LogicExpression(LogicElement element)
        {
            Element = element;
        }

        public string ToProlog()
        {
            return empty ? "[]" : $"[{Element.ToString()}]";
        }
    }
}