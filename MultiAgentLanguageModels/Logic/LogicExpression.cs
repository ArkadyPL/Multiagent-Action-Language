using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class LogicExpression
    {
        public List<IGrouping<string, Fluent>> Fluents
        {
            get
            {
                if (empty)
                {
                    return null;
                }

                List<Fluent> fluents = new List<Fluent>();
                List<LogicElement> temp = new List<LogicElement>
                {
                    Element
                };
                LogicElement logicElement;
                while (temp.Count != 0)
                {
                    logicElement = temp.First();
                    temp.RemoveAt(0);
                    if (logicElement.GetType() == typeof(Fluent))
                    {
                        fluents.Add(logicElement as Fluent);
                    }
                    else
                    {
                        if (logicElement.Left != null)
                        {
                            temp.Add(logicElement.Left);
                        }
                        if (logicElement.Right != null)
                        {
                            temp.Add(logicElement.Right);
                        }
                    }
                }
                return fluents.GroupBy(fluent => fluent.Name).OrderBy(x => x.Key).ToList();
            }
        }

        public LogicElement Element { get; }

        private readonly bool empty = false;

        private static LogicExpression _empty;

        public string StringExpression { get => Element.ToString(); }

        public static LogicExpression Empty {
            get {
                if (_empty == null)
                    _empty = new LogicExpression();
                return _empty;
            }
        }

        private LogicExpression()
        {
            empty = true;
        }

        public LogicExpression(LogicElement element)
        {
            Element = element;
        }

        public List<Dictionary<string, bool>> EvaluateLogicExpression()
        {
            if (empty)
            {
                return null;
            }

            List<Dictionary<string, bool>> results = new List<Dictionary<string, bool>>();
            var fluents = Fluents;
            for(int i=0; i < Math.Pow(2, fluents.Count); i++)
            {
                var binary = Convert.ToString(i, 2).PadLeft(fluents.Count, '0').Select(x => x == '1' ? true : false).ToArray();
                for(int j = 0; j < fluents.Count; j++)
                {
                    fluents[j].ToList().ForEach(x => x.Value = binary[j]);
                }
                if (Element.GetValue())
                {
                    Dictionary<string, bool> res = new Dictionary<string, bool>();
                    fluents.ForEach(x => res.Add(x.Key, x.First().Value));
                    results.Add(res);
                }
            }
            return results;
        }

        public static implicit operator LogicExpression(LogicElement logicElement)
        {
            return new LogicExpression(logicElement);
        }

        public static implicit operator LogicElement(LogicExpression logicExpression)
        {
            return logicExpression.Element;
        }

        public static implicit operator LogicExpression(string str)
        {
            return new Fluent(str);
        }
    }

    public static class EvaluationExt
    {
        public static List<string> ToListOfStrings(this List<List<Tuple<string, bool>>> tuples)
        {
            if(tuples == null)
            {
                return new List<string>() { "[]" };
            }
            return tuples.Select(x => $"[{x.Select(t => t.Item2 ? t.Item1 : $"\\{t.Item1}").Aggregate((a, b) => a + ", " + b)}]").ToList();
        }
    }
}