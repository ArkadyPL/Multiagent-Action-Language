using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class LogicExpression : IProlog
    {

        private List<Fluent> Fluents
        {
            get
            {
                List<Fluent> fluents = new List<Fluent>();
                List<LogicElement> temp = new List<LogicElement>();
                temp.Add(Element);
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
                return fluents.OrderBy(x => x.Name).ToList();
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

        public string ToProlog()
        {
            return empty ? "[]" : $"[{Element.ToString()}]";
        }

        public List<List<Tuple<string, bool>>> EvaluateLogicExpression()
        {
            List<List<Tuple<string, bool>>> results = new List<List<Tuple<string, bool>>>();
            var fluents = Fluents;
            for(int i=0; i < Math.Pow(2, fluents.Count); i++)
            {
                var binary = Convert.ToString(i, 2).PadLeft(fluents.Count, '0').Select(x => x == '1' ? true : false).ToArray();
                for(int j = 0; j < fluents.Count; j++)
                {
                    fluents[j].Value = binary[j];
                }
                if (Element.GetValue())
                {
                    results.Add(
                            fluents.Select(x => new Tuple<string, bool>(x.Name, x.Value)).ToList()
                        );
                }
            }
            return results;
        }
    }

    public static class EvaluationExt
    {
        public static List<string> ToListOfStrings(this List<List<Tuple<string, bool>>> tuples)
        {
            return tuples.Select(x => $"[{x.Select(t => t.Item2 ? t.Item1 : $"\\{t.Item1}").Aggregate((a, b) => a + ", " + b)}]").ToList();
        }
    }
}