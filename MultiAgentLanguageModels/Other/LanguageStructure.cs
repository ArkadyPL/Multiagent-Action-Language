using MultiAgentLanguageModels.Expressions;
using System.Collections.Generic;
using System.Linq;

namespace MultiAgentLanguageModels
{
    public class LanguageStructure : List<Expression>
    {
        private IEnumerable<Initially> Initiallies { get
            {
                return this.Where(x => x.GetType() == typeof(Initially)).Select(x => x as Initially);
            }
        }
        public IEnumerable<string> StringExpression { get => ToProlog(); }
        public IEnumerable<string> ToProlog()
        {
            var initiallies = Initiallies;
            if(initiallies.Count() == 0)
            {
                return new List<string>() { this.Select(x => x.ToProlog()).Aggregate((a, b) => a + "\n" + b) };
            }
            var restOfTheStory = this.Except(initiallies);
            var initialStatesExpression = initiallies.Select(x => x.Condition).Aggregate((a, b) => new And(a, b));
            var initialStates = initialStatesExpression.EvaluateLogicExpression().ToListOfStrings();
            var stories = initialStates.Select(s => $"initially({s}).\n" + restOfTheStory.Select(x => x.ToProlog()).Aggregate((a, b) => a + "\n" + b));
            return stories;
        }
    }
}
