using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Reasoning;
using NUnit.Framework;
using MultiAgentLanguageModels;
using System.Collections.Generic;

namespace MultiAgentLanguageModelsTests
{
    public class PossibleStatesTests
    {
        [Test]
        public void Test1()
        {
            var reasoning = new ReasoningEngine();
            var ExpressionsList = new ExpressionsList()
            {
                new Always("f"),
                new ByCausesIf("a1", new AgentsList(){ "g1", "g2"}, "f1", "f")
            };
            var resp = reasoning.PossibleStates(ExpressionsList);

            Assert.AreEqual(resp, resp);
        }
    }
}