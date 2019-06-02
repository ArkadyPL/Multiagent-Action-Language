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
            var loaded = new Fluent("loaded");
            var walking = new Fluent("walking");
            var alive = new Fluent("alive");
            var reasoning = new ReasoningEngine();
            var ExpressionsList = new ExpressionsList()
            {
                new Initially(new And(loaded, walking)),
                new Always(new If(walking, alive)),
                new Causes("load", loaded),
                new Causes("shoot", new Not(loaded)),
                new CausesIf("shoot", new Not(alive), loaded)
            };
            var resp = reasoning.PossibleStates(ExpressionsList);

            Assert.AreEqual(resp, resp);
        }

        [Test]
        public void Test3()
        {
            var loaded = new Fluent("loaded");
            var walking = new Fluent("walking");
            var alive = new Fluent("alive");
            var reasoning = new ReasoningEngine();
            var ExpressionsList = new ExpressionsList()
            {
                new Initially(new And(loaded, walking)),
                new Always(new If(walking, alive)),
                new Causes("load", loaded),
                new Causes("shoot", new Not(loaded)),
                new CausesIf("shoot", new Not(alive), loaded)
            };
            var resp = reasoning.Res0(ExpressionsList);

            Assert.AreEqual(resp, resp);
        }

        [Test]
        public void Test2()
        {
            var loaded = new Fluent("loaded");
            var walking = new Fluent("walking");
            var alive = new Fluent("alive");
            var reasoning = new ReasoningEngine();
            var ExpressionsList = new ExpressionsList()
            {
                new Initially(new And(loaded, walking)),
                new Always(new If(walking, alive)),
                new Causes("load", loaded),
                new Causes("shoot", new Not(loaded)),
                new CausesIf("shoot", new Not(alive), loaded)
            };
            var resp = reasoning.Res(ExpressionsList);

            Assert.AreEqual(resp, resp);
        }
    }
}