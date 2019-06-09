﻿using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Reasoning;
using NUnit.Framework;
using MultiAgentLanguageModels;
using System.Collections.Generic;

namespace Reasoning
{
    public class PossibleStatesTests
    {
        [Test]
        public void TestPossibleStates()
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

            var resp = reasoning.GenerateStructure(ExpressionsList);

            Assert.AreEqual(resp, resp);
        }

        [Test]
        public void TestRes0()
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
            var resp = reasoning.GenerateStructure(ExpressionsList);

            Assert.AreEqual(resp, resp);
        }

        [Test]
        public void TestRes()
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
            var resp = reasoning.GenerateStructure(ExpressionsList);

            Assert.AreEqual(resp, resp);
        }


        [Test]
        public void TestRes_Releases()
        {
            var hasA = new Fluent("hasA");
            var hasB = new Fluent("hasB");
            var reasoning = new ReasoningEngine();
            var ExpressionsList = new ExpressionsList()
            {
                new Causes("buypaper", new Or(hasA, hasB)),
                new ReleasesIf("buypaper", hasA, new Not(hasA)),
                new ReleasesIf("buypaper", hasB, new Not(hasB))
            };
            var resp = reasoning.GenerateStructure(ExpressionsList);

            Assert.AreEqual(resp, resp);
        }
    }
}