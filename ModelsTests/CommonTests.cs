using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Action = MultiAgentLanguageModels.Action;

namespace Tests
{
    public class CommonTests
    {
        [Test]
        public void GeneratingAgents()
        {
            var names = Enumerable.Range(0, 10).Select(x => (new Guid()).ToString());
            var agents = new AgentsList(names.Select(x => new Agent(x)).ToList());
            names.ToList().Sort();
            Assert.AreEqual($"[{names.ToList().Aggregate((a, b) => a + ", " + b)}]", agents.ToProlog());
        }

        [Test]
        public void GeneratingInstructions()
        {
            Action action1 = new Action("action1");
            Action action2 = new Action("action2");
            Agent g1 = new Agent("g1");
            Agent g2 = new Agent("g2");
            Agent g3 = new Agent("g3");
            Instruction instruction = new Instruction()
            {
                new Tuple<Action, AgentsList>(action1, new AgentsList(){ g2, g1 }),
                new Tuple<Action, AgentsList>(action2, new AgentsList(){ g1, g2 })
            };
            var ans = instruction.ToProlog();
            Assert.AreEqual("[[action1, [g1, g2]], [action2, [g1, g2]]]", ans);
        }

        [Test]
        public void GeneratingStories()
        {
            var a = new Agent("a");
            var b = new Agent("b");
            var push = new Action("push");
            var run = new Action("run");

            var alpha = new Fluent("alpha");
            var beta = new Fluent("beta");
            var notBeta = new Not(beta);
            var alphaOrNotBeta = new Or(alpha, notBeta);

            var expressions = new LanguageStructure()
            {
                new Initially(alphaOrNotBeta),
                new ByCauses(push, new AgentsList{ a }, LogicExpression.Empty),
                new ByCauses(push, new AgentsList{ b }, LogicExpression.Empty),
                new ByCauses(run, new AgentsList{ a }, LogicExpression.Empty),
                new ByCauses(run, new AgentsList{ b }, LogicExpression.Empty)
            };

            var actual = expressions.ToProlog().Select(x => Regex.Replace(x, @"[^0-9a-zA-Z:.]+", "")).ToList();
            var expected = new List<string>()
            {
@"initially([\alpha, \beta]).
by_causes(push, [a], []).
by_causes(push, [b], []).
by_causes(run, [a], []).
by_causes(run, [b], []).",

@"initially([alpha, \beta]).
by_causes(push, [a], []).
by_causes(push, [b], []).
by_causes(run, [a], []).
by_causes(run, [b], []).",

@"initially([alpha, beta]).
by_causes(push, [a], []).
by_causes(push, [b], []).
by_causes(run, [a], []).
by_causes(run, [b], [])."
            }.Select(x => Regex.Replace(x, @"[^0-9a-zA-Z:.]+", "")).ToList(); 


            var check1 = expected.TrueForAll(x => actual.Contains(x));
            var check2 = actual.TrueForAll(x => expected.Contains(x));
            Assert.True(check1 && check2);
        }

        [Test]
        public void GeneratingStories2()
        {
            var a = new Agent("a");
            var b = new Agent("b");
            var push = new Action("push");
            var run = new Action("run");

            var alpha = new Fluent("alpha");
            var beta = new Fluent("beta");
            var notBeta = new Not(beta);
            var alphaOrNotBeta = new Or(alpha, notBeta);

            var gamma = new Fluent("gamma");
            var gammaOrNotGamma = new Or(gamma, new Not(gamma));

            var expressions = new LanguageStructure()
            {
                new Initially(alphaOrNotBeta),
                new ByCauses(push, new AgentsList{ a }, gammaOrNotGamma),
                new ByCauses(run, new AgentsList{ b }, LogicExpression.Empty)
            };

            var actual = expressions.ToProlog().Select(x => Regex.Replace(x, @"[^0-9a-zA-Z:.]+", "")).ToList();
            var expected = new List<string>()
            {
@"initially([\alpha, \beta]).
by_causes(push, [a], [\gamma]).
by_causes(push, [a], [gamma]).
by_causes(run, [b], []).",

@"initially([alpha, \beta]).
by_causes(push, [a], [\gamma]).
by_causes(push, [a], [gamma]).
by_causes(run, [b], []).",

@"initially([alpha, beta]).
by_causes(push, [a], [\gamma]).
by_causes(push, [a], [gamma]).
by_causes(run, [b], [])."
            }.Select(x => Regex.Replace(x, @"[^0-9a-zA-Z:.]+", "")).ToList();


            var check1 = expected.TrueForAll(x => actual.Contains(x));
            var check2 = actual.TrueForAll(x => expected.Contains(x));
            Assert.True(check1 && check2);
        }
    }
}