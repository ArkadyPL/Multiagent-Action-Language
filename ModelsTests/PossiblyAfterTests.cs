using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using Ninject;
using NUnit.Framework;
using System.Reflection;

namespace Tests
{
    public class PossiblyAfterTests
    {
        Action a1, a1b, a2b;
        Agent g1, g1b, g2b;
        Fluent result, pi, finalb, resultb, anotherb, pib, p2;
        LanguageStructure s1, s2;

        [SetUp]
        public void Setup()
        {
            a1 = new Action("a1");
            a1b = new Action("a1_2");
            a2b = new Action("a2b");
            g1 = new Agent("g1");
            g1b = new Agent("g1b");
            g2b = new Agent("g2b");
            result = new Fluent("d1");
            finalb = new Fluent("d2");
            resultb = new Fluent("d3");
            anotherb = new Fluent("d4");
            pib = new Fluent("d5");
            pi = new Fluent("p1");
            p2 = new Fluent("p2");
            s1 = new LanguageStructure()
            {
                new After(result, new Instruction()
                {
                    new System.Tuple<Action, AgentsList>(a1, new AgentsList(){ g1 })
                }),
                new ByReleasesIf(a1, new AgentsList(){g1}, result, pi)
            };

            s2 = new LanguageStructure()
            {
                new ByReleasesIf(a1b, new AgentsList(){g1b, g2b}, resultb, pib),
                new ByCausesIf(a2b, new AgentsList(){g1b, g2b}, anotherb, resultb),
                new After(finalb, new Instruction()
                {
                    new System.Tuple<Action, AgentsList>(a1b, new AgentsList(){g1b, g2b}),
                    new System.Tuple<Action, AgentsList>(a2b, new AgentsList(){g1b,g2b})
                })
            };
        }

        [Test]
        public void Test11()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a1, new AgentsList(){g1})
            };
            var query = new PossiblyAfterFrom(instruction, result, pi);
            var solution = prologService.GetSolution(s1, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test12()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a1b, new AgentsList(){g1b, g2b}),
                new System.Tuple<Action, AgentsList>(a2b, new AgentsList(){g1b, g2b})
            };
            var query = new PossiblyAfterFrom(instruction, finalb, pib);
            var solution = prologService.GetSolution(s2, query);
            Assert.AreEqual(true, solution);
        }
    }
}