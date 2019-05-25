using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using Ninject;
using NUnit.Framework;
using System.Reflection;

namespace Tests
{
    public class PossiblyExecutableTests
    {
        Action a11, a12, a21;
        Agent g11, g12, g21;
        Fluent d1, p1, d2, p2;
        LanguageStructure s1, s2;

        [SetUp]
        public void Setup()
        {
            a11 = new Action("a1_1");
            a12 = new Action("a1_2");
            a21 = new Action("a21");
            g11 = new Agent("g1_1");
            g12 = new Agent("g1_2");
            g21 = new Agent("g22");
            d1 = new Fluent("d1");
            d2 = new Fluent("d2");
            p1 = new Fluent("p1");
            p2 = new Fluent("p2");
            s1 = new LanguageStructure()
            {
                new ByReleasesIf(a11, new AgentsList(){g11}, d1, p1),
                new ByCausesIf(a12, new AgentsList(){g12}, d1, p1)
            };
            s2 = new LanguageStructure()
            {
                new ImpossibleBy(a21, new AgentsList(){g21}),
                new ByReleasesIf(a21, new AgentsList(){g21}, d2, p2)
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
                new System.Tuple<Action, AgentsList>(a11, new AgentsList(){g11})
            };

            var query = new PossiblyExecutableFrom(instruction, new LogicExpression(p1));
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
                new System.Tuple<Action, AgentsList>(a12, new AgentsList(){g12})
            };

            var query = new PossiblyExecutableFrom(instruction, new LogicExpression(p1));
            var solution = prologService.GetSolution(s1, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test21()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a21, new AgentsList(){g21})
            };

            var query = new PossiblyExecutableFrom(instruction, new LogicExpression(p2));
            var solution = prologService.GetSolution(s2, query);
            Assert.AreEqual(false, solution);
        }
    }
}