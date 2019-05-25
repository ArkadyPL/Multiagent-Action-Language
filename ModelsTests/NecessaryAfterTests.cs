using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using Ninject;
using NUnit.Framework;
using System.Reflection;

namespace Tests
{
    public class NecessaryAfterTests
    {
        

        [SetUp]
        public void Setup()
        {
            
        }

        [Test]
        public void Test1()
        {
            Action a1, a2;
            AgentsList G1 = new AgentsList()
            {
                new Agent("g11"), new Agent("g12")
            };
            a1 = new Action("a11");
            a2 = new Action("a12");
            Fluent result, pi, another, expected;
            result = new Fluent("res");
            pi = new Fluent("pi");
            another = new Fluent("another");
            expected = new Fluent("exp");

            var story = new LanguageStructure()
            {
                new ByCausesIf(a1, G1, result, pi),
                new ByCauses(a2, G1, another),
                new After(expected, new Instruction()
                {
                    new System.Tuple<Action, AgentsList>(a1, G1),
                    new System.Tuple<Action, AgentsList>(a2, G1)
                })
            };

            var query = new NecessaryAfter(new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a1, G1),
                new System.Tuple<Action, AgentsList>(a2, G1)
            }, expected);

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(false, answer);
        }

        [Test]
        public void Test2()
        {
            AgentsList G = new AgentsList()
            {
                "g1", "g2"
            };
            
            var story = new LanguageStructure()
            {
                new ByCausesIf("a1", G, "result", LogicExpression.Empty),
                new ByCausesIf("a2", G, "another", "pi"),
                new After("expected", new Instruction()
                {
                    new System.Tuple<Action, AgentsList>("a1", G),
                    new System.Tuple<Action, AgentsList>("a2", G)
                })
            };

            var query = new NecessaryAfter(new Instruction()
            {
                new System.Tuple<Action, AgentsList>("a1", G),
                new System.Tuple<Action, AgentsList>("a2", G)
            }, "expected");

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(false, answer);
        }

        [Test]
        public void Test3()
        {
            var G = new AgentsList()
            {
                "g1", "g2"
            };

            var story = new LanguageStructure(){
                new ByCauses("a1", G, "res"),
                new ByCausesIf("a2", G, "another", "res"),
                new After("expected", new Instruction()
                {
                    new System.Tuple<Action, AgentsList>("a1", G),
                    new System.Tuple<Action, AgentsList>("a2", G)
                })
            };
            var query = new NecessaryAfterFrom(new Instruction()
            {
                new System.Tuple<Action, AgentsList>("a1", G),
                new System.Tuple<Action, AgentsList>("a2", G)
            }, "expected", LogicExpression.Empty);

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(true, answer);
        }
    }
}
