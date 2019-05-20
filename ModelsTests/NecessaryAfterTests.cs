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
        Action a1, a2;
        Agent g1, g2;


        [SetUp]
        public void Setup()
        {
            a1 = new Action("action1");
            a2 = new Action("action2");

            g1 = new Agent("g1");
            g2 = new Agent("g2");
        }

        [Test]
        public void Test1a()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var instructions = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a1, new AgentsList(){g1, g2 }),
                new System.Tuple<Action, AgentsList>(a2, new AgentsList(){g1, g2})
            };
            var final = new Fluent("final");
            var pi = new Fluent("pi");
            var test = new NecessaryAfterFrom(instructions, new LogicExpression(final), new LogicExpression(pi));

            var result = new Fluent("result");
            var story = new LanguageStructure()
            {
                new ByCausesIf(a1, new AgentsList(){g1, g2}, new LogicExpression(result), new LogicExpression(pi)),
                new ByCausesIf(a2, new AgentsList(){g1,g2}, new LogicExpression(new Fluent("another")), new LogicExpression(result)),
                new After(new LogicExpression(final), instructions)
            };

            var answer = prologService.GetSolution(story, test);
            Assert.AreEqual(true, answer);
        }
    }
}
