using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using Ninject;
using NUnit.Framework;
using System.Collections.Generic;
using System.Reflection;

namespace Tests
{
    public class PossiblyEngagedTests
    {
        private IPrologService prologService;
        private Agent a, b, c, d, x, y;
        private Action push, run, sing, fly, swim;
        private Story story1, story2, story3;
        [SetUp]
        public void Setup()
        {
            var kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            prologService = kernel.Get<IPrologService>();

            a = new Agent("a");
            b = new Agent("b");
            c = new Agent("c");
            d = new Agent("d");
            x = new Agent("x");
            y = new Agent("y");
            push = new MultiAgentLanguageModels.Action("push");
            sing = new MultiAgentLanguageModels.Action("sing");
            run = new MultiAgentLanguageModels.Action("run");
            story1 = new Story(new List<MultiAgentLanguageModels.Expressions.Expression>
            {
                new ByCauses(push, new List<Agent>{ a }, LogicExpression.Empty),
                new ByCauses(push, new List<Agent>{ b }, LogicExpression.Empty)
            });
        }

        [Test]
        public void Test1()
        {
            var query = new PossiblyEngaged(new List<Agent> { a }, new List<Action> { push });
            var solution = prologService.GetSolution(story1, query);
            Assert.AreEqual(true, solution);
        }
    }
}
