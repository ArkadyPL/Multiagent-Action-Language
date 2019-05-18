using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using Ninject;
using NUnit.Framework;
using System.Collections.Generic;
using System.Reflection;
using System.Threading.Tasks;

namespace Tests
{
    public class PossiblyEngagedTests
    {
        private Agent a, b, c, d, x, y, z;
        private Action push, run, sing, fly, swim;
        private Story story1, story2, story3;

        [SetUp]
        public void Setup()
        {
            a = new Agent("a");
            b = new Agent("b");
            c = new Agent("c");
            d = new Agent("d");
            x = new Agent("x");
            y = new Agent("y");
            z = new Agent("z");
            push = new Action("push");
            sing = new Action("sing");
            fly = new Action("fly");
            run = new Action("run");
            swim = new Action("swim");
            story1 = new Story()
            {
                new ByCauses(push, new AgentsList{ a }, LogicExpression.Empty),
                new ByCauses(push, new AgentsList{ b }, LogicExpression.Empty)
            };
            story2 = new Story()
            {
                new ByCauses(sing, new AgentsList{c}, LogicExpression.Empty),
                new ByCauses(sing, new AgentsList{d}, LogicExpression.Empty),
                new ByCauses(fly, new AgentsList{c}, LogicExpression.Empty)
            };
            story3 = new Story()
            {
                new ByCauses(swim, new AgentsList{x,y}, LogicExpression.Empty)
            };
        }

        [Test]
        public void Test11()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { a }, new List<Action> { push });
            var solution = prologService.GetSolution(story1, query);
            Assert.AreEqual(true, solution);
        }
        
        [Test]
        public void Test12()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { b }, new List<Action> { push });
            var solution = prologService.GetSolution(story1, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test13()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { a }, new List<Action> { run, push });
            var solution = prologService.GetSolution(story1, query);
            Assert.AreEqual(false, solution);
        }
        
        [Test]
        public void Test14()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { b }, new List<Action> { push, run });
            var solution = prologService.GetSolution(story1, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public void Test21()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { c }, new List<Action> { fly, sing });
            var solution = prologService.GetSolution(story2, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test22()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { c }, new List<Action> { sing, fly });
            var solution = prologService.GetSolution(story2, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test23()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { d }, new List<Action> { fly, sing });
            var solution = prologService.GetSolution(story2, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public void Test24()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { d }, new List<Action> { sing, fly });
            var solution = prologService.GetSolution(story2, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public void Test31()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { x }, new List<Action> { swim });
            var solution = prologService.GetSolution(story3, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public void Test32()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { x, y }, new List<Action> { swim });
            var solution = prologService.GetSolution(story3, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test33()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new PossiblyEngaged(new AgentsList { x, y, z }, new List<Action> { swim });
            var solution = prologService.GetSolution(story3, query);
            Assert.AreEqual(true, solution);
        }
    }
}
