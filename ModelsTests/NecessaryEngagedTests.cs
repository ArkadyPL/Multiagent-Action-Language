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
    class NecessaryEngagedTests
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
            story1 = new Story(new List<Expression>
            {
                new ByCauses(push, new List<Agent>{ a }, LogicExpression.Empty),
                new ByCauses(push, new List<Agent>{ b }, LogicExpression.Empty),
                new ByCauses(run, new List<Agent>{ a }, LogicExpression.Empty),
                new ByCauses(run, new List<Agent>{ b }, LogicExpression.Empty)
            });
            story2 = new Story(new List<Expression>
            {
                new ByCauses(sing, new List<Agent>{c}, LogicExpression.Empty),
                new ByCauses(sing, new List<Agent>{d}, LogicExpression.Empty),
                new ByCauses(fly, new List<Agent>{c}, LogicExpression.Empty)
            });
            story3 = new Story(new List<Expression>
            {
                new ByCauses(swim, new List<Agent>{x,y}, LogicExpression.Empty)
            });
        }

        [Test]
        public async Task Test11()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { a }, new List<Action> { push });
            var solution = await prologService.GetSolution(story1, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public async Task Test12()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { b }, new List<Action> { push });
            var solution = await prologService.GetSolution(story1, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public async Task Test13()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { a }, new List<Action> { run, push });
            var solution = await prologService.GetSolution(story1, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public async Task Test14()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { b }, new List<Action> { push, run });
            var solution = await prologService.GetSolution(story1, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public async Task Test21()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { c }, new List<Action> { fly, sing });
            var solution = await prologService.GetSolution(story2, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public async Task Test22()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { c }, new List<Action> { sing, fly });
            var solution = await prologService.GetSolution(story2, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public async Task Test23()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { d }, new List<Action> { fly, sing });
            var solution = await prologService.GetSolution(story2, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public async Task Test24()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { d }, new List<Action> { sing, fly });
            var solution = await prologService.GetSolution(story2, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public async Task Test31()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { x }, new List<Action> { swim });
            var solution = await prologService.GetSolution(story3, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public async Task Test32()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { x, y }, new List<Action> { swim });
            var solution = await prologService.GetSolution(story3, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public async Task Test33()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var query = new NecessaryEngaged(new List<Agent> { x, y, z }, new List<Action> { swim });
            var solution = await prologService.GetSolution(story3, query);
            Assert.AreEqual(true, solution);
        }
    }
}