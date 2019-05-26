using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using Ninject;
using NUnit.Framework;
using System.Reflection;

namespace Tests
{
    public class Story1
    {
        LanguageStructure story;
        [SetUp]
        public void Setup()
        {
            story = new LanguageStructure()
            {
                new Initially(new Not(new Fluent("isRunning"))),
                new ImpossibleIf("push", "isRunning"),
                new ImpossibleBy("push", new AgentsList(){"d"}),
                new ByCauses("push", new AgentsList(){"a"}, new Not(new Fluent("isRunning"))),
                new ByCauses("push", new AgentsList(){"b"}, new Not(new Fluent("isRunning"))),
                new ByCauses("push", new AgentsList(){"c"}, new Not(new Fluent("isRunning"))),
                new ByCauses("push", new AgentsList(){"c", "b"}, new Not(new Fluent("isRunning"))),
                new ByCauses("push", new AgentsList(){"a", "b"}, new Fluent("isRunning")),
                new ByCauses("push", new AgentsList(){"a", "c"}, new Fluent("isRunning")),
                new ByCauses("push", new AgentsList(){"a", "b", "c"}, new Not(new Fluent("isRunning"))),
            };
        }
        [Test]
        public void Test1()
        {
            var query = new NecessaryExecutableFrom(new Instruction()
            {
                new System.Tuple<Action, AgentsList>("push", new AgentsList(){"a", "b"})
            }, new Not(new Fluent("isRunning")));

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(true, answer);
        }

        [Test]
        public void Test2()
        {
            var query = new NecessaryExecutableFrom(new Instruction()
            {
                new System.Tuple<Action, AgentsList>("push", new AgentsList(){"a", "c"})
            }, new Not(new Fluent("isRunning")));

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(true, answer);
        }

        [Test]
        public void Test3()
        {
            var query = new NecessaryAfterFrom(new Instruction()
            {
                new System.Tuple<Action, AgentsList>("push", new AgentsList(){"a", "b"})
            }, "isRunning", new Not(new Fluent("isRunning")));

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(true, answer);
        }

        [Test]
        public void Test4()
        {
            var query = new NecessaryAfterFrom(new Instruction()
            {
                new System.Tuple<Action, AgentsList>("push", new AgentsList(){"a", "c"})
            }, "isRunning", new Not(new Fluent("isRunning")));

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(true, answer);
        }

        [Test]
        public void Test5()
        {
            var query = new NecessaryEngaged(new AgentsList() { "a" }, new System.Collections.Generic.List<Action>() {"push" });

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(true, answer);
        }

        [Test]
        public void Test6()
        {
            var query = new PossiblyEngaged(new AgentsList() { "b" }, new System.Collections.Generic.List<Action>() { "push" });

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(true, answer);
        }

        [Test]
        public void Test7()
        {
            var query = new PossiblyEngaged(new AgentsList() { "c" }, new System.Collections.Generic.List<Action>() { "push" });

            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            var answer = prologService.GetSolution(story, query);
            Assert.AreEqual(true, answer);
        }
    }
}
