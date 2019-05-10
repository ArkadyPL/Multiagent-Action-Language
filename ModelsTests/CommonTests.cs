using MultiAgentLanguageModels;
using Ninject;
using NUnit.Framework;
using System;
using System.Linq;
using System.Reflection;

namespace Tests
{
    public class CommonTests
    {
        private IPrologService prologService;
        
        [SetUp]
        public void Setup()
        {
            var kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            prologService = kernel.Get<IPrologService>();
        }

        [Test]
        public void GeneratingAgents()
        {
            var names = Enumerable.Range(0, 10).Select(x => (new Guid()).ToString());
            var agents = names.Select(x => new Agent(x)).ToList();
            names.ToList().Sort();
            Assert.AreEqual($"[{names.ToList().Aggregate((a, b) => a + ", " + b)}]", agents.ToProlog());
        }
    }
}