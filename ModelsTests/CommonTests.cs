using MultiAgentLanguageModels;
using Ninject;
using NUnit.Framework;
using System;
using System.Linq;
using System.Reflection;
using Action = MultiAgentLanguageModels.Action;

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
        public void GeneratingLogicExpression1()
        {
            var sigma = new Fluent("sigma");
            var sigmaExpression = new LogicExpression(sigma);
            Assert.AreEqual("[sigma]", sigmaExpression.ToProlog());
        }

        [Test]
        public void GeneratingLogicExpression2()
        {
            var sigma = new Fluent("sigma");
            var notSigma = new Not(sigma);
            var notSigmaExpression = new LogicExpression(notSigma);
            Assert.AreEqual(@"[(\sigma)]", notSigmaExpression.ToProlog());
        }
    }
}