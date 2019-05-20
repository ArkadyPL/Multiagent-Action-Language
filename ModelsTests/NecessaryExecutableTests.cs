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
    public class NecessaryExecutableTests
    {
        Action a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
        Agent g1, g2, g3, g4, g5, g6, g7, g8, gX, gY;
        LanguageStructure s1, s2, s3, s4;
        Fluent result, pi, another, sigma, beta, alpha, other, delta, psi, result3, gamma;

        [SetUp]
        public void SetUp()
        {
            a1 = new Action("action1");
            a2 = new Action("action2");
            g1 = new Agent("g1");
            g2 = new Agent("g2");
            gX = new Agent("gX");
            gY = new Agent("gY");
            result = new Fluent("result");
            pi = new Fluent("pi");
            another = new Fluent("another");
            s1 = new LanguageStructure()
            {
                new ByCausesIf(a1, new AgentsList(){g1, g2}, new LogicExpression(result), new LogicExpression(pi)),
                new ByCausesIf(a2, new AgentsList(){g1, g2}, new LogicExpression(another), new LogicExpression(result))
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
                new System.Tuple<Action, AgentsList>(a1, new AgentsList(){g1, g2}),
                new System.Tuple<Action, AgentsList>(a2, new AgentsList(){g1, g2})
            };

            var query = new NecessaryExecutableFrom(instruction, new LogicExpression(pi));
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
                new System.Tuple<Action, AgentsList>(a1, new AgentsList(){gY, gX}),
                new System.Tuple<Action, AgentsList>(a2, new AgentsList(){gX, gY})
            };

            var query = new NecessaryExecutableFrom(instruction, new LogicExpression(pi));
            var solution = prologService.GetSolution(s1, query);
            Assert.AreEqual(false, solution);
        }
    }
}