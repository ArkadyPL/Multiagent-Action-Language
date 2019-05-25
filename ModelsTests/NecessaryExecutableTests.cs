using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using Ninject;
using NUnit.Framework;
using System.Reflection;

namespace Tests
{
    public class NecessaryExecutableTests
    {
        Action a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a51, a61;
        Agent g1, g2, g3, g4, g5, g6, g7, g8, gX, gY, g51, g52, g61;
        LanguageStructure s1, s2, s3, s4, s5, s6;
        Fluent result, pi, another, sigma, beta, alpha, other, delta, psi, result3, gamma, delta5, psi5, psi6, delta6;

        [SetUp]
        public void SetUp()
        {
            a1 = new Action("action1");
            a2 = new Action("action2");
            a7 = new Action("action7");
            a8 = new Action("a8");
            a9 = new Action("a9");
            a10 = new Action("a10");
            a3 = new Action("a3");
            a4 = new Action("a4");
            a51 = new Action("a51");
            a61 = new Action("a61");
            g1 = new Agent("g1");
            g2 = new Agent("g2");
            gX = new Agent("gX");
            gY = new Agent("gY");
            g7 = new Agent("g7");
            g8 = new Agent("g8");
            g3 = new Agent("g3");
            g4 = new Agent("g4");
            g51 = new Agent("g51");
            g52 = new Agent("g52");
            g61 = new Agent("g61");
            result = new Fluent("result");
            pi = new Fluent("pi");
            another = new Fluent("another");
            sigma = new Fluent("sigma");
            beta = new Fluent("beta");
            alpha = new Fluent("alpha");
            other = new Fluent("o");
            delta5 = new Fluent("d5");
            psi5 = new Fluent("p5");
            psi6 = new Fluent("psi6");
            delta6 = new Fluent("delt");
            s1 = new LanguageStructure()
            {
                new ByCausesIf(a1, new AgentsList(){g1, g2}, new LogicExpression(result), new LogicExpression(pi)),
                new ByCausesIf(a2, new AgentsList(){g1, g2}, new LogicExpression(another), new LogicExpression(result))
            };
            s2 = new LanguageStructure()
            {
                new ByCausesIf(a7, new AgentsList(){g7, g8}, new Not(sigma), sigma),
                new ByCausesIf(a8, new AgentsList(){g7, g8}, sigma, new Not(sigma)),
                new ByCausesIf(a9, new AgentsList(){g7, g8}, sigma, sigma),
                new ByCausesIf(a10, new AgentsList(){g7, g8}, sigma, new Not(sigma))
            };
            s3 = new LanguageStructure()
            {
                new ByCausesIf(a3, new AgentsList(){g3,g4}, beta, alpha),
                new ByCausesIf(a4, new AgentsList(){g3,g4}, other, new And(alpha,beta))
            };
            s5 = new LanguageStructure()
            {
                new ImpossibleBy(a51, new AgentsList(){ g51 }),
                new ByCausesIf(a51, new AgentsList(){ g51 }, delta5, psi5),
                new ByCausesIf(a51, new AgentsList(){ g52 }, delta5, psi5)
            };
            s6 = new LanguageStructure()
            {
                new ByReleasesIf(a61, new AgentsList(){g61}, delta6, psi6)
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

        [Test]
        public void Test21()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a7, new AgentsList(){g7, g8}),
                new System.Tuple<Action, AgentsList>(a8, new AgentsList(){g7, g8})
            };
            var query = new NecessaryExecutableFrom(instruction, sigma);
            var solution = prologService.GetSolution(s2, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test22()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a7, new AgentsList(){g7, g8}),
                new System.Tuple<Action, AgentsList>(a8, new AgentsList(){g7, g8}),
                new System.Tuple<Action, AgentsList>(a9, new AgentsList(){g7, g8})
            };
            var query = new NecessaryExecutableFrom(instruction, sigma);
            var solution = prologService.GetSolution(s2, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test23()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a7, new AgentsList(){g7, g8}),
                new System.Tuple<Action, AgentsList>(a8, new AgentsList(){g7, g8}),
                new System.Tuple<Action, AgentsList>(a10, new AgentsList(){g7, g8})
            };
            var query = new NecessaryExecutableFrom(instruction, sigma);
            var solution = prologService.GetSolution(s2, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public void Test24()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a10, new AgentsList(){g7, g8}),
                new System.Tuple<Action, AgentsList>(a9, new AgentsList(){g7, g8}),
                new System.Tuple<Action, AgentsList>(a8, new AgentsList(){g7, g8})
            };
            var query = new NecessaryExecutableFrom(instruction, sigma);
            var solution = prologService.GetSolution(s2, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public void Test31()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a3, new AgentsList(){g4, g3}),
                new System.Tuple<Action, AgentsList>(a4, new AgentsList(){g3, g4})
            };
            var query = new NecessaryExecutableFrom(instruction, alpha);
            var solution = prologService.GetSolution(s3, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test51()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a51, new AgentsList(){g51})
            };
            var query = new NecessaryExecutableFrom(instruction, psi5);
            var solution = prologService.GetSolution(s5, query);
            Assert.AreEqual(false, solution);
        }

        [Test]
        public void Test52()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a51, new AgentsList(){g52})
            };
            var query = new NecessaryExecutableFrom(instruction, psi5);
            var solution = prologService.GetSolution(s5, query);
            Assert.AreEqual(true, solution);
        }

        [Test]
        public void Test61()
        {
            StandardKernel kernel = new StandardKernel();
            kernel.Load(Assembly.GetExecutingAssembly());
            var prologService = kernel.Get<IPrologService>();
            Instruction instruction = new Instruction()
            {
                new System.Tuple<Action, AgentsList>(a61, new AgentsList(){g61})
            };
            var query = new NecessaryExecutableFrom(instruction, psi6);
            var solution = prologService.GetSolution(s6, query);
            Assert.AreEqual(false, solution);
        }
    }
}