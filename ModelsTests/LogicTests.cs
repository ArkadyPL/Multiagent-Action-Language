using MultiAgentLanguageModels;
using NUnit.Framework;
using System;
using System.Collections.Generic;

namespace Tests
{
    public class LogicTests
    {
        [Test]
        public void GeneratingLogicExpression1()
        {
            var sigma = new Fluent("sigma");
            var sigmaExpression = new LogicExpression(sigma);
            Assert.AreEqual("sigma", sigmaExpression.StringExpression);
        }

        [Test]
        public void GeneratingLogicExpression2()
        {
            var sigma = new Fluent("sigma");
            var notSigma = new Not(sigma);
            var notSigmaExpression = new LogicExpression(notSigma);
            Assert.AreEqual(@"(\sigma)", notSigmaExpression.StringExpression);
        }

        [Test]
        public void LogicExpressionCheck()
        {
            var sigma = new Fluent("sigma");
            var notSigma = new Not(sigma);
            var notSigmaExpression = new LogicExpression(notSigma);
            var actual = notSigmaExpression.EvaluateLogicExpression();
            var expected = new List<List<Tuple<string, bool>>>()
            {
                new List<Tuple<string, bool>>()
                {
                    new Tuple<string, bool>("sigma", false)
                }
            };
            Assert.AreEqual(expected, actual);
        }

        [Test]
        public void LogicExpressionCheck2()
        {
            var sigma = new Fluent("sigma");
            var pi = new Fluent("pi");
            var beta = new Fluent("beta");
            var notSigma = new Not(sigma);
            var ifPiBeta = new If(pi, beta);
            var iffPiBetaSigma = new Iff(ifPiBeta, notSigma);

            var iffExpression = new LogicExpression(iffPiBetaSigma);
            var actual = iffExpression.EvaluateLogicExpression().ToListOfStrings();
            var expected = new List<string>()
            {
                @"[beta, pi, \sigma]",
                @"[beta, \pi, \sigma]",
                @"[\beta, \pi, \sigma]",
                @"[\beta, pi, sigma]"
            };
            var check1 = expected.TrueForAll(x => actual.Contains(x));
            var check2 = actual.TrueForAll(x => expected.Contains(x));
            Assert.True(check1 && check2);
        }

        [Test]
        public void LogicExpressionCheck3()
        {
            var sigma = new Fluent("sigma");
            var falseOrSigma = new Or(sigma, new False());
            var falseOrSigmaExpression = new LogicExpression(falseOrSigma);
            var actual = falseOrSigmaExpression.EvaluateLogicExpression().ToListOfStrings();
            var expected = new List<string>()
            {
                @"[sigma]"
            };
            var check1 = expected.TrueForAll(x => actual.Contains(x));
            var check2 = actual.TrueForAll(x => expected.Contains(x));
            Assert.True(check1 && check2);
        }

        [Test]
        public void LogicExpressionCheck4()
        {
            var sigma = new Fluent("sigma");
            var taut = new Or(sigma, new Not(sigma));
            var tautExpression = new LogicExpression(taut);
            var actual = tautExpression.EvaluateLogicExpression().ToListOfStrings();
            var expected = new List<string>()
            {
                @"[sigma]",
                @"[\sigma]"
            };
            var check1 = expected.TrueForAll(x => actual.Contains(x));
            var check2 = actual.TrueForAll(x => expected.Contains(x));
            Assert.True(check1 && check2);
        }

        [Test]
        public void LogicExpressionCheck5()
        {
            var sigma = new Fluent("sigma");
            var taut = new Iff(sigma, sigma);
            var tautExpression = new LogicExpression(taut);
            var actual = tautExpression.EvaluateLogicExpression().ToListOfStrings();
            var expected = new List<string>()
            {
                @"[sigma]",
                @"[\sigma]"
            };
            var check1 = expected.TrueForAll(x => actual.Contains(x));
            var check2 = actual.TrueForAll(x => expected.Contains(x));
            Assert.True(check1 && check2);
        }
    }
}
