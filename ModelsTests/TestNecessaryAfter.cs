using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;

namespace MultiAgentLanguageModelsTests
{
    public class TestNecessaryAfter
    {
        [Test]
        public void Test1()
        {
            string story = @"
Fluent hasA
Fluent hasB
Agent g
Action buypaper
buypaper by [g] causes [hasA || hasB]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = new ExpressionsList();
            expressions.AddRange(parserState.Expression);
            expressions.AddRange(parserState.Noninertial.Values);

            string query = @"
necessary [hasA] after (buypaper, [g])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }

        [Test]
        public void Test2()
        {
            string story = @"
Fluent hasA
Fluent hasB
Agent g
Action buypaper
buypaper by [g] causes [hasA || hasB]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = new ExpressionsList();
            expressions.AddRange(parserState.Expression);
            expressions.AddRange(parserState.Noninertial.Values);

            string query = @"
necessary [hasA || hasB] after (buypaper, [g])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test3()
        {
            string story = @"
Fluent hasA
Fluent hasB
Agent g
Action buypaper
buypaper by [g] causes [hasA || hasB]
initially [hasB]
buypaper by [g] releases [hasA]
buypaper by [g] releases [hasB]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = new ExpressionsList();
            expressions.AddRange(parserState.Expression);
            expressions.AddRange(parserState.Noninertial.Values);

            string query = @"
necessary [hasA] after (buypaper, [g])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }

        [Test]
        public void Test4()
        {
            string story = @"
Fluent hasA
Fluent hasB
Agent g
Action buypaper
buypaper by [g] causes [hasA || hasB]
initially [hasB]
buypaper by [g] releases [hasA]
buypaper by [g] releases [hasB]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = new ExpressionsList();
            expressions.AddRange(parserState.Expression);
            expressions.AddRange(parserState.Noninertial.Values);

            string query = @"
necessary [hasA || hasB] after (buypaper, [g])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(true, res);
        }
    }
}
