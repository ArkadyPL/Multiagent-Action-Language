using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;

namespace MultiAgentLanguageModelsTests
{
    public class TestPossiblyEngeged
    {

        [Test]
        public void Test0()
        {
            string story = @"
Fluent R1
Fluent R2
Fluent R3
Agent g1
Agent g2
Agent g3
Agent g4
Action A1
Action A2
A1 by [g1, g2, g3] causes [R1]
A1 by [g1, g3] causes [R3]
A2 by [g2, g3, g4] causes [R2]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = new ExpressionsList();
            expressions.AddRange(parserState.Expression);
            expressions.AddRange(parserState.Noninertial.Values);

            string query = @"
possibly [g2] engaged in (A1, [g1, g2, g3]),(A2, [g2, g3, g4])
";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(true, res);
        }

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
possibly [g] engaged in (buypaper, [g])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test2()
        {
            string story = @"
Fluent hasA
Fluent hasB
Agent g
Agent h
Action buypaper
Action buyOtherPaper
buypaper by [g] causes [hasA]
buypaper by [h] causes [hasA]
buyOtherPaper by [g] causes [hasB]
buyOtherPaper by [h] causes [hasB]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
possibly [g] engaged in buypaper
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
Agent c
Agent d
Action sing
Action fly
sing by [d] causes [hasA]
sing by [c] causes [hasA]
fly by [d] causes [hasB]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = new ExpressionsList();
            expressions.AddRange(parserState.Expression);
            expressions.AddRange(parserState.Noninertial.Values);

            string query = @"
possibly [d] engaged in fly, sing
";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(true, res);
        }
    }
}
