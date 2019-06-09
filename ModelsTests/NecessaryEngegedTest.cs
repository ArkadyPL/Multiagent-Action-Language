using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;

namespace EngagedQuery
{
    public class TestNecessaryEngaged
    {

        [Test]
        public void Test0_Unexecutable()
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
A1 by [g1, g3, g4] causes [R1]
A1 by [g1, g3] causes [R3]
A2 by [g3, g4] causes [R2]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [g2] engaged in (A1, [g1, g2, g3, g4]),(A2, [g2, g3, g4])
";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);

            var res = q.Solve(expressions);

            // TODO: Check hypothesis: should be true because it is always necessary when not possible
            Assert.AreEqual(true, res);
        }

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
            var expressions = parserState.Story;

            string query = @"
necessary [g2] engaged in (A1, [g1, g2, g3]),(A2, [g2, g3, g4])
";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test0_2()
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
A2 by [g1, g4] causes [R2]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [g2] engaged in (A1, [g1, g2, g3]),(A2, [g2, g3, g4])
";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
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
            var expressions = parserState.Story;

            string query = @"
        necessary [g] engaged in (buypaper, [g])
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
        necessary [g] engaged in (buypaper, [g, h])
        ";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }

    }
}
