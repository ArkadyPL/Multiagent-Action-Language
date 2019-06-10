using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;

namespace ExecutableQuery
{
    class PossiblyExecutableTest
    {
        string YaleShootingProblemStory = @"
Agent a
Fluent loaded
Fluent alive
Action LOAD
Action SHOOT
initially [~loaded]
initially [alive]
LOAD causes [loaded]
SHOOT causes [~loaded]
SHOOT causes [~alive] if [loaded]
";

        [Test]
        public void Test1()
        {
            // GIVEN
            var tokens = Tokenizer.Tokenize(YaleShootingProblemStory);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
possibly executable (LOAD, [a]) from [~loaded]
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test2()
        {
            string YaleShootingProblemStory = @"
Agent a
Agent b
Fluent loaded
Fluent alive
Action LOAD
Action SHOOT
initially [~loaded]
initially [alive]
LOAD causes [loaded]
SHOOT causes [~loaded]
SHOOT causes [~alive] if [loaded]
impossible SHOOT by [b] if [~loaded || loaded]
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(YaleShootingProblemStory);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
possibly executable (SHOOT, [b]) from [~loaded]
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(false, res);
        }

        [Test]
        public void Test3()
        {
            string YaleShootingProblemStory = @"
Agent a
Agent b
Fluent loaded
Fluent alive
Action LOAD
Action SHOOT
initially [~loaded]
initially [alive]
LOAD causes [loaded]
SHOOT causes [~loaded]
SHOOT causes [~alive] if [loaded]
impossible SHOOT by [b] if [loaded || ~loaded]
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(YaleShootingProblemStory);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
possibly executable (SHOOT, [b]) from [~loaded]
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(false, res);
        }
    }
}
