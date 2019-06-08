using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;

namespace MultiAgentLanguageModelsTests
{
    public class TestNecessaryExecutable
    {
        [Test]
        public void Test1()
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
SHOOT causes [~loaded] if [loaded]
SHOOT causes [~alive] if [loaded]
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(YaleShootingProblemStory);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary executable (SHOOT, [a]) from [~loaded]
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }



        [Test]
        public void Test2()
        {
            string str = @"
Agent a
Fluent loaded
Fluent alive
Action LOAD
Action SHOOT
initially [~loaded]
initially [alive]
LOAD releases [loaded]
SHOOT causes [~loaded] if [loaded]
SHOOT causes [~alive] if [loaded]
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary executable (LOAD, [a]), (SHOOT, [a]) from [~loaded]
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(false, res);
        }


        [Test]
        public void Test3()
        {
            string str = @"
Agent a
Fluent loaded
Fluent alive
Action LOAD
Action SHOOT
initially [~loaded]
initially [alive]
LOAD causes [loaded]
SHOOT causes [~loaded] if [loaded]
SHOOT causes [~alive] if [loaded]
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary executable (LOAD, [a]), (SHOOT, [a]) from [~loaded]
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test4()
        {
            string str = @"
Agent a
Fluent loaded
Fluent alive
Action LOAD
Action SHOOT
initially [~loaded]
initially [alive]
impossible LOAD by [a] if [loaded || ~loaded]
SHOOT causes [~loaded] if [loaded]
SHOOT causes [~alive] if [loaded]
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary executable (LOAD, [a]), (SHOOT, [a]) from [~loaded]
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(false, res);
        }
    }
}

