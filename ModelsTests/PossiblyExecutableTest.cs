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

        [Test]
        public void Test4()
        {
            string str = @"
            Agent Monica
            Action eat
            Action buy
            Fluent hungry
            Fluent has_food
            
            initially [~has_food && hungry]
            buy releases has_food
            eat causes [~hungry] if [has_food]
            ";

            string query = @"
            possibly executable(buy, [Monica]),(eat, [Monica]) from[~has_food && hungry]";


            Assert.AreEqual(true, TestQuery(str, query));
        }

        [Test]
        public void Test5()
        {
            string str = @"
            Agent Tomek
            Action sing
            Fluent is_neighbour_angry
            
            initially [~is_neighbour_angry]
            sing by [Tomek] releases [is_neighbour_angry]
            ";

            string query = @"
            possibly executable(sing, [Tomek]) from []";

            Assert.AreEqual(true, TestQuery(str, query));
        }

        public bool TestQuery(string str, string query)
        {
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            return res;
        }
    }
}
