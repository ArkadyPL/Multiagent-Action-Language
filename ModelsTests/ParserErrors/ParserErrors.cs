using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;

namespace ParserErrors
{
    public class ParserErrors
    {
        /// <summary>
        /// noninertial fluent should be additionally declared as "Fluent light"
        /// this is obligatory for all fluents
        /// It's connected to PossiblyAfter Tests - Test 5
        /// </summary>
        [Test]
        public void Noninertial()
        {
            string str = @"
Agent a
Fluent switch2
Fluent switch1
Fluent light
Action TOGGLE1
Action TOGGLE2
noninertial light
initially [switch1 && switch2]
always [light <-> (switch1 <-> switch2)]
TOGGLE1 causes [~switch1] if [switch1]
TOGGLE1 causes [switch1] if [~switch1]
TOGGLE2 causes [~switch2] if [switch2]
TOGGLE2 causes [switch2] if [~switch2]
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
possibly [light] after (TOGGLE1, [a]), (TOGGLE2, [a])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        /// <summary>
        /// can't parse impossible ACTION by [AGENT] - throws null
        /// connected to Possibly Executable - Test 3
        /// </summary>
        [Test]
        public void ImpossibleBy()
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
impossible SHOOT by [b]
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

        /// <summary>
        /// ACTION releases FLUNET - fluent should be given without [ ] 
        /// connected to Necessary After - Test 11
        /// </summary>
        [Test]
        public void ReleasesFluent()
        {
            string str = @"
Action fire
Action spin
Agent x
Fluent loaded
Fluent alive
Fluent walked
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
spin releases loaded
initially [alive] 
[~alive] after (spin, [x]), (fire, [x])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [loaded] after (spin, [x])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }
    }
}