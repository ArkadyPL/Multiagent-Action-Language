using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace MultiAgentLanguageModelsTests
{
    class TestPossiblyExecutable
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
        public void YaleShootingProblem_LoadByAny_FromLoaded()
        {
            // GIVEN
            var tokens = Tokenizer.Tokenize(YaleShootingProblemStory);
            var parserState = Parser.Parse(tokens);
            var expressions = new ExpressionsList();
            expressions.AddRange(parserState.Expression);
            expressions.AddRange(parserState.Noninertial.Values);

            // WHEN
            string query = @"
possibly executable (LOAD, [a]) from [~loaded]
";
            Query q = Parser.ParseQuerry(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }
    }
}
