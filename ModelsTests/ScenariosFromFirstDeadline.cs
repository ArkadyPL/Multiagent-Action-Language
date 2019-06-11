using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace MultiAgentLanguageModelsTests
{
    class ScenariosFromFirstDeadline
    {
        [Test]
        public void Test01()
        {
            string story = @"
Fluent f
Fluent g
Agent x
Agent y
Action a

a by [x] causes [f]
a by [y] causes [g]
initially [~f && ~g]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [f && g] after (a, [x, y])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test02()
        {
            string story = @"
Fluent f
Fluent g
Action a

a causes [f]
a causes [g]
initially [~f && ~g]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [f && g] after (a, [])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test03()
        {
            string story = @"
Fluent f
Fluent g
Agent x
Action a
Action b

a by [x] causes [f]
b by [x] causes [g]
initially [~f && ~g]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [f && g] after (a, [x]), (b, [x])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test04()
        {
            string story = @"
Fluent f
Fluent g
Agent x
Agent y
Action a
Action b

a by [x] causes [f]
b by [y] causes [g]
initially [~f && ~g]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [f && g] after (a, [x]), (b, [y])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test05()
        {
            string story = @"
Fluent f
Fluent g
Agent x
Agent y
Action a
Action b

a by [x] causes [f]
b by [y] causes [g]
initially [~f && ~g]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [f || g] after (a, [x]), (b, [y])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test06()
        {
            string story = @"
Fluent f
Fluent g
Agent x
Agent y
Action a
Action b

a by [x] causes [f]
b by [y] causes [g]
initially [~f && ~g]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [f && ~g] after (a, [x]), (b, [y])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.False(result);
        }

        [Test]
        public void Test07()
        {
            string story = @"
Fluent f
Fluent g
Agent x
Agent y
Action a
Action b

a by [x] causes [f]
b by [y] causes [g]
initially [~f && ~g]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [f && g] after (a, [x]), (b, [x])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test08()
        {
            string story = @"
Fluent loaded
Fluent alive
Action load
Action fire

load causes [loaded] 
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
initially [alive]
[~alive] after (fire, [])
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [~alive] after (fire, [])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test09()
        {
            string story = @"
Fluent loaded
Fluent alive
Action load
Action fire

load causes [loaded] 
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
initially [~loaded && alive]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [~alive] after (fire, [])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.False(result);
        }

        [Test]
        public void Test10()
        {
            string story = @"
Fluent loaded
Fluent alive
Action load
Action fire

load causes [loaded] 
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
initially [~loaded && alive]
[~alive] after (fire, [])
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [~alive && ~loaded] after (load, []), (fire, [])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test11()
        {
            string story = @"
Fluent loaded
Fluent alive
Action load
Action fire

load causes [loaded] 
fire causes [~loaded] 
initially [~loaded && alive]
[~alive] after (fire, [])
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [~alive && ~loaded] after (load, []), (fire, [])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test12()
        {
            string story = @"
Fluent ble
Fluent loaded
Fluent alive
Action cos
Action fire

cos causes [ble] 
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
initially [alive]
[~alive] after (cos, []), (fire, []) 
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [alive] after (cos, [])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.True(result);
        }

        [Test]
        public void Test13()
        {
            string story = @"
Fluent ble
Fluent loaded
Fluent alive
Action cos
Action fire

cos causes [ble] 
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
initially [alive]
[~alive] after (cos, []), (fire, []) 
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
necessary [alive] after (cos, []), (fire, []) 
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var result = q.Solve(expressions);

            Assert.False(result);
        }

    }
}
