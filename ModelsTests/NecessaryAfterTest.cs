using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;

namespace AfterQuery
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
            var expressions = parserState.Story;

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
            var expressions = parserState.Story;

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
            var expressions = parserState.Story;

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
            var expressions = parserState.Story;

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
        public void Test5()
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
necessary [loaded] after (LOAD, [a]), (SHOOT, [a]) from [~loaded]
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }
        [Test]
        public void Test6()
        {
            string str = @"
Agent g1
Agent g2
Fluent f
Fluent g
Action a
Action b
a causes [f] 
b causes [g] 
initially [~f && ~g] 
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [f && g] after (a, [g1]), (b, [g2])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test7()
        {
            string str = @"
Agent x
Agent y
Fluent f
Fluent g
Action a
Action b
a by [x] causes [f] 
a by [y] causes [g] 
initially [~f && ~g]
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [f && g] after (a, [x]), (a, [y])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test8()
        {
            string str = @"
Action fire
Agent x
Fluent loaded
Fluent alive
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
initially [alive] 
[~alive] after (fire, [x])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [alive] after ()
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test9()
        {
            string str = @"
Action fire
Agent x
Fluent loaded
Fluent alive
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
initially [alive] 
[~alive] after (fire, [x])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [loaded] after ()
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test10()
        {
            string str = @"
Action fire
Action walk
Agent x
Fluent loaded
Fluent alive
Fluent walked
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
walk causes [walked]
initially [alive] 
[~alive] after (walk, [x]), (fire, [x])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [loaded] after (walk, [x])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test11()
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

        [Test]
        public void Test12()
        {
            string str = @"
Action fire
Action spin
Fluent loaded
Fluent alive
Fluent walked
fire causes [~loaded] 
fire causes [~alive] if [loaded] 
spin releases loaded
spin causes [loaded || ~loaded]
initially [alive] 
observable [~alive] after (spin, []), (fire, [])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [loaded] after (spin, [])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(false, res);
        }

        [Test]
        public void Test13()
        {
            string str = @"
Action fire
Action spin
Fluent loaded
Fluent alive
fire causes [~loaded] 
fire causes [~alive] if [loaded]
spin causes [loaded]
initially [alive] 
observable [alive] after (spin, []), (fire, [])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [loaded] after (spin, [])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }
        [Test]
        public void Test14()
        {
            string str = @"
Action fire
Action load
Action spin
Fluent loaded
Fluent alive
Fluent spinned
fire causes [~loaded] 
fire causes [~alive] if [loaded]
load causes [loaded]
spin causes [spinned || ~spinned]
spin releases spinned
initially [alive] 
[~alive] after (load, []), (spin, []), (fire, [])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [~alive] after (load, []), (spin, []), (fire, [])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test15()
        {
            string str = @"
Action fire
Action spin
Fluent alive
Fluent spinnedA
Fluent spinnedB
fire causes [~alive]
spin causes [spinnedA || spinnedB]
spin releases spinnedA
spin releases spinnedB
initially [(alive && ~spinnedA) && ~spinnedB] 
[~alive] after (spin, []), (fire, [])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [spinnedA] after (spin, [])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(false, res);
        }

        [Test]
        public void Test16()
        {
            string str = @"
Action fire
Action load
Action spin
Fluent loaded
Fluent alive
Fluent spinnedA
Fluent spinnedB
fire causes [~loaded] 
fire causes [~alive] if [loaded]
load causes [loaded]
spin causes [spinnedA || spinnedB]
spin releases spinnedA
spin releases spinnedB
initially [alive] 
[~alive] after (load, []), (spin, []), (fire, [])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
necessary [spinnedA || spinnedB] after (load, []), (spin, [])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }

        [Test]
        public void Test17()
        {
            string str = @"
                Action washing
                Action cooking
                Action tyding
                Fluent washed
                Fluent cooked
                Fluent tied
                Fluent clean
                cooking causes [cooked]
                washing causes [washed]
                tyding causes [tied]
                [clean] after (cooking,[]),(tyding,[])
                ";
            string query = @"
            necessary [clean] after (washing, []),(cooking, []),(tyding, [])
            ";           
            Assert.AreEqual(true, TestQuery(str, query));
        }

        [Test]
        public void Test18()
        {
            string str = @"
                Action washing
                Action cooking
                Fluent washed
                Fluent cooked
                cooking causes [cooked]
                washing causes [washed]
                [cooked && washed] after (cooking,[]), (washing,[])
                ";
            string query = @"
            necessary [washed] after (washing, [])
            ";
            Assert.AreEqual(true, TestQuery(str, query));
        }

        [Test]
        public void Test19()
        {
            string str = @"
                Agent Tom
                Agent John
                Action work
                Fluent a
                Fluent b
                work causes [b]
                [a] after (work,[John,Tom])
                ";
            string query = @"
            necessary [a] after (work,[John,Tom])
            ";
            Assert.AreEqual(true, TestQuery(str, query));
        }

        [Test]
        public void Test20()
        {
            string str = @"
                Agent Magda
                Agent Irena
                Agent Alice
                Agent Anna
                Action eat
                Action talk
                Fluent z
                Fluent d
                Fluent e
                [d && e] after (talk,[Irena,Alice])
                [d] after (eat,[Magda,Anna])
                ";
            string query = @"
            necessary [d && e] after (eat,[Magda,Anna]),(talk,[Irena,Alice])
            ";
            Assert.AreEqual(true, TestQuery(str, query));
        }

        [Test]
        public void Test21()
        {
            string str = @"
                Agent Milena
                Action does
                Fluent f
                always [f]
                [~f] after (does,[Milena])
                ";
            string query = @"
            necessary [f] after (does,[Milena])
            ";
            Assert.AreEqual(true, TestQuery(str, query));
        }

        [Test]
        public void Test22()
        {
            string str = @"
                Agent Iwona
                Action make
                Fluent g
                [~g] after (make,[Iwona])
                make by [Iwona] causes [g]
                ";
            string query = @"
            necessary [~g] after (make,[Iwona])
            ";
            Assert.AreEqual(true, TestQuery(str, query));
        }

        [Test]
        public void Test23()
        {
            string str = @"
                Agent Ula
                Action swim
                Fluent h
                Fluent i
                always [h]
                swim by [Ula] causes [i] if [h]
                ";
            string query = @"
            necessary [i] after (swim,[Ula])
            ";
            Assert.AreEqual(true, TestQuery(str, query));
        }

        [Test]
        public void Test24()
        {
            string str = @"
                Agent Ula
                Action swim
                Fluent h
                Fluent i
                swim by [Ula] causes [i]
                swim by [Ula] causes [h]
                ";
            string query = @"
            necessary [h] after (swim,[Ula])
            ";
            Assert.AreEqual(true, TestQuery(str, query));
        }

        [Test]
        public void Test25()
        {
            string str = @"
                Agent Hank
                Action does
                Fluent h
                does by [Hank] causes [h || ~h]
                does by [Hank] releases [h]
                ";
            string query = @"
            necessary [h] after (does,[Hank])
            ";
            Assert.AreEqual(false, TestQuery(str, query));
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
