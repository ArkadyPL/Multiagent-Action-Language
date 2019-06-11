using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;

namespace AfterQuery
{
    public class PossiblyAfterTest
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
initially [hasB]
buypaper by [g] releases hasA
buypaper by [g] releases hasB
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
possibly [hasA] after (buypaper, [g])
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
Action buypaper
initially [hasA]
buypaper by [g] causes [hasA || hasB]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
possibly [hasA] after (buypaper, [g])
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
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
possibly [hasA || hasB] after (buypaper, [g])
";

            Query q = Parser.ParseQuery(
                Tokenizer.Tokenize(query),
                parserState);

            var res = q.Solve(expressions);

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
possibly [loaded] after (LOAD, [a]), (SHOOT, [a]) from [~loaded]
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(false, res);
        }
        
        [Test]
        public void Test5()
        {
            string str = @"
Agent a
Fluent switch2
Fluent light
Fluent switch1
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

        [Test]
        public void Test6()
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
possibly [loaded] after (spin, [])
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
Action fire
Action spin
Fluent loaded
Fluent alive
fire causes [~loaded] 
fire causes [~alive] if [loaded]
spin causes [loaded]
initially [alive] 
observable [~alive] after (spin, []), (fire, [])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
possibly [loaded] after (spin, [])
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
Action spin
Fluent loaded
Fluent alive
fire causes [~loaded] 
fire causes [~alive] if [loaded]
spin causes [loaded]
initially [alive] 
[~alive] after (spin, []), (fire, [])
";
            // GIVEN
            var tokens = Tokenizer.Tokenize(str);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            // WHEN
            string query = @"
possibly [loaded] after (spin, [])
";
            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), parserState);
            var res = q.Solve(expressions);

            // THEN
            Assert.AreEqual(true, res);
        }
        [Test]
        public void YSPPossiblyLoaded_AfterLoad()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
								possibly [loaded] after (load, [g])
								";

            Query q = Parser.ParseQuery(
                    Tokenizer.Tokenize(query),
                    parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(true, res);

        }

        [Test]
        public void YSPPossiblyLoaded_AfterLoadLoad()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            var query4 = @"
								possibly [loaded] after (load, [g]),(load, [g]),(load, [g])
								";

            var q4 = Parser.ParseQuery(
                 Tokenizer.Tokenize(query4),
                 parserState);

            var res4 = q4.Solve(expressions);

            Assert.AreEqual(true, res4);


        }
        [Test]
        public void YSPPossiblyLoaded_AfterLoadLoadLoad()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            var query5 = @"
								possibly [loaded] after (load, [g]),(load, [g]),(load, [g])
								";

            var q5 = Parser.ParseQuery(
                 Tokenizer.Tokenize(query5),
                 parserState);

            var res5 = q5.Solve(expressions);

            Assert.AreEqual(true, res5);
        }
        [Test]
        public void YSPPossiblyLoaded_AfterLoadShootLoad()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            var query6 = @"
								possibly [loaded] after (load, [g]),(shoot, [g]),(load, [g])
								";

            var q6 = Parser.ParseQuery(
                 Tokenizer.Tokenize(query6),
                 parserState);

            var res6 = q6.Solve(expressions);

            Assert.AreEqual(true, res6);
        }
        [Test]
        public void YSPPossiblyLoad_AfterLoad()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
initially [loaded]
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            var query7 = @"
            possibly [loaded]  after ()
            ";

            var q7 = Parser.ParseQuery(
                 Tokenizer.Tokenize(query7),
                 parserState);

            var res7 = q7.Solve(expressions);

            Assert.AreEqual(true, res7);
        }
        [Test]
        public void YSPNotPossiblyLoaded_AfterLoadShoot()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            var query8 = @"
possibly [loaded] after (load, [g]),(shoot, [g])
";

            var q8 = Parser.ParseQuery(
                 Tokenizer.Tokenize(query8),
                 parserState);

            var res8 = q8.Solve(expressions);

            Assert.AreEqual(false, res8);
        }
        [Test]
        public void YSPNotPossiblyLoad_AfterLoadLoadShoot()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            var query9 = @"
possibly [loaded] after (load, [g]),(load, [g]),(shoot, [g])
";

            var q9 = Parser.ParseQuery(
                 Tokenizer.Tokenize(query9),
                 parserState);

            var res9 = q9.Solve(expressions);

            Assert.AreEqual(false, res9);
        }
        [Test]
        public void YSPNotPossiblyLoaded_AfterLoadLoadShootLoadShoot()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            var query10 = @"
possibly [loaded] after (load, [g]),(load, [g]),(shoot, [g]),(load, [g]),(shoot, [g])
";

            var q10 = Parser.ParseQuery(
                 Tokenizer.Tokenize(query10),
                 parserState);

            var res10 = q10.Solve(expressions);

            Assert.AreEqual(false, res10);

        }


        [Test]
        public void YSPPossiblyDead_AfterLoad()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
								possibly [~alive] after (load, [g])
								";

            Query q = Parser.ParseQuery(
                    Tokenizer.Tokenize(query),
                    parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }
        [Test]
        public void YSPPossiblyAlive_AfterLoad()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
possibly [alive] after (load, [g])
			";

            Query q = Parser.ParseQuery(
                                        Tokenizer.Tokenize(query),
                                        parserState);

            var res = q.Solve(expressions);
            //there are initial states where you start from \alive, and it stays that way
            Assert.AreEqual(false, res);
        }

        [Test]
        public void YSPPossiblyDead_AfterShoot()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
possibly [~alive] after (shoot, [g])
								";

            Query q = Parser.ParseQuery(
                    Tokenizer.Tokenize(query),
                    parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }
        [Test]
        public void YSPPossiblyAlive_AfterShoot()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
possibly [alive] after (shoot, [g])
";

            Query q = Parser.ParseQuery(
                    Tokenizer.Tokenize(query),
                    parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }


        [Test]
        public void YSPPossiblyDead_AfterShoot_InintiallyAlive()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
initially [alive]
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
								possibly [~alive] after (shoot, [g])
								";

            Query q = Parser.ParseQuery(
                    Tokenizer.Tokenize(query),
                    parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }

        [Test]
        public void YSPPossiblyDead_AfterShoot_InintiallyDead()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
initially [~alive]
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
								possibly [~alive] after (shoot, [g])
								";

            Query q = Parser.ParseQuery(
                    Tokenizer.Tokenize(query),
                    parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(true, res);
        }
        [Test]
        public void YSPNotPossiblyAlive_AfterShoot_InintiallyDead()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
initially [~alive]
Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
								possibly [alive] after (shoot, [g])
								";

            Query q = Parser.ParseQuery(
                    Tokenizer.Tokenize(query),
                    parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }


        [Test]
        public void YSPNotPossiblyWalking_AfterShoot()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
initially [alive]
initially [~walking]
initially [loaded]

Agent g
Action shoot
shoot by [g] causes [~alive] if [loaded]
shoot by [g] causes [~loaded]
Action load
load by [g] causes [loaded]
Action entice
entice by [g] causes [walking] if [alive]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
								possibly [walking] after (shoot, [g]), (entice, [g])
								";

            Query q = Parser.ParseQuery(
                            Tokenizer.Tokenize(query),
                            parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }


        [Test]
        public void YSPNOAgents()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
initially [alive]
initially [~walking]
initially [loaded]

Action shoot
shoot causes [~alive] if [loaded]
shoot  causes [~loaded]
Action load
load  causes [loaded]
Action entice
entice causes [walking] if [alive]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
								possibly [walking] after (shoot, []), (entice, [])
								";

            Query q = Parser.ParseQuery(
                            Tokenizer.Tokenize(query),
                            parserState);

            var res = q.Solve(expressions);

            Assert.AreEqual(false, res);
        }

        [Test]
        public void YSPNOAgents_CanKillWalkingChicken()
        {
            string story = @"
Fluent loaded
Fluent walking
Fluent alive
initially [alive]
initially [walking]
initially [loaded]

Action shoot
shoot causes [~alive] if [loaded]
";
            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
								possibly [alive] after (shoot, [])
								";

            Query q = Parser.ParseQuery(
                            Tokenizer.Tokenize(query),
                            parserState);

            var res = q.Solve(expressions);

						Assert.AreEqual(false, res);
				}


				[Test]
				public void YSPDrunkenAgents_CanChickenSurvive()
				{
						string story = @"
Fluent drunk
Fluent alive


initially [alive]
initially [~drunk]

Agent bill

Action shoot
shoot by[bill] causes[~alive] if [~drunk]

Action drink
drink by [bill] causes [drunk] if [~drunk]



";

            var tokens = Tokenizer.Tokenize(story);
            var parserState = Parser.Parse(tokens);
            var expressions = parserState.Story;

            string query = @"
								possibly [alive] after (drink, [bill]),(shoot, [bill])
								";

						Query q = Parser.ParseQuery(
										Tokenizer.Tokenize(query),
										parserState);

						var res = q.Solve(expressions);

						Assert.AreEqual(true, res);
				}


				[Test]
				public void YSPRealeaseDrunkenAgents_CanChickenSurvive()
				{
						string story = @"
Fluent drunk
Fluent alive


initially [alive]
initially [~drunk]

Agent bill

Action shoot
shoot by[bill] causes[~alive] if [~drunk]

Action drink
drink by [bill] causes [drunk || ~drunk]
drink by [bill] releases drunk

";

						var tokens = Tokenizer.Tokenize(story);
						var parserState = Parser.Parse(tokens);
						var expressions = parserState.Story;

						string query = @"
								possibly [alive] after (drink, [bill]),(shoot, [bill])
								";

						Query q = Parser.ParseQuery(
										Tokenizer.Tokenize(query),
										parserState);

						var res = q.Solve(expressions);

						Assert.AreEqual(true, res);
				}

		}

}


