using MultiAgentLanguageGUI;
using MultiAgentLanguageModels;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Queries;
using MultiAgentLanguageModels.Reasoning;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;
using Action = MultiAgentLanguageModels.Action;

namespace MultiAgentLanguageModelsTests
{
		public class YSPTests
		{

				[Test]
				public void YSPPossiblyLoad()
				{
						string story = @"
Fluent loaded
Fluent walking
Fluent alive
Agent g
Action shoot
shoot by [g] causes [\alive] if [loaded]
shoot by [g] causes [\loaded]
Action load
load by [g] causes [loaded]
";
						var tokens = Tokenizer.Tokenize(story);
						var parserState = Parser.Parse(tokens);
						var expressions = new ExpressionsList();
						expressions.AddRange(parserState.Expression);
						expressions.AddRange(parserState.Noninertial.Values);

//						string query = @"
//possibly [loaded] after (load, [g])
//";

//						Query q = Parser.ParseQuerry(
//								Tokenizer.Tokenize(query),
//								parserState);

//						var res = q.Solve(expressions);

//						Assert.AreEqual(true, res);

//						var query4 = @"
//possibly [loaded] after (load, [g]),(load, [g]),(load, [g])
//";

//						var q4 = Parser.ParseQuerry(
//							 Tokenizer.Tokenize(query4),
//							 parserState);

//						var res4 = q.Solve(expressions);

//						Assert.AreEqual(true, res4);



//						var query5 = @"
//possibly [loaded] after (load, [g]),(load, [g]),(load, [g])
//";

//						var q5 = Parser.ParseQuerry(
//							 Tokenizer.Tokenize(query5),
//							 parserState);

//						var res5 = q5.Solve(expressions);

//						Assert.AreEqual(true, res5);

//						var query6 = @"
//possibly [loaded] after (load, [g]),(shoot, [g]),(load, [g])
//";

//						var q6 = Parser.ParseQuerry(
//							 Tokenizer.Tokenize(query6),
//							 parserState);

//						var res6 = q6.Solve(expressions);

//						Assert.AreEqual(true, res6);
#warning MO : todo
						//						var query7 = @"
						//possibly [loaded] from NULL
						//";

						//						var q7 = Parser.ParseQuerry(
						//							 Tokenizer.Tokenize(query7),
						//							 parserState);

						//						var res7 = q6.Solve(expressions);

						//						Assert.AreEqual(true, res7);
						var query8 = @"
possibly [loaded] after (load, [g]),(shoot, [g])
";

						var q8 = Parser.ParseQuerry(
							 Tokenizer.Tokenize(query8),
							 parserState);

						var res8 = q8.Solve(expressions);

						Assert.AreEqual(false, res8);

						var query9 = @"
possibly [loaded] after (load, [g]),(load, [g]),(shoot, [g])
";

						var q9 = Parser.ParseQuerry(
							 Tokenizer.Tokenize(query9),
							 parserState);

						var res9 = q9.Solve(expressions);

						Assert.AreEqual(false, res9);

						var query10 = @"
possibly [loaded] after (load, [g]),(load, [g]),(shoot, [g]),(load, [g]),(shoot, [g])
";

						var q10 = Parser.ParseQuerry(
							 Tokenizer.Tokenize(query10),
							 parserState);

						var res10 = q10.Solve(expressions);

						Assert.AreEqual(false, res10);

				}


				[Test]
				public void YSPPossiblyDead()
				{
				}
		}
}
