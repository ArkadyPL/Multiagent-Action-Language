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
				public void TestPossibleStates()
				{
						var loaded = new Fluent("loaded");
						var walking = new Fluent("walking");
						var alive = new Fluent("alive");
						var bill = new Agent("bill");
						var agents = new AgentsList(new List<Agent>() { bill });
						var reasoning = new ReasoningEngine();
						var ExpressionsList = new ExpressionsList()
						{
								new Initially(new And(loaded, walking)),
								new Always(new If(walking, alive)),
								new Causes(new Action("load"), loaded),
								new Causes(new Action("shoot"), new Not(loaded)),
								new CausesIf(new Action("shoot"), new Not(alive), loaded),
								
						};

						var resp = reasoning.PossibleStates(ExpressionsList);

						Assert.AreEqual(resp, resp);
				}

				[Test]
				public void ShootTurkey()
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

						string query = @"
possibly [loaded] after (load, [g])
";

						Query q = Parser.ParseQuerry(
								Tokenizer.Tokenize(query),
								parserState);

						var res = q.Solve(expressions);

						Assert.AreEqual(false, res);
				}
		}
}
