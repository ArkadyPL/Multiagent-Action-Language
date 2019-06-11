using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace ForOtherTeam
{
		/// <summary>
		/// Jack i Bob poszli na polowanie na jelenie.
		///	Aby polować potrzebują aby ich broń była naładowana.
		/// W polowaniu przeszkadza picie piwa, oczywiście wypicie wielu piw nie powoduje że ktoś jest pijany.
		/// Chyba że piją w tym samym momencie, wtedy zawsze razem się upiją.
		///	Jack jeżeli jest trzeźwy to sam zastrzeli jelenia, jeżeli jest pijany to sam nie da rady zastrzelić.
		///	Bob jeżeli jest trzeźwy może trafić w jelenia i go zastrzelić(ale nie musi), jeżeli jest pijany to nie ma takiej szansy.
		///	Jeżeli strzelają razem to zawsze zastrzelą jelenia, chyba że są obaj pijani.
		///	Jeżeli są obaj pijani to nie mogą strzelać.

		///	Początkowo obaj są trzeźwi.
		/// Broń nie jest naładowana.
		/// Jeleń jest żywy.
		    /// </summary>
		[TestFixture(Category = "ForOtherTeam")]
		public class ShootDeerTest
		{
				private ParserState _parserState;


				[SetUp]
				public void SetUp()
				{
						var sb = new StringBuilder();
						//declarations
						sb.AppendLine(@"
						Agent jack
						Agent bob

						Action load
						Action drink
						Action shoot

						Fluent alive
						Fluent drunkBob
						Fluent drunkJack
						Fluent loaded
						");
						//initially
						sb.AppendLine(@"
						initially [~loaded]
						initially [alive]
						initially [~drunkBob]
						initially [~drunkJack]");

						//impossible actions
						sb.AppendLine(@"
impossible shoot if [drunkJack && drunkBob]");

						//actions causes and releases
						sb.AppendLine(@"
						load by [jack] causes [loaded]
						load by [bob] causes [loaded]


						drink by [bob] causes [drunkBob || ~drunkBob]
						drink by [bob] releases drunkBob
						drink by [jack] causes [drunkJack || ~drunkJack]
						drink by [jack] releases drunkJack
						drink by [jack, bob] causes [drunkJack && drunkBob]

						shoot by [jack] causes [~alive] if [~drunkJack && loaded]

						shoot by [bob] causes [~alive || alive] if [~drunkBob && loaded]
						shoot by [bob] releases alive if [~drunkBob && loaded]

						shoot by [bob, jack] causes [~alive] if [loaded]");


						string story = sb.ToString();
						var tokens = Tokenizer.Tokenize(story);
						_parserState = Parser.Parse(tokens);
				}

				#region Executable
				[Test]
				public void NecessaryExecutable_Shoot_IfNotLoded_False()
				{
						var query = "necessary executable (shoot, [jack, bob]) from [~loaded]";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.False);
				}

				[Test]
				public void NecessaryExecutable_Shoot_IfLodedAndJackNotDrunk_True()
				{
						var query = "necessary executable (shoot, [jack, bob]) from [loaded && ~drunkJack]";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}

				[Test]
				public void PossiblyExecutable_Shoot_IfLoaded_True()
				{
						var query = "possibly executable (shoot, [jack, bob]) from [loaded]";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}


				[Test]
				public void PossiblyExecutable_Shoot_IfBothDrunk_False()
				{
						var query = "possibly executable (shoot, [jack, bob]) from [drunkBob && drunkJack]";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.False);
				}
				[Test]
				public void PossiblyExecutable_Shoot_IfDrunkBob_True()
				{
						var query = "possibly executable (shoot, [jack, bob]) from [drunkBob]";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				[Test]
				public void PossiblyExecutable_Shoot_IfDrunkJack_True()
				{
						var query = "possibly executable (shoot, [jack, bob]) from [drunkJack]";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				#endregion


				#region After
				[Test]
				public void Possibly_loaded_After_LoadByJack_True()
				{
						var query = "possibly [loaded] after (load, [jack])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				[Test]
				public void Possibly_loaded_After_LoadByMaybeDrunkenJack_True()
				{
						var query = "possibly [loaded] after (drink, [jack]),(load, [jack])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}

				[Test]
				public void Possibly_drunkJack_After_Drinking_True()
				{
						var query = "possibly [drunkJack] after (drink, [jack])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				[Test]
				public void Possibly_drunkJack_After_DrinkingByBob_False()
				{
						var query = "possibly [drunkJack] after (drink, [bob])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.False);
				}
				[Test]
				public void Possibly_bothDrunk_After_Drinking_True()
				{
						var query = "possibly [drunkJack && drunkBob] after (drink, [bob, jack])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}


				[Test]
				public void Necessary_alive_After_DrinkByJack_True()
				{
						var query = "necessary [alive] after (drink, [jack])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				[Test]
				public void Necessary_notalive_After_LoadAndShootByJack_True()
				{
						var query = "necessary [~alive] after (load,[jack]), (shoot, [jack])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				[Test]
				public void Possibly_notalive_After_LoadAndShootByBob_True()
				{
						var query = "possibly [~alive] after (load,[bob]), (shoot, [bob])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				[Test]
				public void Possibly_alive_After_LoadAndShootByBob_True()
				{
						var query = "possibly [alive] after (load,[bob]), (shoot, [bob])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				[Test]
				public void Possibly_notalive_After_LoadByJackAndShootByBob_True()
				{
						var query = "possibly [~alive] after (load,[jack]), (shoot, [bob])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}



				#endregion


				#region Engaged

				[Test]
				public void PossiblyEngaged_Jack_In_Load_True()
				{
						var query = "possibly [jack] engaged in (load, [jack])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				[Test]
				public void PossiblyEngaged_Bob_In_Shoot_True()
				{
						var query = "possibly [bob] engaged in (shoot, [jack, bob])";

						Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
						var result = q.Solve(_parserState.Story);

						Assert.That(result, Is.True);
				}
				#endregion
		}
}
