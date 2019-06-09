using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;
using System.Text;

namespace MultiAgentLanguageModelsTests.ForOtherTeam
{
    /// <summary>
    /// Jest bardzo ciężka rzecz, którą należy podnieść i odłożyć na półkę. Jack i Bob są silnymi mężczyznami, ale w pojedynkę nie są w stanie
    /// tej rzeczy podnieść. Uda im się to tylko wtedy gdy zrobią to razem. Jeżeli któryś z nich spróbuje swoich sił samemu to ta rzecz spadnie
    /// na podłogę i może się uszkodzić. W początkowym stanie rzecz nie jest uszkodzona. Rzeczy uszkodzonej nie można podnieść.
    /// </summary>
    [TestFixture(Category = "ForOtherTeam")]
    public class PickUpTest
    {        
        private ParserState _parserState;


        [SetUp]
        public void SetUp()
        {
            var sb = new StringBuilder();

            //declarations
            sb.AppendLine(@"
Action pickUp
Agent jack
Agent bob
Fluent onShelf
Fluent broken");

            //initially
            sb.AppendLine(@"
initially [~onShelf && ~broken]");

            //impossible actions
            sb.AppendLine(@"
impossible pickUp if [broken]");

            //actions causes
            sb.AppendLine(@"
pickUp by [jack,bob] causes [onShelf]
pickUp by [jack] causes [~onShelf]
pickUp by [bob] causes [~onShelf]");

            //TODO usunąć nawiasy przy fluentach po naprawieniu parsera
            //actions releases
            sb.AppendLine(@"
pickUp by [bob] causes [broken || ~broken]
pickUp by [bob] releases [broken]
pickUp by [jack] causes [broken || ~broken]
pickUp by [jack] releases [broken]
");

            string story = sb.ToString();            
            var tokens = Tokenizer.Tokenize(story);
            _parserState = Parser.Parse(tokens);
        }

        #region Executable

        [Test]
        public void NecessaryExecutable_PickUp_IfNotBroken_True()
        {
            var query = "necessary executable (pickUp, [jack, bob]) from [~broken]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }

        [Test]
        public void NecessaryExecutable_PickUp_IfBroken_False()
        {
            var query = "necessary executable (pickUp, [jack, bob]) from [broken]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.False);
        }

        [Test]
        public void PossiblyExecutable_PickUp_IfBroken_False()
        {
            var query = "possibly executable (pickUp, [jack, bob]) from [broken]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.False);
        }

        #endregion

        #region After

        [Test]
        public void Possibly_broken_After_PickUpByJack_True()
        {
            var query = "possibly [broken] after (pickUp, [jack])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }

        [Test]
        public void Necessary_broken_After_PickUpByJack_False()
        {
            var query = "necessary [broken] after (pickUp, [jack])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.False);
        }

        [Test]
        public void Necessary_OnShelf_After_PickUpByBoth_True()
        {
            var query = "necessary [onShelf] after (pickUp, [jack, bob])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }

        #endregion

        #region Engaged

        [Test]
        public void PossiblyEngaged_Jack_In_PickUp_True()
        {
            var query = "possibly [jack] engaged in (pickUp, [jack])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }

        [Test]
        public void NecessaryEngaged_JackAndBob_In_PickUp_False()
        {
            var query = "necessary [jack, bob] engaged in (pickUp, [jack, bob])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.False);
        }

        #endregion
    }
}
