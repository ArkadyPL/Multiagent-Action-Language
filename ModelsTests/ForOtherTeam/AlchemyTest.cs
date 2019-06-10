using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;
using System.Text;

namespace MultiAgentLanguageModelsTests.ForOtherTeam
{
    /// <summary>
    /// Bercik i Filemon to dwaj poczatkujacy alchemicy.
    /// Dostali za zadanie stworzyc magiczna miksture, jednak woleliby lezec
    /// na łózku i nic nie robic. Mieszkaja w nowo wyremontowanej drewnianej 
    /// chatce.Uzupełniaja swoje wady, wiec jesli zabiora sie do warzenia 
    /// mikstury wspólnie, uda im sie.Jednakze, jesli zabierze sie za to 
    /// tylko jeden z nich, mikstura moze nie wyjsc i bedzie trzeba tworzyc 
    /// ja ponownie. Dodatkowo, jesli miksture warzyc bedzie sam Bercik, 
    /// moze sie zdarzyc, ze ta wybuchnie i zniszczy nowiutko wyremontowana
    /// chatke, przez co nie stworza juz mikstury. Z uwagi na lenistwo
    /// alchemików, jesli uda im sie uwarzyc miksture, na pewno nie bedzie
    /// im sie chciało tworzyc kolejnej.
    /// </summary>
    [TestFixture(Category = "ForOtherTeam")]
    public class AlchemyTest
    {
        private ParserState _parserState;


        [SetUp]
        public void SetUp()
        {
            var sb = new StringBuilder();

            //declarations
            sb.AppendLine(@"
            Agent Bercik
            Agent Filemon
            Action brew
            Fluent brewed
            Fluent destroyed");

            //initially
            sb.AppendLine(@"
            initially [~brewed]
            initially [~destroyed]");

            //actions causes and releases
            sb.AppendLine(@"
            brew by [Bercik] causes [brewed || destroyed]
            brew by [Filemon] causes [brewed || ~brewed]
            brew by [Bercik, Filemon] causes [brewed]
            brew by [Filemon] releases [brewed]
            brew by [Filemon] causes [brewed || ~brewed]
            brew by [Bercik] releases [brewed]
            brew by [Bercik] causes [brewed || ~brewed]
            brew by [Bercik] releases [destroyed]
            brew by [Bercik] causes [destroyed || ~destroyed]");

            string story = sb.ToString();
            var tokens = Tokenizer.Tokenize(story);
            _parserState = Parser.Parse(tokens);
        }

        #region Executable

      
        #endregion

        #region After

        [Test]
        public void PossiblyBrewedAfterBrewBercikAndFilemonFromNotBrewedAndNotDestroyed()
        {
            var query = "possibly [brewed] after (brew,[Bercik, Filemon]) from [~brewed && ~destroyed]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }

        [Test]
        public void PossiblyBrewedAfterBrewBercikFromNotBrewedAndNotDestroyed()
        {
            var query = "possibly [brewed] after (brew,[Bercik]) from [~brewed && ~destroyed]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }

        [Test]
        public void PossiblyDestroyedAfterBrewBercikFromNotBrewedAndNotDestroyed()
        {
            var query = "possibly [destroyed] after (brew,[Bercik]) from [~brewed && ~destroyed]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }

        [Test]
        public void PossiblyBrewedAfterBrewFilemonFromNotBrewedAndNotDestroyed()
        {
            var query = "possibly [brewed] after (brew,[Filemon]) from [~brewed && ~destroyed]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }
        #endregion

        #region Engaged

        [Test]
        public void PossibleEngagedFilemonInBrew()
        {
            var query = "possibly [Filemon] engaged in (brew, [])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.True(result);
        }

        [Test]
        public void PossibleEngagedBercikInBrew()
        {
            var query = "possibly [Bercik] engaged in (brew, [])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.True(result);
        }

        [Test]
        public void NecessaryEngagedFilemonInBrew()
        {
            var query = "necessary [Filemon] engaged in (brew, [])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.False(result);
        }

        [Test]
        public void NecessaryEngagedBercikInBrew()
        {
            var query = "necessary [Bercik] engaged in (brew, [])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.False(result);
        }

        [Test]
        public void NecessaryEngagedFilemonAndBercikInBrew()
        {
            var query = "necessary [Filemon, Bercik] engaged in (brew, [Filemon, Bercik])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.True(result);
        }

        #endregion
    }
}
