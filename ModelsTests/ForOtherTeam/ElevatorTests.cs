using MultiAgentLanguageGUI;
using MultiAgentLanguageModels.Queries;
using NUnit.Framework;
using System.Text;

namespace MultiAgentLanguageModelsTests.ForOtherTeam
{
    /// <summary>
    /// Tomek i Marek znajdują się na parterze budynku i chcą się dostać na wysokie piętro. 
    /// Jedynym sposobem aby to zrobić jest winda, pod warunkiem, że działa.
    /// Jeżeli winda nie działa, to przyjeżdża serwisant i może (ale nie musi) ją naprawić. 
    /// Winda jest jednoosobowa. Jeżeli obaj spróbują skorzystać z niej jednocześnie to ją zepsują i mogą nie dojechać do celu.    
    /// </summary>
    [TestFixture(Category = "ForOtherTeam")]
    public class ElevatorTests
    {        
        private ParserState _parserState;


        [SetUp]
        public void SetUp()
        {
            var sb = new StringBuilder();

            //declarations
            sb.AppendLine(@"
Action useElevator
Action repair
Agent tomek
Agent marek
Agent serwisant
Fluent tomekIsUpstairs
Fluent marekIsUpstairs
Fluent elevatorIsWorking");

            //initially
            sb.AppendLine(@"
initially [~tomekIsUpstairs && ~marekIsUpstairs && ~elevatorIsWorking]");

            //impossible actions
            sb.AppendLine(@"
repair not by [tomek,marek]
impossible useElevator if [~elevatorIsWorking]");

            //actions causes
            sb.AppendLine(@"
useElevator by [marek,tomek] causes [~elevatorIsWorking]
useElevator by [marek] causes [marekIsUpstairs] if [~marekIsUpstairs]
useElevator by [marek] causes [~marekIsUpstairs] if [marekIsUpstairs]
useElevator by [tomek] causes [tomekIsUpstairs] if [~tomekIsUpstairs]
useElevator by [tomek] causes [~tomekIsUpstairs] if [tomekIsUpstairs]");

            //TODO usunąć nawiasy przy fluentach po naprawieniu parsera
            //actions releases
            sb.AppendLine(@"
repair by [serwisant] releases [elevatorIsWorking] if [~elevatorIsWorking]
useElevator by [marek,tomek] releases [tomekIsUpstairs]
useElevator by [marek,tomek] releases [marekIsUpstairs]
");

            string story = sb.ToString();            
            var tokens = Tokenizer.Tokenize(story);
            _parserState = Parser.Parse(tokens);
        }

        #region Executable

        [Test]
        public void NecessaryExecutable_Repair_UseElevatorByOne_False()
        {
            //TODO Usunąć stan początkowy
            var query = "necessary executable (repair, [serwisant]), (useElevator, [tomek, marek]) from [~tomekIsUpstairs && ~marekIsUpstairs && ~elevatorIsWorking]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.False);
        }

        [Test]
        public void PossiblyExecutable_Repair_UseElevatorByOne_True()
        {
            //TODO Usunąć stan początkowy
            var query = "possibly executable (repair, [serwisant]), (useElevator, [tomek]) from  [~tomekIsUpstairs && ~marekIsUpstairs && ~elevatorIsWorking]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }

        [Test]
        public void PossiblyExecutable_UseElevator_If_Broken_False()
        {
            var query = "possibly executable (useElevator, [tomek]) from [~elevatorIsWorking]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.False);
        }

        #endregion

        #region After

        [Test]
        public void Possibly_BothUpstairs_After_UseElevatorAtTheSameTime_True()
        {
            var query = "possibly [marekIsUpstairs && tomekIsUpstairs] after (useElevator, [tomek, marek]) from [~tomekIsUpstairs && ~marekIsUpstairs && elevatorIsWorking]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }


        [Test]
        public void Necessary_Broken_After_useElevatorAtTheSameTime_True()
        {
            var query = "necessary [~elevatorIsWorking] after (useElevator,[tomek,marek]) from [~tomekIsUpstairs && ~marekIsUpstairs && elevatorIsWorking]";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }

        #endregion

        #region Engaged

        [Test]
        public void NecessaryEngaged_Serwisant_In_Repair_True()
        {
            var query = "necessary [serwisant] engaged in (repair, [serwisant])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.True);
        }


        [Test]
        public void PossiblyEngaged_Tomek_In_Repair_False()
        {
            var query = "necessary [tomek] engaged in (repair, [tomek])";

            Query q = Parser.ParseQuery(Tokenizer.Tokenize(query), _parserState);
            var result = q.Solve(_parserState.Story);

            Assert.That(result, Is.False);
        }

        #endregion
    }
}
