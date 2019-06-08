using MultiAgentLanguageGUI;
using NUnit.Framework;

namespace MultiAgentLanguageModelsTests
{
    public class TestNecessaryAfter
    {
        [Test]
        public void TestRes_Releases()
        {
            string story = @"
Fluent hasA
Fluent hasB
Agent g
buypaper causes (hasA || hasB) by [g]
";
            var tokens = Tokenizer.Tokenize(story);
            var expressions = Parser.Parse(tokens);

            //Assert.AreEqual(resp, resp);
        }
    }
}
