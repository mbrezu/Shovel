using System;
using NUnit.Framework;

namespace ShovelTests
{
	[TestFixture]
	public class ParserTests
	{
		static void ParserErrorMessageHelper (string source, Action<Shovel.ShovelException> exceptionTest)
		{
			var sources = Utils.MakeSources ("test.sho", source);
			var tokenizer = new Shovel.Compiler.Tokenizer (sources [0]);
			var parser = new Shovel.Compiler.Parser (tokenizer.Tokens, sources);
			Utils.ExpectException<Shovel.ShovelException> (() => {
				foreach (var pt in parser.ParseTrees) {
					Console.WriteLine (pt);
				}
			},
			exceptionTest);
		}

		[Test]
		public void ParserErrorMessageUnexpectedToken ()
		{
			ParserErrorMessageHelper (@"
b(]",
			ex => {
				Assert.IsNotNull (ex);
				Assert.AreEqual (@"Unexpected token ']'.
file 'test.sho' line 2: b(]
file 'test.sho' line 2:   ^", ex.Message);
				Assert.AreEqual (2, ex.Line);
				Assert.AreEqual (3, ex.Column);
			}
			);
		}

		[Test]
		public void ParserErrorMessageExpectedIdentifier ()
		{
			ParserErrorMessageHelper (@"
var a = fn [x] 1",
			ex => {
				Assert.IsNotNull (ex);
				Assert.AreEqual (@"Expected a identifier, but got '['.
file 'test.sho' line 2: var a = fn [x] 1
file 'test.sho' line 2:            ^", ex.Message);
				Assert.AreEqual (2, ex.Line);
				Assert.AreEqual (12, ex.Column);
			}
			);
			ParserErrorMessageHelper (@"
var fn = 1",
			ex => {
				Assert.IsNotNull (ex);
				Assert.AreEqual (@"Expected a identifier, but got 'fn'.
file 'test.sho' line 2: var fn = 1
file 'test.sho' line 2:     ^^", ex.Message);
				Assert.AreEqual (2, ex.Line);
				Assert.AreEqual (5, ex.Column);
			}
			);
			ParserErrorMessageHelper (@"
var slice = 1",
			ex => {
				Assert.IsNotNull (ex);
				Assert.AreEqual (@"Name 'slice' is reserved for a primitive.
file 'test.sho' line 2: var slice = 1
file 'test.sho' line 2:     ^^^^^", ex.Message);
				Assert.AreEqual (2, ex.Line);
				Assert.AreEqual (5, ex.Column);
			}
			);
		}
	}
}

