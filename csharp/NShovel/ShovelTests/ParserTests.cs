// Copyright (c) 2012, Miron Brezuleanu
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

