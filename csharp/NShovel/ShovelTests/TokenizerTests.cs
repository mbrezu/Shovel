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
using System.Text;
using Shovel.Exceptions;

namespace ShovelTests
{
    [TestFixture]
    public class TokenizerTests
    {
        [Test]
        public void SimpleTest ()
        {
            var text = "var a = 20";
            var source = new Shovel.SourceFile () {
                FileName = "test.sho",
                Content = text
            };
            var tokenizer = new Shovel.Compiler.Tokenizer (source);

            // Expect 5 tokens because a 'filename' token is inserted.
            Assert.AreEqual (5, tokenizer.Tokens.Count);

            var sb = new StringBuilder ();
            sb.AppendLine ();
            foreach (var token in tokenizer.Tokens) {
                sb.AppendFormat (
                    "[Token: Type={0}, StartPos={1}, EndPos={2}, Content='{3}']\n", 
                    token.Type, token.StartPos, token.EndPos, token.Content);
            }

            Assert.AreEqual (@"
[Token: Type=FileName, StartPos=0, EndPos=0, Content='test.sho']
[Token: Type=Keyword, StartPos=0, EndPos=2, Content='var']
[Token: Type=Identifier, StartPos=4, EndPos=4, Content='a']
[Token: Type=Punctuation, StartPos=6, EndPos=6, Content='=']
[Token: Type=Number, StartPos=8, EndPos=9, Content='20']
", sb.ToString ());
        }

        [Test]
        public void TestUnfinishedLiteralString ()
        {
            var text = "var a = '20";
            var source = new Shovel.SourceFile () {
                FileName = "test.sho",
                Content = text
            };
            Utils.ExpectException<ShovelException> (() => {
                Assert.IsNotNull (new Shovel.Compiler.Tokenizer (source).Tokens);
            },
            (ex) => {
                Assert.IsNotNull (ex);
                Assert.AreEqual ("Expected an end quote, but reached the end of file.", ex.Message);
                Assert.AreEqual ("test.sho", ex.FileName);
                Assert.AreEqual (true, ex.AtEof);
            }
            );
        }

        [Test]
        public void TestUnknownCharacter ()
        {
            var text = "var a = $";
            var source = new Shovel.SourceFile () {
                FileName = "test.sho",
                Content = text
            };
            Utils.ExpectException<ShovelException> (() => {
                Assert.IsNotNull (new Shovel.Compiler.Tokenizer (source).Tokens);
            },
            (ex) => {
                Assert.IsNotNull (ex);
                Assert.AreEqual (@"Unexpected character '$'.
file 'test.sho' line 1: var a = $
file 'test.sho' line 1:         ^", ex.Message);
                Assert.AreEqual ("test.sho", ex.FileName);
                Assert.AreEqual (false, ex.AtEof);
                Assert.AreEqual (1, ex.Line);
                Assert.AreEqual (9, ex.Column);
            }
            );
        }
    }
}

