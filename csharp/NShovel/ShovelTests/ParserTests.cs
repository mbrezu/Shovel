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
    public class ParserTests
    {
        static void ParserErrorMessageHelper (string source, Action<ShovelException> exceptionTest)
        {
            var sources = Shovel.Api.MakeSources ("test.sho", source);
            var tokenizer = new Shovel.Compiler.Tokenizer (sources [0]);
            var parser = new Shovel.Compiler.Parser (tokenizer.Tokens, sources);
            Utils.ExpectException<ShovelException> (() => {
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
        }

        [Test]
        public void ParserErrorRequiredPrimitive()
        {
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

        [Test]
        public void ParseResult()
        {
                        var source = @"
var fact = fn n if n == 0 1 else n * fact(n - 1)
";
            var sources = Shovel.Api.MakeSources ("test.sho", source);
            var tokenizer = new Shovel.Compiler.Tokenizer (sources [0]);
            var parser = new Shovel.Compiler.Parser (tokenizer.Tokens, sources);
            var sb = new StringBuilder();
            foreach (var pt in parser.ParseTrees) {
                sb.Append (pt.ToString());
            }
            Assert.AreEqual (@"FileName (0 -- 0) 'test.sho'
Var (1 -- 48)
  Name (5 -- 8) 'fact'
  Fn (12 -- 48)
    List (15 -- 15)
      Name (15 -- 15) 'n'
    If (17 -- 48)
      Call (20 -- 25)
        Prim0 (22 -- 23) '=='
        Name (20 -- 20) 'n'
        Number (25 -- 25) '0'
      Number (27 -- 27) '1'
      Call (34 -- 48)
        Prim0 (36 -- 36) '*'
        Name (34 -- 34) 'n'
        Call (38 -- 48)
          Name (38 -- 41) 'fact'
          Call (43 -- 47)
            Prim0 (45 -- 45) '-'
            Name (43 -- 43) 'n'
            Number (47 -- 47) '1'
", sb.ToString());
        }

        [Test]
        public void WeaveAtMostOneReplacement()
        {
            var source = @"
var f = fn (x, y) x + y
3 -> f($, $)
";
            var sources = Shovel.Api.MakeSources("test.sho", source);
            var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
            var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
            Utils.ExpectException<ShovelException>(() => { var x = parser.ParseTrees; }, (ex) => {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"More than one replacement in postfix call (->, $).
file 'test.sho' line 3: 3 -> f($, $)
file 'test.sho' line 3:           ^".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });            
        }

        [Test]
        public void WeaveAtLeastOneReplacement()
        {
            var source = @"
var f = fn (x, y) x + y
3 -> f(4, 5)
";
            var sources = Shovel.Api.MakeSources("test.sho", source);
            var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
            var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
            Utils.ExpectException<ShovelException>(() => { var x = parser.ParseTrees; }, (ex) =>
            {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"No replacement in postfix call (->, $).
file 'test.sho' line 3: 3 -> f(4, 5)
file 'test.sho' line 3: ^^^^^^^^^^^^".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });                        
        }

        [Test]
        public void CollectArgumentNotLast()
        {
            var source = @"
var f = fn (...x, y) x + y
";
            var sources = Shovel.Api.MakeSources("test.sho", source);
            var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
            var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
            Utils.ExpectException<ShovelException>(() => { var x = parser.ParseTrees; }, (ex) =>
            {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"Collect arguments (e.g. ""...rest"") allowed only at the end of the argument list.
file 'test.sho' line 2: var f = fn (...x, y) x + y
file 'test.sho' line 2:                ^".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });                        
        }

        [Test]
        public void CollectArgumentMultiple()
        {
            var source = @"
var f = fn (...x, ...y) x + y
";
            var sources = Shovel.Api.MakeSources("test.sho", source);
            var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
            var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
            Utils.ExpectException<ShovelException>(() => { var x = parser.ParseTrees; }, (ex) =>
            {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"Only one collect argument (e.g. ""...rest"") allowed.
file 'test.sho' line 2: var f = fn (...x, ...y) x + y
file 'test.sho' line 2:                      ^".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });
        }
    }
}

