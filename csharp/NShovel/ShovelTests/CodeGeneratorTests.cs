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
	public class CodeGeneratorTests
	{
		[Test]
		public void FactorialCode ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", @"
var fact = fn (n) {
    if n == 0
    1
    else n * fact(n - 1)
}
fact(10)
"
			);
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 1A680D29F5B254213B99ACFA46DC51FD
    VMBYTECODEMD5 ?
    FILENAME test.sho
;; file 'test.sho' line 2: var fact = fn (n) { [...content snipped...]
;; file 'test.sho' line 2: ^^^^^^^^^^^^^^^^^^^
    NEWFRAME fact
    JUMP L2
;; file 'test.sho' line 2: var fact = fn (n) { [...content snipped...]
;; file 'test.sho' line 2:            ^^^^^^^^
FN1:
;; file 'test.sho' line 2: var fact = fn (n) {
;; file 'test.sho' line 2:                ^
    NEWFRAME n
    ARGS 1
;; file 'test.sho' line 3:     if n == 0
;; file 'test.sho' line 3:        ^
    LGET 0, 0
;; file 'test.sho' line 3:     if n == 0
;; file 'test.sho' line 3:             ^
    CONST 0
;; file 'test.sho' line 3:     if n == 0
;; file 'test.sho' line 3:          ^^
    EQ
    FJUMP L3
;; file 'test.sho' line 4:     1
;; file 'test.sho' line 4:     ^
    CONST 1
    RETURN
L3:
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:          ^
    LGET 0, 0
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:                   ^
    LGET 0, 0
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:                       ^
    CONST 1
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:                     ^
    SUB
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:              ^^^^
    LGET 1, 0
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:              ^^^^^^^^^^^
    CALL 1
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:            ^
    MUL
    RETURN
L2:
    FN FN1, 1
;; file 'test.sho' line 2: var fact = fn (n) { [...content snipped...]
;; file 'test.sho' line 2: ^^^^^^^^^^^^^^^^^^^
    LSET 0, 0
    POP
;; file 'test.sho' line 7: fact(10)
;; file 'test.sho' line 7:      ^^
    CONST 10
;; file 'test.sho' line 7: fact(10)
;; file 'test.sho' line 7: ^^^^
    LGET 0, 0
;; file 'test.sho' line 7: fact(10)
;; file 'test.sho' line 7: ^^^^^^^^
    CALL 1
    DROPFRAME
", Shovel.Api.PrintRawBytecode (sources));
		}

		[Test]
		public void Empties ()
		{
			var sources = Shovel.Api.MakeSources ();
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 D41D8CD98F00B204E9800998ECF8427E
    VMBYTECODEMD5 ?
    CONST null
", Shovel.Api.PrintRawBytecode (sources));
			sources = Shovel.Api.MakeSources ("test-1", "");
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 D41D8CD98F00B204E9800998ECF8427E
    VMBYTECODEMD5 ?
    FILENAME test-1
    CONST null
", Shovel.Api.PrintRawBytecode (sources));
			sources = Shovel.Api.MakeSources ("test-1", "", "test-2", "");
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 D41D8CD98F00B204E9800998ECF8427E
    VMBYTECODEMD5 ?
    FILENAME test-1
    FILENAME test-2
    CONST null
", Shovel.Api.PrintRawBytecode (sources));
			sources = Shovel.Api.MakeSources ("test-1", "{}");
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 99914B932BD37A50B983C5E7C90AE93B
    VMBYTECODEMD5 ?
    FILENAME test-1
    CONST null
", Shovel.Api.PrintRawBytecode (sources));
			sources = Shovel.Api.MakeSources ("test-1", "{}{}");
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 C53F4EBE9B2A50BC2B52FD88A5D503E1
    VMBYTECODEMD5 ?
    FILENAME test-1
    CONST null
    POP
    CONST null
", Shovel.Api.PrintRawBytecode (sources));
			sources = Shovel.Api.MakeSources ("test-1", "{{}}");
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 3F7A56499D58DE719351A6D324A76BBD
    VMBYTECODEMD5 ?
    FILENAME test-1
    CONST null
", Shovel.Api.PrintRawBytecode (sources));
			sources = Shovel.Api.MakeSources ("test-1", "{{}}{{{}}}");
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 66F99E017EE16ED7E471D3B4830ADF02
    VMBYTECODEMD5 ?
    FILENAME test-1
    CONST null
    POP
    CONST null
", Shovel.Api.PrintRawBytecode (sources));
			sources = Shovel.Api.MakeSources ("test-1", "1", "test-2", "");
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 C4CA4238A0B923820DCC509A6F75849B
    VMBYTECODEMD5 ?
    FILENAME test-1
;; file 'test-1' line 1: 1
;; file 'test-1' line 1: ^
    CONST 1
    FILENAME test-2
", Shovel.Api.PrintRawBytecode (sources));
			sources = Shovel.Api.MakeSources ("test-1", "1", "test-2", "2");
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 C20AD4D76FE97759AA27A0C99BFF6710
    VMBYTECODEMD5 ?
    FILENAME test-1
    FILENAME test-2
;; file 'test-2' line 1: 2
;; file 'test-2' line 1: ^
    CONST 2
", Shovel.Api.PrintRawBytecode (sources));
			sources = Shovel.Api.MakeSources ("test-1", "", "test-2", "2");
			Assert.AreEqual (@"    VMVERSION 1
    VMSOURCESMD5 C81E728D9D4C2F636F067F89CC14862C
    VMBYTECODEMD5 ?
    FILENAME test-1
    FILENAME test-2
;; file 'test-2' line 1: 2
;; file 'test-2' line 1: ^
    CONST 2
", Shovel.Api.PrintRawBytecode (sources));
		}

        [Test]
        public void CompileEnvironments()
        {
            var source = @"
var a = fn() b()
var b = fn() 3
a()
";
            Utils.TestValue(source, Shovel.Value.Kinds.Integer, (long)3);
        }

        [Test]
        public void DuplicateDefinitionError()
        {
            var source = @"
var a = 1
var a = 2
";
            Utils.ExpectException<Shovel.Exceptions.ShovelException>(
            () => {
                Shovel.Api.GetBytecode(Shovel.Api.MakeSources("test.sho", source));
            },
            (ex) => {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"Variable 'a' is already defined in this frame in file 'test.sho', at line 2, column 5.
file 'test.sho' line 3: var a = 2
file 'test.sho' line 3:     ^", ex.Message);
            });
        }

	}
}

