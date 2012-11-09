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
    PRIM0 ==
;; file 'test.sho' line 3:     if n == 0
;; file 'test.sho' line 3:        ^^^^^^
    CALL 2
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
    PRIM0 -
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:                   ^^^^^
    CALL 2
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:              ^^^^
    LGET 1, 0
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:              ^^^^^^^^^^^
    CALL 1
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:            ^
    PRIM0 *
;; file 'test.sho' line 5:     else n * fact(n - 1)
;; file 'test.sho' line 5:          ^^^^^^^^^^^^^^^
    CALLJ 2
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
", Shovel.Api.PrintCode (sources));
		}
	}
}

