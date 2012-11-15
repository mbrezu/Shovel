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
using System.Collections.Generic;

namespace ShovelTests
{
	[TestFixture]
	public class VmTests
	{
		[Test]
		public void Context1 ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", @"
var id = fn x x
var h = fn (x) context
var g = fn (x) id(h(x))
var f = fn (x) id(g(x))
f(3)
"
			);
			var result = (Dictionary<string, object>)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual (2, result.Keys.Count);
			Assert.IsTrue (result.ContainsKey ("stack"));
			Assert.IsTrue (result.ContainsKey ("environment"));
			Assert.AreEqual (@"Frame starts at:
file 'test.sho' line 3: var h = fn (x) context
file 'test.sho' line 3:             ^
Frame variables are:
x = 3

Frame starts at:
file 'test.sho' line 2: var id = fn x x [...content snipped...]
file 'test.sho' line 2: ^^^^^^^^^^^^^^^
Frame variables are:
id = [...callable...]
h = [...callable...]
g = [...callable...]
f = [...callable...]

", result ["environment"]);
			Assert.AreEqual (@"file 'test.sho' line 3: var h = fn (x) context
file 'test.sho' line 3:                ^^^^^^^
file 'test.sho' line 4: var g = fn (x) id(h(x))
file 'test.sho' line 4:                   ^^^^
file 'test.sho' line 5: var f = fn (x) id(g(x))
file 'test.sho' line 5:                   ^^^^
file 'test.sho' line 6: f(3)
file 'test.sho' line 6: ^^^^
", result ["stack"]);
		}

		[Test]
		public void Context2()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", @"
var h = fn (x) context
var g = fn (x) h(x)
var f = fn (x) g(x)
f(3)
"
			);
			var result = (Dictionary<string, object>)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual (2, result.Keys.Count);
			Assert.IsTrue (result.ContainsKey ("stack"));
			Assert.IsTrue (result.ContainsKey ("environment"));
			Assert.AreEqual (@"Frame starts at:
file 'test.sho' line 2: var h = fn (x) context
file 'test.sho' line 2:             ^
Frame variables are:
x = 3

Frame starts at:
file 'test.sho' line 2: var h = fn (x) context [...content snipped...]
file 'test.sho' line 2: ^^^^^^^^^^^^^^^^^^^^^^
Frame variables are:
h = [...callable...]
g = [...callable...]
f = [...callable...]

", result ["environment"]);
			Assert.AreEqual (@"file 'test.sho' line 2: var h = fn (x) context
file 'test.sho' line 2:                ^^^^^^^
file 'test.sho' line 5: f(3)
file 'test.sho' line 5: ^^^^
", result ["stack"]);
		}
	}
}

