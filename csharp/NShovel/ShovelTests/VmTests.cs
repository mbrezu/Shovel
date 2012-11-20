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
			var result = Shovel.Api.NakedRunVm (sources);
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Hash);
			Assert.AreEqual (2, result.HashValue.Keys.Count);
			Assert.IsTrue (result.HashValue.ContainsKey (Shovel.ShovelValue.Make("stack")));
			Assert.IsTrue (result.HashValue.ContainsKey (Shovel.ShovelValue.Make("environment")));
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

", result.HashValue [Shovel.ShovelValue.Make("environment")].StringValue);
			Assert.AreEqual (@"file 'test.sho' line 3: var h = fn (x) context
file 'test.sho' line 3:                ^^^^^^^
file 'test.sho' line 4: var g = fn (x) id(h(x))
file 'test.sho' line 4:                   ^^^^
file 'test.sho' line 5: var f = fn (x) id(g(x))
file 'test.sho' line 5:                   ^^^^
file 'test.sho' line 6: f(3)
file 'test.sho' line 6: ^^^^
", result.HashValue [Shovel.ShovelValue.Make("stack")].StringValue);
		}

		[Test]
		public void Context2 ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", @"
var h = fn (x) context
var g = fn (x) h(x)
var f = fn (x) g(x)
f(3)
"
			);
			var result = Shovel.Api.NakedRunVm (sources);
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Hash);
			Assert.AreEqual (2, result.HashValue.Keys.Count);
			Assert.IsTrue (result.HashValue.ContainsKey (Shovel.ShovelValue.Make("stack")));
			Assert.IsTrue (result.HashValue.ContainsKey (Shovel.ShovelValue.Make("environment")));
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

", result.HashValue [Shovel.ShovelValue.Make("environment")].StringValue);
			Assert.AreEqual (@"file 'test.sho' line 2: var h = fn (x) context
file 'test.sho' line 2:                ^^^^^^^
file 'test.sho' line 5: f(3)
file 'test.sho' line 5: ^^^^
", result.HashValue [Shovel.ShovelValue.Make("stack")].StringValue);
		}

		[Test]
		public void Factorial ()
		{
			Utils.TestValue (Utils.FactorialOfTenProgram(), Shovel.ShovelValue.Kinds.Integer, (long)3628800);
		}

		[Test]
		public void Fibonacci()
		{
			Utils.TestValue (Utils.FibonacciOfTenProgram(), Shovel.ShovelValue.Kinds.Integer, (long)89);
		}

		[Test]
		public void QuickSort()
		{
			var sources = Shovel.Api.MakeSourcesWithStdlib("qsort.sho", Utils.QsortProgram());
			var result = Shovel.Api.NakedRunVm(sources);
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Array);
			Assert.AreEqual (5, result.ArrayValue.Count);
			Assert.AreEqual (1, result.ArrayValue[0].IntegerValue);
			Assert.AreEqual (2, result.ArrayValue[1].IntegerValue);
			Assert.AreEqual (3, result.ArrayValue[2].IntegerValue);
			Assert.AreEqual (4, result.ArrayValue[3].IntegerValue);
			Assert.AreEqual (5, result.ArrayValue[4].IntegerValue);
		}

		[Test]
		public void NonLocalReturn()
		{
			Utils.TestValue(@"
var h = fn x x + 2
var g = fn x h(x) + 2
var f = fn x block 'f' g(x) + 2
f(1)
", Shovel.ShovelValue.Kinds.Integer, (long)7);

			Utils.TestValue(@"
var h = fn x return 'f' 10
var g = fn x h(x) + 2
var f = fn x block 'f' g(x) + 2
f(1)
", Shovel.ShovelValue.Kinds.Integer, (long)10);
		}

        [Test]
        public void ManyArgs2()
        {
            Utils.TestValue(@"
var g = fn (a, b) a - b
var f = fn (a, b) g(a, b) + 1
f(1, 2)
", Shovel.ShovelValue.Kinds.Integer, (long)0);
        }

        [Test]
        public void ManyArgs3()
        {
            Utils.TestValue(@"
var g = fn (a, b, c) (a - b) * c
var f = fn (a, b, c) g(a, b, c) + 1
f(1, 2, 3)
", Shovel.ShovelValue.Kinds.Integer, (long)-2);
        }

        [Test]
        public void ManyArgs4()
        {
            Utils.TestValue(@"
var g = fn (a, b, c, d) (a - b) * (c - d)
var f = fn (a, b, c, d) g(a, b, c, d) + 1
f(1, 2, 3, 5)
", Shovel.ShovelValue.Kinds.Integer, (long)3);
        }
	}
}

