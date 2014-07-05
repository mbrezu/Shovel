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
using Shovel.Exceptions;

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
            var result = Shovel.Api.TestRunVm (sources);
            Assert.IsTrue (result.Kind == Shovel.Value.Kinds.Hash);
            Assert.AreEqual (2, result.HashValue.Value.Keys.Count);
            Assert.IsTrue(result.HashValue.Value.ContainsKey(Shovel.Value.Make("stack")));
            Assert.IsTrue(result.HashValue.Value.ContainsKey(Shovel.Value.Make("environment")));
            Assert.AreEqual (@"Frame starts at:
file 'test.sho' line 3: var h = fn (x) context
file 'test.sho' line 3:             ^
Frame variables are:
x = 3

Frame starts at:
file 'test.sho' line 2: var id = fn x x
file 'test.sho' line 2: ^
Frame variables are:
id = [...callable...]
h = [...callable...]
g = [...callable...]
f = [...callable...]

", result.HashValue.Value[Shovel.Value.Make("environment")].StringValue.Value.TrimCarriageReturn());
            Assert.AreEqual (@"file 'test.sho' line 3: var h = fn (x) context
file 'test.sho' line 3:                ^^^^^^^
file 'test.sho' line 4: var g = fn (x) id(h(x))
file 'test.sho' line 4:                   ^^^^
file 'test.sho' line 5: var f = fn (x) id(g(x))
file 'test.sho' line 5:                   ^^^^
file 'test.sho' line 6: f(3)
file 'test.sho' line 6: ^^^^
", result.HashValue.Value[Shovel.Value.Make("stack")].StringValue.Value.TrimCarriageReturn());
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
            var result = Shovel.Api.TestRunVm (sources);
            Assert.IsTrue (result.Kind == Shovel.Value.Kinds.Hash);
            Assert.AreEqual(2, result.HashValue.Value.Keys.Count);
            Assert.IsTrue(result.HashValue.Value.ContainsKey(Shovel.Value.Make("stack")));
            Assert.IsTrue(result.HashValue.Value.ContainsKey(Shovel.Value.Make("environment")));
            Assert.AreEqual (@"Frame starts at:
file 'test.sho' line 2: var h = fn (x) context
file 'test.sho' line 2:             ^
Frame variables are:
x = 3

Frame starts at:
file 'test.sho' line 2: var h = fn (x) context
file 'test.sho' line 2: ^
Frame variables are:
h = [...callable...]
g = [...callable...]
f = [...callable...]

", result.HashValue.Value[Shovel.Value.Make("environment")].StringValue.Value.TrimCarriageReturn());
            Assert.AreEqual (@"file 'test.sho' line 2: var h = fn (x) context
file 'test.sho' line 2:                ^^^^^^^
file 'test.sho' line 5: f(3)
file 'test.sho' line 5: ^^^^
", result.HashValue.Value[Shovel.Value.Make("stack")].StringValue.Value.TrimCarriageReturn());
        }

        [Test]
        public void Factorial ()
        {
            Utils.TestValue (Utils.FactorialOfTenProgram (), Shovel.Value.Kinds.Integer, (long)3628800);
        }

        [Test]
        public void Fibonacci ()
        {
            Utils.TestValue (Utils.FibonacciOfTenProgram (), Shovel.Value.Kinds.Integer, (long)89);
        }

        [Test]
        public void QuickSort ()
        {
            var sources = Shovel.Api.MakeSourcesWithStdlib ("qsort.sho", Utils.QsortProgram ());
            var result = Shovel.Api.TestRunVm (sources);
            Assert.IsTrue (result.Kind == Shovel.Value.Kinds.Array);
            Assert.AreEqual(5, result.ArrayValue.Value.Count);
            Assert.AreEqual(1, result.ArrayValue.Value[0].IntegerValue.Value);
            Assert.AreEqual(2, result.ArrayValue.Value[1].IntegerValue.Value);
            Assert.AreEqual(3, result.ArrayValue.Value[2].IntegerValue.Value);
            Assert.AreEqual(4, result.ArrayValue.Value[3].IntegerValue.Value);
            Assert.AreEqual(5, result.ArrayValue.Value[4].IntegerValue.Value);
        }

        [Test]
        public void NonLocalReturn ()
        {
            Utils.TestValue (@"
var h = fn x x + 2
var g = fn x h(x) + 2
var f = fn x block 'f' g(x) + 2
f(1)
", Shovel.Value.Kinds.Integer, (long)7);

            Utils.TestValue (@"
var h = fn x return 'f' 10
var g = fn x h(x) + 2
var f = fn x block 'f' g(x) + 2
f(1)
", Shovel.Value.Kinds.Integer, (long)10);

            Utils.TestValue (@"
var b = fn () return 'a' null
var a = fn () {
    block 'a' b()
    10
}
a()
", Shovel.Value.Kinds.Integer, (long)10);

        }

        [Test]
        public void ManyArgs2 ()
        {
            Utils.TestValue (@"
var g = fn (a, b) a - b
var f = fn (a, b) g(a, b) + 1
f(1, 2)
", Shovel.Value.Kinds.Integer, (long)0);
        }

        [Test]
        public void ManyArgs3 ()
        {
            Utils.TestValue (@"
var g = fn (a, b, c) (a - b) * c
var f = fn (a, b, c) g(a, b, c) + 1
f(1, 2, 3)
", Shovel.Value.Kinds.Integer, (long)-2);
        }

        [Test]
        public void ManyArgs4 ()
        {
            Utils.TestValue (@"
var g = fn (a, b, c, d) (a - b) * (c - d)
var f = fn (a, b, c, d) g(a, b, c, d) + 1
f(1, 2, 3, 5)
", Shovel.Value.Kinds.Integer, (long)3);
        }

        [Test]
        public void UserDefinedPrimitives ()
        {
            List<string> log = new List<string> ();
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) => {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    log.Add (args [0].StringValue.Value);
                } else {
                    throw new InvalidOperationException ();
                }
            };
            var sources = Shovel.Api.MakeSourcesWithStdlib ("test.sho", @"
var a = 0
stdlib.repeat(3, fn () {
    a = a + 1
    @print(string(a))
})
"
            );
            var bytecode = Shovel.Api.GetBytecode (sources);
            Shovel.Api.RunVm (bytecode, sources, new Shovel.Callable[] {
                Shovel.Callable.MakeUdp ("print", print, 1),
            }
            );
            Assert.AreEqual (3, log.Count);
            Assert.AreEqual ("1", log [0]);
            Assert.AreEqual ("2", log [1]);
            Assert.AreEqual ("3", log [2]);
        }

        [Test]
        public void StopAndWakeUp ()
        {
            List<string> log = new List<string> ();
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var a = ""hello, ""
var b = ""world""
@stop()
@print(string(a + b))
"
            );
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, false);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            Shovel.Api.WakeUpVm (vm);
            Shovel.Api.RunVm (vm, sources, userPrimitives);
            Assert.AreEqual (1, log.Count);
            Assert.AreEqual ("hello, world", log [0]);
        }

        [Test]
        public void StopWakeUpAndRetry ()
        {
            List<string> log = new List<string> ();
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var a = ""hello, ""
var b = @stop()
@print(string(a + b))
"
            );
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, true);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            Shovel.Api.WakeUpVm (vm);
            Shovel.Api.RunVm (vm, sources, userPrimitives);
            Assert.AreEqual (1, log.Count);
            Assert.AreEqual ("hello, world", log [0]);
        }

        [Test]
        public void StopSerializeWakeUpAndRetry ()
        {
            List<string> log = new List<string> ();
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var a = ""hello, ""
var b = @stop()
@print(string(a + b))
"
            );
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, true);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState (vm);
            Shovel.Api.RunVm (bytecode, sources, userPrimitives, state);
            Assert.AreEqual (1, log.Count);
            Assert.AreEqual ("hello, world", log [0]);
        }

        [Test]
        public void StopSerializeWakeUp ()
        {
            List<string> log = new List<string> ();
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var a = ""hello, ""
var b = ""world""
@stop()
@print(string(a + b))
"
            );
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, false);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState (vm);
            Shovel.Api.RunVm (bytecode, sources, userPrimitives, state);
            Assert.AreEqual (1, log.Count);
            Assert.AreEqual ("hello, world", log [0]);
        }

        [Test]
        public void SerializeSimpleObjects ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var makeCounter = fn () {
  var counter = 0
  fn () counter = counter + 1
}
var c1 = makeCounter()
var c2 = makeCounter()
c1()
c1()
c2()
c2()
c2()
@stop()
@print(string(c1()))
@print(string(c2()))
"
            );
            List<string> log = new List<string> ();
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, false);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState (vm);
            Shovel.Api.RunVm (bytecode, sources, userPrimitives, state);
            Assert.AreEqual (2, log.Count);
            Assert.AreEqual ("3", log [0]);
            Assert.AreEqual ("4", log [1]);
        }

        [Test]
        public void SerializeWithReturnAddressOnStack ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var makeCounter = fn () {
  var counter = 0
  fn () counter = counter + 1
}
var c1 = makeCounter()
var c2 = makeCounter()
var main = fn () {
  c1()
  c1()
  c2()
  c2()
  c2()
  @stop()
  @print(string(c1()))
  @print(string(c2()))
}
main()
"
            );
            List<string> log = new List<string> ();
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, false);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState (vm);
            Shovel.Api.RunVm (bytecode, sources, userPrimitives, state);
            Assert.AreEqual (2, log.Count);
            Assert.AreEqual ("3", log [0]);
            Assert.AreEqual ("4", log [1]);
        }

        [Test]
        public void SerializeWithArray ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var main = fn () {
  var arr = array(1, 2, 3, 4)
  @print(string(arr[0]))
  @print(string(arr[1]))
  @stop()
  @print(string(arr[2]))
  @print(string(arr[3]))
}
main()
"
            );
            List<string> log = new List<string> ();
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, false);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState (vm);
            Shovel.Api.RunVm (bytecode, sources, userPrimitives, state);
            Assert.AreEqual (4, log.Count);
            Assert.AreEqual ("1", log [0]);
            Assert.AreEqual ("2", log [1]);
            Assert.AreEqual ("3", log [2]);
            Assert.AreEqual ("4", log [3]);
        }

        [Test]
        public void SerializeWithStruct ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var point = defstruct(array('x', 'y'))
var main = fn () {
  var pt = make(point, 1, 2)
  @stop()
  @print(string(pt.x))
  @print(string(pt.y))
}
main()
"
            );
            List<string> log = new List<string> ();
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, false);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState (vm);
            Shovel.Api.RunVm (bytecode, sources, userPrimitives, state);
            Assert.AreEqual (2, log.Count);
            Assert.AreEqual ("1", log [0]);
            Assert.AreEqual ("2", log [1]);
        }

        [Test]
        public void SerializeWithHash ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var main = fn () {
  var h = hash('a', 1, 'b', 2)
  @print(string(h.a))
  @stop()
  @print(string(h.b))
}
main()
"
            );
            List<string> log = new List<string> ();
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, false);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState (vm);
            Shovel.Api.RunVm (bytecode, sources, userPrimitives, state);
            Assert.AreEqual (2, log.Count);
            Assert.AreEqual ("1", log [0]);
            Assert.AreEqual ("2", log [1]);
        }

        [Test]
        public void NonLocalReturnAndCachedFrames ()
        {
            Utils.TestValue (@"
var g = fn (x) if x == 3 return 'b' 10 else x
var f = fn (x) g(x) + 3
block 'b' f(2) + f(3)
", Shovel.Value.Kinds.Integer, (long)10);
        }

        [Test]
        public void RestoreStateWithWrongBytecode ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var main = fn () {
  var h = hash('a', 1, 'b', 2)
  @print(string(h.a))
  @stop()
  @print(string(h.b))
}
main()
"
            );
            List<string> log = new List<string> ();
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, false);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState (vm);
            sources = Shovel.Api.MakeSources ("test.sho", @"'hello'");
            bytecode = Shovel.Api.GetBytecode (sources);
            Utils.ExpectException<Shovel.Exceptions.BytecodeDoesntMatchState> (() => {
                Shovel.Api.RunVm (bytecode, sources, userPrimitives, state);
            }, 
            ex => {
                Assert.IsNotNull (ex);
            }
            );
        }

        [Test]
        public void CellsQuota ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var makeCounter = fn () {
  var counter = 0
  fn () counter = counter + 1
}
var c1 = makeCounter()
block 'c' {
  block 'b' {
    c1()
  }
}
"
            );
            var bytecode = Shovel.Api.GetBytecode (sources);
            Utils.ExpectException<Shovel.Exceptions.ShovelCellQuotaExceededException> (() => {
                Shovel.Api.RunVm (bytecode, sources, usedCellsQuota: 30);
            },
            (ex) => {
                Assert.IsNotNull (ex);
            }
            );
        }

        [Test]
        public void ExecutedTicksQuota ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var makeCounter = fn () {
  var counter = 0
  fn () counter = counter + 1
}
var c1 = makeCounter()
block 'c' {
  block 'b' {
    c1()
  }
}
c1()
c1()
"
            );
            var bytecode = Shovel.Api.GetBytecode (sources);
            Utils.ExpectException<Shovel.Exceptions.ShovelTicksQuotaExceededException> (() => {
                Shovel.Api.RunVm (bytecode, sources, totalTicksQuota: 20);
            },
            (ex) => {
                Assert.IsNotNull (ex);
            }
            );
        }

        [Test]
        public void TicksUntilNextNapQuota ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var makeCounter = fn () {
  var counter = 0
  fn () counter = counter + 1
}
var c1 = makeCounter()
block 'c' {
  block 'b' {
    c1()
  }
}
c1()
c1()
"
            );
            var bytecode = Shovel.Api.GetBytecode (sources);
            var vm = Shovel.Api.RunVm (bytecode, sources, ticksUntilNextNapQuota: 20);
            Assert.IsFalse (Shovel.Api.VmIsLive (vm));
            Assert.IsFalse (Shovel.Api.VmExecutionComplete (vm));
            Shovel.Api.WakeUpVm (vm);
            Shovel.Api.RunVm (vm, sources, ticksUntilNextNapQuota: 10);
            Assert.IsFalse (Shovel.Api.VmIsLive (vm));
            Assert.IsFalse (Shovel.Api.VmExecutionComplete (vm));
            Shovel.Api.WakeUpVm (vm);
            Shovel.Api.RunVm (vm, sources, ticksUntilNextNapQuota: 20);
            Assert.IsFalse (Shovel.Api.VmIsLive (vm));
            Assert.IsTrue (Shovel.Api.VmExecutionComplete (vm));
        }

        [Test]
        public void UdpError ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
@print(100)
"
            );
            List<string> log = new List<string> ();
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = Utils.GetPrintAndStopUdps (log, false);
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);
            Assert.IsNotNull (Shovel.Api.VmUserDefinedPrimitiveError (vm));
        }

        [Test]
        public void MultipleGrefs ()
        {
            Utils.TestValue ("var a = array(array(array(3))) a[0][0][0]", Shovel.Value.Kinds.Integer, (long)3);
        }

        [Test]
        public void MultilineComments()
        {
            var program = @"
var fun = fn (x, y, z) x + y + z
fun(/* x */ 1, /* y */ 2, /* z */ 3)
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)6);
        }

        [Test]
        public void WeaveChain()
        {
            var program = @"
var f = fn (x, y) x + y
var g = fn (t, u) t * u
3 -> f($, 2) -> g(3, $)
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)15);
        }

        [Test]
        public void WeaveChainNest()
        {
            var program = @"
var f = fn (x, y) x + y
var g = fn (t, u) t * u
3 -> f($, 2) -> g(2 -> (1 + $), $)
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)15);
        }

        [Test]
        public void ReplacementWithoutWeave()
        {
            var program = @"
$";
            Utils.ExpectException<ShovelException>(() =>
            {
                Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)6);
            }, (ex) =>
            {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"'$' without '->'.
file 'test.sho' line 2: $
file 'test.sho' line 2: ^".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });                        
        }

        [Test]
        public void Apply()
        {
            var program = @"
var f = fn (x, y) x + y
var args = array(1, 2)
apply(f, args)
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)3);
        }

        [Test]
        public void ApplyPrimitive()
        {
            var program = @"
var f = fn (x, y, z) x + y + z
var args = apply(array, array(1, 2, 3))
apply(f, args)
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)6);
        }

        [Test]
        public void ApplyArity()
        {
            var program = @"
var f = fn (x, y, z) x + y + z
var args = array(1, 2)
apply(f, args)
";
            Utils.ExpectException<ShovelException>(() => { 
                Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)6);
            }, (ex) => {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"The first argument of 'apply', a function of 3 arguments, was called with 2 arguments.

Current stack trace:
file 'test.sho' line 4: apply(f, args)
file 'test.sho' line 4: ^^^^^

Current environment:

Frame starts at:
file 'test.sho' line 2: var f = fn (x, y, z) x + y + z
file 'test.sho' line 2: ^
Frame variables are:
f = [...callable...]
args = array(1, 2)

".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });            
        }

        [Test]
        public void ApplyCurry()
        {
            var program = @"
var curry = fn (f, ...args) fn ...extras apply(f, args + extras)
var add = fn (x, y) x + y
var add3 = curry(add, 3)
add3(5)
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)8);
        }

        [Test]
        public void ApplyTailCall()
        {
            var program = @"
var g = fn(x, y) x + y
var f = fn (x, y) g(x, y)
apply(f, array(1, 2))
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)3);
        }

        [Test]
        public void CollectArguments()
        {
            var program = @"
var f = fn(x, ...y) x + length(y)
f(3) + f(3, 4, 5)
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)8);
        }

        [Test]
        public void CollectArgumentsTailCall()
        {
            var program = @"
var f = fn(x, ...y) x + length(y)
var g = fn(x, y) f(x, y, 3)
g(1, 2)
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)3);
        }

        [Test]
        public void IndirectGetArray()
        {
            var program = @"
var ar = array(1, 2, 3)
setHandlers(ar, fn (obj, idx) idx - 100, null)
ar[-1]
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)-101);
        }

        [Test]
        public void IndirectSetArray()
        {
            var program = @"
var ar = array(1, 2, 3)
var h = hash()
setHandlers(ar, null, fn (obj, idx, value) h[string(idx)] = value)
ar[-1] = 1001
h['-1']
";
            Utils.TestValue(program, Shovel.Value.Kinds.Integer, (long)1001);
        }

        [Test]
        public void IndirectGetHash()
        {
            var program = @"
var h = hash('a', 1, 'b', 2)
setHandlers(h, fn (obj, index) index + index, null)
h['yo']
";
            Utils.TestValue(program, Shovel.Value.Kinds.String, "yoyo");
        }

        [Test]
        public void IndirectSetHash()
        {
            var program = @"
var h = hash('a', 1, 'b', 2)
var h2 = hash()
setHandlers(h, null, fn (hsh, index, value) h2[index] = value)
h['yo'] = 20
stringRepresentation(h) + ' ' + stringRepresentation(h2)
";
            Utils.TestValue(program, Shovel.Value.Kinds.String, "hash(\"a\", 1, \"b\", 2) hash(\"yo\", 20)");
        }

        [Test]
        public void IndirectHashGetSetAndSerialization()
        {
            List<string> log = new List<string>();
            var sources = Shovel.Api.MakeSources("test.sho", @"
var h = hash('a', 1)
var h2 = hash()
var getter = fn (obj, index) index + index
var setter = fn (obj, index, value) {
    @stop()
    h2[index] = value
}
setHandlers(h, getter, setter)
h.b = 'test'
@print(h2.b)
@print(h.b)
");
            var bytecode = Shovel.Api.GetBytecode(sources);
            var userPrimitives = Utils.GetPrintAndStopUdps(log, true);
            var vm = Shovel.Api.RunVm(bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState(vm);
            Shovel.Api.RunVm(bytecode, sources, userPrimitives, state);
            Assert.AreEqual(2, log.Count);
            Assert.AreEqual("test", log[0]);
            Assert.AreEqual("bb", log[1]);
        }

        [Test]
        public void IndirectArrayGetSetAndSerialization()
        {
            List<string> log = new List<string>();
            var sources = Shovel.Api.MakeSources("test.sho", @"
var ar = array(1, 2, 3)
var h2 = hash()
var getter = fn (obj, index) index * index
var setter = fn (obj, index, value) {
    @stop()
    h2[string(index)] = value
}
setHandlers(ar, getter, setter)
ar[-1] = 'hello'
@print(h2['-1'])
@print(string(ar[-1]))
@print(string(ar[8]))
");
            var bytecode = Shovel.Api.GetBytecode(sources);
            var userPrimitives = Utils.GetPrintAndStopUdps(log, true);
            var vm = Shovel.Api.RunVm(bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState(vm);
            Shovel.Api.RunVm(bytecode, sources, userPrimitives, state);
            Assert.AreEqual(3, log.Count);
            Assert.AreEqual("hello", log[0]);
            Assert.AreEqual("1", log[1]);
            Assert.AreEqual("64", log[2]);
        }

        [Test]
        public void ApplyAndSerialization()
        {
            List<string> log = new List<string>();
            var sources = Shovel.Api.MakeSources("test.sho", @"
var fun = fn (x, y, z) {
    @stop()
    x + y + z
}
var result = string(apply(fun, array(1, 2, 3)))
@print(result)
");
            var bytecode = Shovel.Api.GetBytecode(sources);
            var userPrimitives = Utils.GetPrintAndStopUdps(log, true);
            var vm = Shovel.Api.RunVm(bytecode, sources, userPrimitives);
            var state = Shovel.Api.SerializeVmState(vm);
            Shovel.Api.RunVm(bytecode, sources, userPrimitives, state);
            Assert.AreEqual(1, log.Count);
            Assert.AreEqual("6", log[0]);
        }

        [Test]
        public void ApplyUdp()
        {
            List<string> log = new List<string>();
            var sources = Shovel.Api.MakeSources("test.sho", @"
apply(@print, array('hello'))
");
            var bytecode = Shovel.Api.GetBytecode(sources);
            var userPrimitives = Utils.GetPrintAndStopUdps(log, true);
            var vm = Shovel.Api.RunVm(bytecode, sources, userPrimitives);
            Assert.AreEqual(1, log.Count);
            Assert.AreEqual("hello", log[0]);
        }

        [Test]
        public void Format()
        {
            var program = @"
var product = hash('name', 'paperweight', 'price', 20)
format('The {0} is {1:C2}.', product.name, product.price)
";
            Utils.TestValue(program, Shovel.Value.Kinds.String, "The paperweight is $20.00.");
        }

        [Test]
        public void StringInterpolation()
        {
            var program = @"
var name = 'John'
var quality = 'wise'
'${name} is ${quality}.'
";
            Utils.TestValue(program, Shovel.Value.Kinds.String, "John is wise.");
        }

        [Test]
        public void StringInterpolationEscape()
        {
            var program = @"
var name = 'John'
var quality = 'wise'
'\${name} is ${quality}.'
";
            Utils.TestValue(program, Shovel.Value.Kinds.String, "${name} is wise.");
        }

        [Test]
        public void StringInterpolationError1()
        {
            var program = @"
var name = 'John'
var quality = 'wise'
'${name is ${quality}.'
";
            Utils.ExpectException<ShovelException>(() =>
            {
                Utils.TestValue(program, Shovel.Value.Kinds.String, "John is wise.");
            }, (ex) =>
            {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"While interpolating a string value:
Expected a identifier, but reached the end of input.
file 'test.sho' line 4: '${name is ${quality}.'
file 'test.sho' line 4: ^^^^^^^^^^^^^^^^^^^^^^^".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });

        }

        [Test]
        public void StringInterpolationError2()
        {
            var program = @"
var product = hash('name', 'paperweight', 'price', 19.90)
var names = array('John', 'Smith')
'Mr. ${upper(names[1)}, the ${product.name} is ${product.price:C2}.'
";
            Utils.ExpectException<ShovelException>(() =>
            {
                Utils.TestValue(program, Shovel.Value.Kinds.String, "John is wise.");
            }, (ex) =>
            {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"While interpolating a string value:
Expected ']', but got ')'.
file 'test.sho' line 4: 'Mr. ${upper(names[1)}, the ${product.name} is ${product.price:C2}.'
file 'test.sho' line 4:                     ^
file 'test.sho' line 4: 'Mr. ${upper(names[1)}, the ${product.name} is ${product.price:C2}.'
file 'test.sho' line 4: ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });
        }

        [Test]
        public void StringInterpolationRuntimeError()
        {
            var program = @"
var product = hash('name', 'paperweight', 'price', 19.90)
var names = array('John', 'Smith')
'Mr. ${upper(names[1])}, the ${product.nam} is ${product.price:C2}.'
";
            Utils.ExpectException<ShovelException>(() =>
            {
                Utils.TestValue(program, Shovel.Value.Kinds.String, "John is wise.");
            }, (ex) =>
            {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"Key not found in hash table.

Current stack trace:
file 'test.sho' line 4: 'Mr. ${upper(names[1])}, the ${product.nam} is ${product.price:C2}.'
file 'test.sho' line 4:                                       ^

Current environment:

Frame starts at:
file 'test.sho' line 2: var product = hash('name', 'paperweight', 'price', 19.90)
file 'test.sho' line 2: ^
Frame variables are:
product = hash(""name"", ""paperweight"", ""price"", 19.9)
names = array(""John"", ""Smith"")

".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });
        }

        [Test]
        public void StringInterpolationComplex()
        {
            var program = @"
var product = hash('name', 'paperweight', 'price', 19.90)
var names = array('John', 'Smith')
'Mr. ${upper(names[1])}, the ${product.name} is ${product.price:C2}.'
";
            Utils.TestValue(program, Shovel.Value.Kinds.String, "Mr. SMITH, the paperweight is $19.90.");
        }

        [Test]
        public void StringInterpolationMany()
        {
            var program = @"
var product = hash('name', 'paperweight', 'price', 19.90, 'category', 'stuff')
var names = array('John', 'Smith')
'Mr. ${upper(names[1])}, the ${product.name} is ${product.price:C2} (category ""${product.category}"").'
";
            Utils.TestValue(program, Shovel.Value.Kinds.String, "Mr. SMITH, the paperweight is $19.90 (category \"stuff\").");
        }

        [Test]
        public void StringInterpolationNested()
        {
            var program = @"
var product = hash('name', 'paperweight', 'price', 19.90)
var names = array('John', 'Smith')
'Mr. ${""${names[0]}""} ${upper(names[1])}, the ${product.name} is ${product.price:C2}.'
";
            Utils.TestValue(program, Shovel.Value.Kinds.String, "Mr. John SMITH, the paperweight is $19.90.");
        }

        [Test]
        public void StringInterpolationMissingBracket()
        {
            var program = @"
var product = hash('name', 'paperweight', 'price', 19.90)
var names = array('John', 'Smith')
'The names are: ${names'
";
            Utils.ExpectException<ShovelException>(() =>
            {
                Utils.TestValue(program, Shovel.Value.Kinds.String, "John is wise.");
            }, (ex) =>
            {
                Assert.IsNotNull(ex);
                Assert.AreEqual(@"Missing '}' for '${}' block.
file 'test.sho' line 4: 'The names are: ${names'
file 'test.sho' line 4:                 ^^^^^^^^".TrimCarriageReturn(), ex.Message.TrimCarriageReturn());
                Assert.AreEqual("test.sho", ex.FileName);
            });
        }

    }
}
