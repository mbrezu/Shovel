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
using System.Text;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ConsoleTest
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            //MasterMindBenchmark();
            //SimpleTest ();
            //PostfixTest();
            //IndirectTest();
            //CurryTest();
            //ApplyTestTails();
            //CollectTestTails();
            IndirectHashTest();
            //IndirectArrayTest();
            //AnotherSimpleTest ();
            //SerializerTest ();
            //UdpTest();
        }

        static void IndirectArrayTest()
        {
            var sources = Shovel.Api.MakeSources("test.sho", @"
var x = array(1, 2, 3)
//length(x)
x[0] = 'm'
stringRepresentation(x)
            ");
            try
            {
                //var bytecode = Shovel.Api.GetBytecode (sources);
                //Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                //Console.WriteLine(Shovel.Api.TestRunVm(sources));
                Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
                var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
                foreach (var pt in parser.ParseTrees)
                {
                    //Console.WriteLine(pt);
                }
            }
            catch (Shovel.Exceptions.ShovelException shex)
            {
                Console.WriteLine(shex.Message);
            }
        }

        static void IndirectHashTest()
        {
            var sources = Shovel.Api.MakeSources("test.sho", @"
var x = hash('a', 1)
var getter = fn (obj, propertyName) propertyName + propertyName
var y = hash()
var setter = fn (obj, propertyName, value) y[propertyName] = value
setHandlers(x, getter, setter)
var key = 'whatever'
//x['whatever'] = 10
x[key] = 10
//x[key]
//x.whatever = 10
//x.a = 2
stringRepresentation(x) + ' ' + stringRepresentation(y)
            ");
            try
            {
                //var bytecode = Shovel.Api.GetBytecode (sources);
                Console.WriteLine(Shovel.Api.TestRunVm(sources));
                Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
                var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
                foreach (var pt in parser.ParseTrees)
                {
                    //Console.WriteLine(pt);
                }
            }
            catch (Shovel.Exceptions.ShovelException shex)
            {
                Console.WriteLine(shex.Message);
            }
        }

        static void CollectTestTails()
        {
            var sources = Shovel.Api.MakeSources("test.sho", @"
var fun1 = fn (x, ...rest) { push(rest, x) rest }
var fun2 = fn (...rest1) fun1(rest1[0] + rest1[1], 1, 2)
stringRepresentation(fun2(1, 2))
            ");
            try
            {
                //var bytecode = Shovel.Api.GetBytecode (sources);
                //Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                Console.WriteLine(Shovel.Api.TestRunVm(sources));
                Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
                var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
                foreach (var pt in parser.ParseTrees)
                {
                    //Console.WriteLine(pt);
                }
            }
            catch (Shovel.Exceptions.ShovelException shex)
            {
                Console.WriteLine(shex.Message);
            }
        }

        static void ApplyTestTails()
        {
            var sources = Shovel.Api.MakeSources("test.sho", @"
var fun1 = fn (x) x * x            
var fun2 = fn (x, y) fun1(x + y)
fun1(apply(fun2, array(1, 2)))
            ");
            try
            {
                //var bytecode = Shovel.Api.GetBytecode (sources);
                //Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                Console.WriteLine(Shovel.Api.TestRunVm(sources));
                Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
                var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
                foreach (var pt in parser.ParseTrees)
                {
                    //Console.WriteLine(pt);
                }
            }
            catch (Shovel.Exceptions.ShovelException shex)
            {
                Console.WriteLine(shex.Message);
            }            
        }

        static void CurryTest()
        {
            var sources = Shovel.Api.MakeSources("test.sho", @"
var curry = fn (f, ...args) fn ...extras apply(f, args + extras)
var readableCurry = fn (f, ...args) {
    fn (...extras) {
        apply(f, args + extras)
    }
}
var add = fn (x, y) x + y
var add3 = curry(add, 3)
var add31 = readableCurry(add, 3)
add3(4) + add31(3)
            ");
            try
            {
                //var bytecode = Shovel.Api.GetBytecode (sources);
                //Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                Console.WriteLine(Shovel.Api.TestRunVm(sources));
                Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
                var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
                foreach (var pt in parser.ParseTrees)
                {
                    //Console.WriteLine(pt);
                }
            }
            catch (Shovel.Exceptions.ShovelException shex)
            {
                Console.WriteLine(shex.Message);
            }
        }

        static void IndirectTest()
        {
            var sources = Shovel.Api.MakeSources("test.sho", @"
var slothFactory = fn () {
    var realHash = hash()
    hash('indirectGet', fn name realHash[name],
         'indirectSet', fn (name, value) realHash[name] = value)
}
var a = slothFactory()
a:b = slothFactory()
a:b:c = 10
a:b:d = a:b:c + 1
a:x = a:b
a:x:d + a:x:c
            ");
            try
            {
                //var bytecode = Shovel.Api.GetBytecode (sources);
                //Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                Console.WriteLine(Shovel.Api.TestRunVm(sources));
                //Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
                var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
                foreach (var pt in parser.ParseTrees)
                {
                    //Console.WriteLine(pt);
                }
            }
            catch (Shovel.Exceptions.ShovelException shex)
            {
                Console.WriteLine(shex.Message);
            }
        }

        static void PostfixTest()
        {
            var sources = Shovel.Api.MakeSources("test.sho", @"
var f = fn (n, x) n + x
var g = fn (x, n) n * x
var x = 10
//var z = x -> (fn (x) x + $)
x -> (3 + $) -> f(3, $) -> g($, 2)
            ");
            try
            {
                //var bytecode = Shovel.Api.GetBytecode (sources);
                //Console.WriteLine(Shovel.Api.PrintRawBytecode(sources, true));
                Console.WriteLine(Shovel.Api.TestRunVm(sources));
                var tokenizer = new Shovel.Compiler.Tokenizer(sources[0]);
                var parser = new Shovel.Compiler.Parser(tokenizer.Tokens, sources);
                foreach (var pt in parser.ParseTrees) {
                    //Console.WriteLine(pt);
                }
            }
            catch (Shovel.Exceptions.ShovelException shex)
            {
                Console.WriteLine(shex.Message);
            }
        }

        static void UdpTest ()
        {
            var sources = Shovel.Api.MakeSourcesWithStdlib ("test.sho", @"
var a = 0
stdlib.repeat(10, fn () {
    a = a + 1
    @print(string(a))
})
@print (""What's your name?"")
var b = @readLine()
@print (""Hello, "" + b)
"
            );
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) => {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    Console.WriteLine (args [0].StringValue);
                } else {
                    Console.WriteLine ("do be doo");
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> readLine = (api, args, result) => {
                result.Result = Shovel.Value.Make (Console.ReadLine ());
            };
            var bytecode = Shovel.Api.GetBytecode (sources);

            Shovel.Api.RunVm (bytecode, sources, new Shovel.Callable[] {
                Shovel.Callable.MakeUdp ("print", print, 1),
                Shovel.Callable.MakeUdp ("readLine", readLine, 0),
            }
            );
        }

        static void SerializerTest ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var makeCounter = fn () {
  var counter = 0
  fn () counter = counter + 1
}
var main = fn () {
  var c1 = makeCounter()
  var arr = array(1, 2, 3, 4)
  @print(string(arr[0]))
  @print(string(arr[1]))
  c1()
  @print(string(c1()))
  @stop()
  @print(string(arr[2]))
  @print(string(arr[3]))
}
main()
"
            );
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) => {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    Console.WriteLine (args [0].StringValue);
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> stop = (api, args, result) => {
                result.After = Shovel.UdpResult.AfterCall.Nap;
            };
            var bytecode = Shovel.Api.GetBytecode (sources);
            var userPrimitives = new Shovel.Callable[] {
                Shovel.Callable.MakeUdp ("print", print, 1),
                Shovel.Callable.MakeUdp ("stop", stop, 0),
            };
            Console.WriteLine (Shovel.Api.PrintAssembledBytecode (bytecode));
            var vm = Shovel.Api.RunVm (bytecode, sources, userPrimitives);

            var sw = new System.Diagnostics.Stopwatch ();
            sw.Start ();
            var state = Shovel.Api.SerializeVmState (vm);
            for (var i = 0; i < 9999; i++) {
                state = Shovel.Api.SerializeVmState (vm);
            }
            sw.Stop ();
            Console.WriteLine ("Serialization time: {0}", sw.ElapsedMilliseconds / 1000.0);


            Console.WriteLine (state.Length);
            File.WriteAllBytes ("test.bin", state);

            Shovel.Api.RunVm (bytecode, sources, userPrimitives, state);
        }

        static IEnumerable<Shovel.Callable> GetPrintAndStopUdps ()
        {
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) => {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    Console.WriteLine (args [0].StringValue);
                } else {
                    throw new InvalidOperationException ();
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> stop = (api, args, result) => {
                result.After = Shovel.UdpResult.AfterCall.Nap;
            };
            return new Shovel.Callable[] {
                Shovel.Callable.MakeUdp ("print", print, 1),
                Shovel.Callable.MakeUdp ("stop", stop, 0),
            };
        }

        public static void SimpleTest ()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var point = defstruct(array('x', 'y'))
var point2 = defstruct(array('y', 'x'))
var setX = fn (pt, newX) pt.x = newX
var pt1 = make(point, 1, 2)
setX(pt1, 3)
// The code in mkpt should use optimizations on the second call,
// but notice that we use a different structure and invalidate
// the cache.
var pt2 = make(point2, 1, 2)
setX(pt2, 3)
pt2.y
"
                                                  );
            try {
                //var bytecode = Shovel.Api.GetBytecode (sources);
                Console.WriteLine (Shovel.Api.PrintRawBytecode(sources, true));
                Console.WriteLine (Shovel.Api.TestRunVm(sources));
            } catch (Shovel.Exceptions.ShovelException shex) {
                Console.WriteLine (shex.Message);
            }
        }

        public static void AnotherSimpleTest ()
        {
            var sources = Shovel.Api.MakeSourcesWithStdlib (
                "test.sho", 
                "- 8");
            Console.WriteLine (Shovel.Api.TestRunVm (sources));
        }

        public static Shovel.Value MasterMindRun (Shovel.Instruction[] bytecode, List<Shovel.SourceFile> sources)
        {
            System.Diagnostics.Stopwatch sw = new System.Diagnostics.Stopwatch ();
            sw.Reset ();
            sw.Start ();
            var result = Shovel.Api.TestRunVm (bytecode, sources);
            sw.Stop ();
            Console.WriteLine (sw.ElapsedMilliseconds / 1000.0);
            return result;
        }

        public static void MasterMindBenchmark ()
        {
            var sources = Shovel.Api.MakeSourcesWithStdlib ("mmind", Mastermind ());
            File.WriteAllText ("test.txt", Shovel.Api.PrintAssembledBytecode (sources));
            var bytecode = Shovel.Api.GetBytecode (sources);
            Shovel.Value result = Shovel.Value.Make ();
            for (var i = 0; i < 20; i++) {
                result = MasterMindRun (bytecode, sources);
            }
            foreach (var k in result.ArrayValue) {
                if (k.Kind == Shovel.Value.Kinds.Array) {
                    foreach (var kk in k.ArrayValue) {
                        Console.Write (kk.IntegerValue);
                        Console.Write (" ");
                    }
                    Console.WriteLine ();
                }
            }
        }

        static string Mastermind ()
        {
            return @"
    var breakNumber = fn (n) {
      array((n / 1000) % 10,
            (n / 100) % 10,
            (n / 10) % 10,
            n % 10)
    }
    var allDifferent2 = {
      var map = array(0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0)
      fn (bn) {
        var iter = fn (idx) {
          if idx < 4 { 
            if map[bn[idx]] == 0 {
              map[bn[idx]] = 1
              var result = iter(idx + 1)
              map[bn[idx]] = 0
              result
            } else false
          } else true
        }
        iter(0)
        //0
      }
    }
    var allDifferent = fn (bn) {
      var iter = fn (num, idx) {
        if idx == 4 false
        else num == bn[idx] || iter(num, idx + 1)
      }
      !(iter(bn[0], 1) || iter(bn[1], 2) || iter(bn[2], 3))
    }
    var allDifferent3 = fn (bn) {
      bn[0] != bn[1] && bn[0] != bn[2] && bn[0] != bn[3]
      && bn[1] != bn[2] && bn[1] != bn[3]
      && bn[2] != bn[3]
    }
    var generateOptions = fn () {
      var result = array()
      var iter = fn (n) {
        if n < 10000 {
          var bn = breakNumber(n)
          if allDifferent3(bn) push(result, bn)
          iter(n + 1)
        }
      }
      iter(0)
      result
    }
    //length(generateOptions())
    //stdlib.repeat(9, fn() generateOptions())
    slice(generateOptions(), 0, 10)
";
        }
    }
}
