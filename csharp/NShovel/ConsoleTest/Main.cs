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
            //AnotherSimpleTest();
			var v1 = Shovel.ShovelValue.Make ();
			var v2 = Shovel.ShovelValue.Make ();
			v1.Kind = Shovel.ShovelValue.Kinds.Bool;
			v1.BoolValue = true;
			v2.Kind = Shovel.ShovelValue.Kinds.Double;
			v2.DoubleValue = 1.5;
			Console.WriteLine(v1.Kind.ToString());
			Console.WriteLine(v1.BoolValue);
			Console.WriteLine(v2.Kind.ToString());
			Console.WriteLine(v2.BoolValue);
        }

        public static void SimpleTest()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"
var adder = fn (n) fn (x) x + n
var add1 = adder(1)
add1(3)");
            Console.WriteLine (Shovel.Api.PrintRawBytecode(sources));
            Console.WriteLine (Shovel.Api.NakedRunVm(sources));
        }

        public static void AnotherSimpleTest()
        {
            var sources = Shovel.Api.MakeSources ("test.sho", @"true || false");
            Console.WriteLine (Shovel.Api.PrintAssembledBytecode(sources));
            Console.WriteLine(Shovel.Api.NakedRunVm (sources));
        }	

        public static void MasterMindBenchmark ()
        {
//			var sources = Shovel.Api.MakeSourcesWithStdlib ("qsort", "stdlib.sort(array(1, 5, 3, 4, 2), fn (a, b) a < b)");
            var sources = Shovel.Api.MakeSourcesWithStdlib ("mmind", Mastermind ());
            File.WriteAllText ("test.txt", Shovel.Api.PrintAssembledBytecode (sources));
            System.Diagnostics.Stopwatch sw = new System.Diagnostics.Stopwatch();
//			sw.Reset ();
//			sw.Start ();
//			for (int i = 0; i < 3000; i ++) {
//				Shovel.Api.GetBytecode (sources);
//			}
//			sw.Stop ();
//			Console.WriteLine (sw.ElapsedMilliseconds / 1000.0);
            var bytecode = Shovel.Api.GetBytecode (sources);

            sw.Reset();
            sw.Start();
            var result = Shovel.Api.RunVm(bytecode, sources);
            sw.Stop();

            Console.WriteLine(sw.ElapsedMilliseconds / 1000.0);

            //			var result = Shovel.Api.NakedRunVm (sources);
            foreach (var k in (List<object>)result)
            {
                foreach (var kk in (List<object>)k)
                {
                    Console.Write(kk);
                    Console.Write(" ");
                }
                Console.WriteLine();
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
    stdlib.repeat(9, fn() generateOptions())
    slice(generateOptions(), 0, 10)
";
        }
    }
}
