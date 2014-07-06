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
using System.Collections.Generic;

namespace _01_GuessTheNumberLocal
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            var program = @"
var game = fn () {
  var secretNumber = floor(@random() * 100 + 1)
  var attempt = 0
  var iteration = fn () {
    attempt = attempt + 1
    @print('Attempt ' + string(attempt) + ' - ')
    @print('enter a number between 1 and 100: ')
    var guess = @readInt()
    if guess < secretNumber {
      @printLn('Too small!')
      iteration()
    }
    else if guess > secretNumber {
      @printLn('Too large!')
      iteration()
    }
    else {
      @printLn('You guessed it in ' + string(attempt) + ' attempts! Congratulations!')
      @print('Another game? (y/n) ')
      if @readChar() == 'y' game()
    }
  }
  iteration()
}

game()
";
            var sources = Shovel.Api.MakeSources ("test.sho", program);
            try {
                var bytecode = Shovel.Api.GetBytecode (sources);
                Shovel.Api.RunVm (bytecode, sources, Udps ());
            } catch (Shovel.Exceptions.ShovelException shex) {
                Console.WriteLine (shex.Message);
            }
        }

        public static IEnumerable<Shovel.Callable> Udps ()
        {
            var rng = new Random();
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) => {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    Console.Write (args [0].String);
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> printLn = (api, args, result) => {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    Console.WriteLine (args [0].String);
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> readInt = (api, args, result) => {
                int dummy;
                if (!int.TryParse(Console.ReadLine (), out dummy)) {
                    dummy = 0;
                }
                result.Result = Shovel.Value.MakeInt (dummy);
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> readChar = (api, args, result) => {
                var line = Console.ReadLine ();
                if (line.Length > 0) {
                    result.Result = Shovel.Value.Make (line.Substring (0, 1));
                } else {
                    result.Result = Shovel.Value.Make ("");
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> random = (api, args, result) => {
                result.Result = Shovel.Value.MakeFloat (rng.NextDouble());
            };           
            return new Shovel.Callable[] {
                Shovel.Callable.MakeUdp ("print", print, 1),
                Shovel.Callable.MakeUdp ("printLn", printLn, 1),
                Shovel.Callable.MakeUdp ("readInt", readInt, 0),
                Shovel.Callable.MakeUdp ("readChar", readChar, 0),
                Shovel.Callable.MakeUdp ("random", random, 0),
            };
        }
    }
}
