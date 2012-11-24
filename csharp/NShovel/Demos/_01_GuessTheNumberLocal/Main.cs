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
                    Console.Write (args [0].StringValue);
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> printLn = (api, args, result) => {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    Console.WriteLine (args [0].StringValue);
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
