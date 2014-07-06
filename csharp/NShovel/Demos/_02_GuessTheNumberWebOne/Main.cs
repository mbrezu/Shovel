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
using System.Net;
using System.IO;
using System.Text;
using System.Web;
using System.Collections.Generic;
using System.Threading;

namespace _02_GuessTheNumberWebOne
{
    class MainClass
    {
        static List<Shovel.SourceFile> ProgramSources ()
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
            return Shovel.Api.MakeSources ("guess.sho", program);
        }

        static Shovel.Instruction[] ProgramBytecode ()
        {
            try {
                return Shovel.Api.GetBytecode (ProgramSources ());
            } catch (Shovel.Exceptions.ShovelException shex) {
                Console.WriteLine (shex.Message);
                return null;
            }
        }

        enum ReadStates
        {
            None,
            ReadInteger,
            ReadChar
        }
        ;

        static ReadStates readState = ReadStates.None;
        static string userInput = null;
        static StringBuilder pageContent = new StringBuilder ();
        static byte[] shovelVmState = null;

        static IEnumerable<Shovel.Callable> Udps ()
        {
            var rng = new Random ();
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) =>
            {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    pageContent.Append ("<span>");
                    pageContent.Append (HttpUtility.HtmlEncode (args [0].String));
                    pageContent.Append ("</span>");
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> printLn = (api, args, result) =>
            {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    pageContent.Append ("<span>");
                    pageContent.Append (HttpUtility.HtmlEncode (args [0].String));
                    pageContent.Append ("</span><br/>");
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> readInt = (api, args, result) =>
            {
                if (readState == ReadStates.None) {
                    result.After = Shovel.UdpResult.AfterCall.NapAndRetryOnWakeUp;
                    readState = ReadStates.ReadInteger;
                } else if (readState == ReadStates.ReadInteger) {
                    int dummy;
                    if (!int.TryParse (userInput, out dummy)) {
                        dummy = 0;
                    }
                    result.Result = Shovel.Value.MakeInt (dummy);
                    readState = ReadStates.None;
                    pageContent.Append (HttpUtility.HtmlEncode (userInput));
                    pageContent.Append ("<br/>");
                } else {
                    throw new InvalidOperationException ();
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> readChar = (api, args, result) =>
            {
                if (readState == ReadStates.None) {
                    result.After = Shovel.UdpResult.AfterCall.NapAndRetryOnWakeUp;
                    readState = ReadStates.ReadChar;
                } else if (readState == ReadStates.ReadChar) {
                    var line = userInput;
                    if (line.Length > 0) {
                        result.Result = Shovel.Value.Make (line.Substring (0, 1));
                    } else {
                        result.Result = Shovel.Value.Make ("");
                    }
                    readState = ReadStates.None;
                    pageContent.Append (HttpUtility.HtmlEncode (userInput));
                    pageContent.Append ("<br/>");
                } else {
                    throw new InvalidOperationException ();
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> random = (api, args, result) =>
            {
                result.Result = Shovel.Value.MakeFloat (rng.NextDouble ());
            };
            return new Shovel.Callable[] {
                Shovel.Callable.MakeUdp ("print", print, 1),
                Shovel.Callable.MakeUdp ("printLn", printLn, 1),
                Shovel.Callable.MakeUdp ("readInt", readInt, 0),
                Shovel.Callable.MakeUdp ("readChar", readChar, 0),
                Shovel.Callable.MakeUdp ("random", random, 0),
            };
        }

        private static void ServeGuessNumberRequest (HttpListenerContext ctx)
        {
            ctx.Response.ContentType = "text/html";
            userInput = ctx.Request.QueryString ["input"];
            var bytecode = Shovel.Api.GetBytecode (ProgramSources ());
            var vm = Shovel.Api.RunVm (bytecode, ProgramSources (), Udps (), shovelVmState);
            if (Shovel.Api.VmExecutionComplete (vm)) {
                shovelVmState = null;
                pageContent = new StringBuilder ();
                userInput = null;
                readState = ReadStates.None;
                vm = Shovel.Api.RunVm (bytecode, ProgramSources (), Udps (), shovelVmState);
            }
            shovelVmState = Shovel.Api.SerializeVmState (vm);
            using (var sw = new StreamWriter(ctx.Response.OutputStream)) {
                sw.Write ("<!DOCTYPE html>\n");
                sw.Write (pageContent.ToString ());
                sw.Write ("<form action='/' method='get'>");
                sw.Write ("<input type='text' name='input' id='shovel-input'/>");
                sw.Write ("<input type='submit' value='Submit'/>");
                sw.Write ("</form>");
                sw.Write ("<script>\n");
                sw.Write ("document.getElementById('shovel-input').focus()\n");
                sw.Write ("</script>\n");
            }
            ctx.Response.OutputStream.Close ();
        }

        public static void Main (string[] args)
        {
            if (!HttpListener.IsSupported) {
                Console.WriteLine ("HttpListener not available.");
                Environment.Exit (-1);
            }
            var bytecode = ProgramBytecode ();
            if (bytecode == null) {
                Environment.Exit (-1);
            }
            HttpListener hl = new HttpListener ();
            hl.Prefixes.Add ("http://localhost:8080/");
            hl.Start ();
            var requestNo = 1;
            while (hl.IsListening) {
                var ctx = hl.GetContext ();

                Console.WriteLine ("Serving a request ({0} {1}).", requestNo, ctx.Request.Url.AbsolutePath);

                if (ctx.Request.Url.AbsolutePath == "/") {
                    ServeGuessNumberRequest (ctx);
                } else {
                    ctx.Response.OutputStream.Close ();
                }

                Console.WriteLine ("Served a request ({0}).", requestNo);
                requestNo++;
            }
        }

    }
}
