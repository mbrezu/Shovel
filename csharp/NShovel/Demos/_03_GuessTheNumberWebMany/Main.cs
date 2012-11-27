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
using System.Net;
using System.Web;
using System.IO;

namespace _03_GuessTheNumberWebMany
{
    class MainClass
    {
        static string Program ()
        {
            return @"
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
        }

        static List<Shovel.SourceFile> ProgramSources (string program)
        {
            return Shovel.Api.MakeSources ("guess.sho", program);
        }

        static Shovel.Instruction[] ProgramBytecode ()
        {
            try {
                return Shovel.Api.GetBytecode (ProgramSources (Program ()));
            } catch (Shovel.Exceptions.ShovelException shex) {
                Console.WriteLine (shex.Message);
                return null;
            }
        }

        static IEnumerable<Shovel.Callable> Udps (Session session, string userInput)
        {
            var rng = new Random ();
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) =>
            {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    session.PageContent.Append ("<span>");
                    session.PageContent.Append (HttpUtility.HtmlEncode (args [0].StringValue));
                    session.PageContent.Append ("</span>");
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> printLn = (api, args, result) =>
            {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    session.PageContent.Append ("<span>");
                    session.PageContent.Append (HttpUtility.HtmlEncode (args [0].StringValue));
                    session.PageContent.Append ("</span><br/>");
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> readInt = (api, args, result) =>
            {
                if (session.ReadState == Session.ReadStates.None) {
                    result.After = Shovel.UdpResult.AfterCall.NapAndRetryOnWakeUp;
                    session.ReadState = Session.ReadStates.ReadInteger;
                } else if (session.ReadState == Session.ReadStates.ReadInteger) {
                    int dummy;
                    if (!int.TryParse (userInput, out dummy)) {
                        dummy = 0;
                    }
                    result.Result = Shovel.Value.MakeInt (dummy);
                    session.ReadState = Session.ReadStates.None;
                    session.PageContent.Append (HttpUtility.HtmlEncode (userInput));
                    session.PageContent.Append ("<br/>");
                } else {
                    throw new InvalidOperationException ();
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> readChar = (api, args, result) =>
            {
                if (session.ReadState == Session.ReadStates.None) {
                    result.After = Shovel.UdpResult.AfterCall.NapAndRetryOnWakeUp;
                    session.ReadState = Session.ReadStates.ReadChar;
                } else if (session.ReadState == Session.ReadStates.ReadChar) {
                    var line = userInput;
                    if (line.Length > 0) {
                        result.Result = Shovel.Value.Make (line.Substring (0, 1));
                    } else {
                        result.Result = Shovel.Value.Make ("");
                    }
                    session.ReadState = Session.ReadStates.None;
                    session.PageContent.Append (HttpUtility.HtmlEncode (userInput));
                    session.PageContent.Append ("<br/>");
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

        static Session FreshSession (FileSystemDatabase fsd)
        {
            var session = new Session ();
            session.Id = fsd.GetFreshId ();
            session.ShovelVmSources = Program ();
            session.ShovelVmBytecode = Shovel.Api.SerializeBytecode (ProgramBytecode ());
            return session;
        }

        private static void ServeGuessNumberRequest (HttpListenerContext ctx, FileSystemDatabase fsd)
        {
            ctx.Response.ContentType = "text/html";
            var userInput = ctx.Request.QueryString ["input"];
            int sessionId = 0;
            var sessionIdStr = ctx.Request.QueryString ["sessionid"];
            int.TryParse (sessionIdStr, out sessionId);
            Session session = null;
            if (sessionId != 0) {
                session = Session.Load (fsd, sessionId);
            }
            if (session == null) {
                session = FreshSession (fsd);
            }
            var vm = Shovel.Api.RunVm (
                Shovel.Api.DeserializeBytecode (session.ShovelVmBytecode), 
                ProgramSources (Program ()), 
                Udps (session, userInput), 
                session.ShovelVmState);
            if (Shovel.Api.VmExecutionComplete (vm)) {
                ctx.Response.Redirect ("/");
            } else {
                session.ShovelVmState = Shovel.Api.SerializeVmState (vm);
                // FIXME: Uncomment the next statement to fix the 'back button bug'.
                //session.Id = fsd.GetFreshId ();
                session.Save (fsd);
                using (var sw = new StreamWriter(ctx.Response.OutputStream)) {
                    sw.Write ("<!DOCTYPE html>\n");
                    sw.Write (session.PageContent.ToString ());
                    sw.Write ("<form action='/' method='get'>");
                    sw.Write ("<input type='text' name='input' id='shovel-input'/>");
                    sw.Write ("<input type='submit' value='Submit'/>");
                    sw.Write (String.Format (
                        "<input type='hidden' name='sessionid' value='{0}' id='shovel-input'/>", session.Id));
                    sw.Write ("</form>");
                    sw.Write ("<script>\n");
                    sw.Write ("document.getElementById('shovel-input').focus()\n");
                    sw.Write ("</script>\n");
                }
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
            var fsd = new FileSystemDatabase ("db");
            while (hl.IsListening) {
                var ctx = hl.GetContext ();

                Console.WriteLine ("Serving a request ({0} {1}).", requestNo, ctx.Request.Url.AbsolutePath);

                if (ctx.Request.Url.AbsolutePath == "/") {
                    ServeGuessNumberRequest (ctx, fsd);
                } else {
                    ctx.Response.OutputStream.Close ();
                }

                Console.WriteLine ("Served a request ({0}).", requestNo);
                requestNo++;
            }
        }

    }
}
