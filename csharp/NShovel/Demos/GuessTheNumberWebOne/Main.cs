using System;
using System.Net;
using System.IO;
using System.Text;
using System.Web;
using System.Collections.Generic;
using System.Threading;

namespace GuessTheNumberWebOne
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
                    pageContent.Append (HttpUtility.HtmlEncode (args [0].StringValue));
                    pageContent.Append ("</span>");
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> printLn = (api, args, result) =>
            {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    pageContent.Append ("<span>");
                    pageContent.Append (HttpUtility.HtmlEncode (args [0].StringValue));
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
            using (MemoryStream ms = new MemoryStream()) {
                using (var sw = new StreamWriter(ms)) {
                    sw.Write ("<!DOCTYPE html>\n");
                    //sw.Write("<h1>hello</h1>");
                    sw.Write (pageContent.ToString ());
                    sw.Write ("<form action='/' method='get'>");
                    sw.Write ("<input type='text' name='input' id='shovel-input'/>");
                    sw.Write ("<input type='submit' value='Submit'/>");
                    sw.Write ("</form>");
                    sw.Write ("<script>\n");
                    sw.Write ("document.getElementById('shovel-input').focus()\n");
                    sw.Write ("</script>\n");
                }
                var bytes = ms.ToArray ();
                ctx.Response.ContentLength64 = bytes.Length;
                ctx.Response.OutputStream.Write (bytes, 0, bytes.Length);
                ctx.Response.OutputStream.Close ();
            }
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
