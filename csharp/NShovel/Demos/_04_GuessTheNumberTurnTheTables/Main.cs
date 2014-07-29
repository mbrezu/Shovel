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
using System.Net;
using System.Web;
using System.IO;

namespace _04_GuessTheNumberTurnTheTables
{
    class MainClass
    {
        static List<Shovel.SourceFile> ProgramSources (string program)
        {
            return Shovel.Api.MakeSources ("guess.sho", program);
        }

        static Shovel.Instruction[] ProgramBytecode (string sources)
        {
            try {
                return Shovel.Api.GetBytecode (ProgramSources (sources));
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
                    session.PageContent.Append (HttpUtility.HtmlEncode (args [0].String));
                    session.PageContent.Append ("</span>");
                } else {
                    session.PageContent.Append ("PRINT call failed!");
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> printLn = (api, args, result) =>
            {
                if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                    session.PageContent.Append ("<span>");
                    session.PageContent.Append (HttpUtility.HtmlEncode (args [0].String));
                    session.PageContent.Append ("</span><br/>");
                } else {
                    session.PageContent.Append ("PRINT call failed!");
                }
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> readLine = (api, args, result) =>
            {
                if (session.ReadState == Session.ReadStates.None) {
                    result.After = Shovel.UdpResult.AfterCall.NapAndRetryOnWakeUp;
                    session.ReadState = Session.ReadStates.ReadLine;
                } else if (session.ReadState == Session.ReadStates.ReadLine) {
                    result.Result = Shovel.Value.Make (userInput);
                    session.ReadState = Session.ReadStates.None;
                    session.PageContent.Append (HttpUtility.HtmlEncode (userInput));
                    session.PageContent.Append ("<br/>");
                } else {
                    throw new InvalidOperationException ();
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
                Shovel.Callable.MakeUdp ("readLine", readLine, 0),
                Shovel.Callable.MakeUdp ("readInt", readInt, 0),
                Shovel.Callable.MakeUdp ("readChar", readChar, 0),
                Shovel.Callable.MakeUdp ("random", random, 0),
            };
        }

        static Session FreshSession (FileSystemDatabase fsd, Shovel.Instruction[] bytecode, string program)
        {
            var session = new Session ();
            session.Id = fsd.GetFreshId ();
            session.ShovelVmSources = program;
            session.ShovelVmBytecode = Shovel.Api.SerializeBytecode (bytecode);
            return session;
        }

        private static void ServeTheTextArea (HttpListenerContext ctx, 
                                              string programSource = null,
                                              string error = null)
        {
            using (var sw = new StreamWriter(ctx.Response.OutputStream)) {
                sw.Write ("<!DOCTYPE html>\n");
                sw.Write ("<form action='/' method='post'>");
                sw.Write ("<p>Write your ShovelScript program below:</p>");
                sw.Write ("<textarea name='program' id='shovel-input' rows='30' cols='80'>");
                if (programSource != null) {
                    sw.Write (HttpUtility.HtmlEncode (programSource));
                }
                sw.Write ("</textarea><br/>");
                sw.Write ("<input type='submit' value='Submit'/>");
                sw.Write ("</form>");
                if (error != null) {
                    sw.Write ("<pre>");
                    sw.Write (HttpUtility.HtmlEncode (error));
                    sw.Write ("</pre>");
                }
                sw.Write ("<script>\n");
                sw.Write ("document.getElementById('shovel-input').focus()\n");
                sw.Write ("</script>\n");
            }
            ctx.Response.OutputStream.Close ();
        }

        static Dictionary<string, string> ParsePostData (string postData)
        {
            var parts = postData.Split ('\n');
            var result = new Dictionary<string, string> ();
            foreach (var part in parts) {
                var pos = part.IndexOf ('=');
                var key = part.Substring (0, pos);
                var value = HttpUtility.UrlDecode (part.Substring (pos + 1));
                result [key.Trim ()] = value;
            }
            return result;
        }

        static void ServeError (HttpListenerContext ctx, Session session, string error)
        {
            using (var sw = new StreamWriter(ctx.Response.OutputStream)) {
                sw.Write ("<!DOCTYPE html>\n");
                sw.Write (session.PageContent.ToString ());
                sw.Write ("<p>Program execution failed.</p>");
                sw.Write ("<pre>");
                sw.Write (HttpUtility.HtmlEncode (error));
                sw.Write ("</pre>");
                sw.Write ("<a href='/' id='restart-link'>Enter another program.</p>");
                sw.Write ("<script>\n");
                sw.Write ("document.getElementById('restart-link').focus()\n");
                sw.Write ("</script>\n");
            }
        }

        static void ServeSession (HttpListenerContext ctx, FileSystemDatabase fsd, Session session, string userInput)
        {
            var vm = Shovel.Api.RunVm (
                Shovel.Api.DeserializeBytecode (session.ShovelVmBytecode), 
                ProgramSources (session.ShovelVmSources), 
                Udps (session, userInput), 
                session.ShovelVmState,
                totalTicksQuota: null,
                ticksUntilNextNapQuota: null,
                usedCellsQuota: null);
            if (Shovel.Api.VmProgrammingError (vm) != null) {
                ServeError (ctx, session, Shovel.Api.VmProgrammingError (vm).Message);
            } else if (Shovel.Api.VmUserDefinedPrimitiveError (vm) != null) {
                ServeError (ctx, session, Shovel.Api.VmUserDefinedPrimitiveError (vm).Message);
            } else if (Shovel.Api.VmExecutionComplete (vm)) {
                using (var sw = new StreamWriter(ctx.Response.OutputStream)) {
                    sw.Write ("<!DOCTYPE html>\n");
                    sw.Write (session.PageContent.ToString ());
                    sw.Write ("<p>Program execution completed.</p>");
                    sw.Write ("<a href='/' id='restart-link'>Enter another program.</p>");
                    sw.Write ("<script>\n");
                    sw.Write ("document.getElementById('restart-link').focus()\n");
                    sw.Write ("</script>\n");
                }
            } else {
                session.ShovelVmState = Shovel.Api.SerializeVmState (vm);
                session.Id = fsd.GetFreshId ();
                session.Save (fsd);
                using (var sw = new StreamWriter(ctx.Response.OutputStream)) {
                    sw.Write ("<!DOCTYPE html>\n");
                    sw.Write (session.PageContent.ToString ());
                    sw.Write ("<form action='/' method='get'>");
                    sw.Write ("<input type='text' name='input' id='shovel-input'/>");
                    sw.Write ("<input type='submit' value='Submit'/>");
                    sw.Write (String.Format (
                        "<input type='hidden' name='sessionid' value='{0}' id='shovel-input'/>", 
                        session.Id)
                    );
                    sw.Write ("</form>");
                    sw.Write ("<script>\n");
                    sw.Write ("document.getElementById('shovel-input').focus()\n");
                    sw.Write ("</script>\n");
                }
            }
            ctx.Response.OutputStream.Close ();
        }

        static void CompileProgram (HttpListenerContext ctx, FileSystemDatabase fsd)
        {
            string postData;
            using (StreamReader getPostParam = new StreamReader(ctx.Request.InputStream, true)) {
                postData = getPostParam.ReadToEnd ();
                var parsedPostData = ParsePostData (postData);
                if (parsedPostData.ContainsKey ("program")) {
                    var source = parsedPostData ["program"];
                    try {
                        var bytecode = Shovel.Api.GetBytecode (ProgramSources (source));
                        var session = FreshSession (fsd, bytecode, source);
                        ServeSession (ctx, fsd, session, null);
                    } catch (Shovel.Exceptions.ShovelException shex) {
                        ServeTheTextArea (ctx, source, shex.Message);
                    }
                } else {
                    ServeTheTextArea (ctx);
                }
            }
        }

        static void ServeRequest (HttpListenerContext ctx, FileSystemDatabase fsd)
        {
            ctx.Response.ContentType = "text/html";
            if (ctx.Request.HttpMethod.ToUpper () == "POST") {
                CompileProgram (ctx, fsd);
            } else {
                var userInput = ctx.Request.QueryString ["input"];
                var sessionIdStr = ctx.Request.QueryString ["sessionid"];
                if (sessionIdStr == null) {
                    ServeTheTextArea (ctx);
                } else {
                    int sessionId = 0;
                    int.TryParse (sessionIdStr, out sessionId);
                    Session session = null;
                    if (sessionId != 0) {
                        session = Session.Load (fsd, sessionId);
                    }
                    if (session == null) {
                        ServeTheTextArea (ctx);
                    } else {
                        ServeSession (ctx, fsd, session, userInput);
                    }
                }
            }
        }

        public static void Main (string[] args)
        {
            if (!HttpListener.IsSupported) {
                Console.WriteLine ("HttpListener not available.");
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
                    ServeRequest (ctx, fsd);
                } else {
                    ctx.Response.OutputStream.Close ();
                }

                Console.WriteLine ("Served a request ({0}).", requestNo);
                requestNo++;
            }
        }

    }
}
