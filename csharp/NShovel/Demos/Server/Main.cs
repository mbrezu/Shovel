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
using System.Text;
using System.Collections.Generic;

namespace Server
{
    class MainClass
    {
        public static IEnumerable<Shovel.Callable> Udps ()
        {
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> addToAccount = (api, args, result) =>
            {
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> subtractFromAccount = (api, args, result) =>
            {
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> getTransactionDeadline = (api, args, result) =>
            {
                result.Result = Shovel.Value.Make ("Judgment day");
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> goToClient = (api, args, result) =>
            {
                result.After = Shovel.UdpResult.AfterCall.Nap;
            };
            return new Shovel.Callable[] {
                Shovel.Callable.MakeUdp ("addToAccount", addToAccount, 2),
                Shovel.Callable.MakeUdp ("subtractFromAccount", subtractFromAccount, 2),
                Shovel.Callable.MakeUdp ("getTransactionDeadline", getTransactionDeadline, 0),
                Shovel.Callable.MakeUdp ("goToClient", goToClient, 0),
            };
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
            while (hl.IsListening) {
                var ctx = hl.GetContext ();

                Console.WriteLine ("Serving a request ({0}).", ctx.Request.Url.AbsolutePath);

                if (ctx.Request.Url.AbsolutePath == "/") {
                    byte[] bytes = new byte[4];

                    byte[] state = null;
                    string program = null;
                    byte[] bytecode = null;

                    TimeIt ("read request", () => {
                        ctx.Request.InputStream.Read (bytes, 0, bytes.Length);
                        int bytecodeLength = BitConverter.ToInt32 (bytes, 0);

                        ctx.Request.InputStream.Read (bytes, 0, bytes.Length);
                        int sourcesLength = BitConverter.ToInt32 (bytes, 0);

                        ctx.Request.InputStream.Read (bytes, 0, bytes.Length);
                        int stateLength = BitConverter.ToInt32 (bytes, 0);

                        bytecode = new byte[bytecodeLength];
                        ctx.Request.InputStream.Read (bytecode, 0, bytecode.Length);

                        byte[] sourceBytes = new byte[sourcesLength];
                        ctx.Request.InputStream.Read (sourceBytes, 0, sourceBytes.Length);
                        program = Encoding.UTF8.GetString (sourceBytes);

                        state = new byte[stateLength];
                        ctx.Request.InputStream.Read (state, 0, state.Length);
                    }
                    );

                    // To execute the ShovelScript code between
                    // @goToServer and @goToClient in a DB transaction,
                    // just wrap this call in a DB transaction.

                    Shovel.Vm.Vm vm = null;
                    TimeIt ("server", () => {
                        vm = Shovel.Api.RunVm (
                        Shovel.Api.DeserializeBytecode (bytecode),
                        Shovel.Api.MakeSources ("request.sho", program), 
                        Udps (), 
                        state);
                    }
                    );

                    // In a real application, we should check the values returned by 
                    // Shovel.Api.VmProgrammingError and Shovel.Api.VmUserDefinedPrimitiveError 
                    // and return something else based on the presence/absence of errors.

                    // In a normal context, you must set quotas for RAM and CPU when calling RunVm (so a single
                    // broken/malicious request doesn't bring down the whole server). This is not 
                    // done here to keep things simple.

                    // This is accomplished using the RunVm 'totalTicksQuota' and 'usedCellsQuota' parameters.

                    // Using 'ticksUntilNextNapQuota' should also be used to protect against infinite loops.

                    byte[] stateAfter = null;
                    TimeIt ("serialize state", () => {
                        stateAfter = Shovel.Api.SerializeVmState (vm);
                    }
                    );

                    TimeIt ("write response", () => {
                        bytes = BitConverter.GetBytes (stateAfter.Length);
                        ctx.Response.OutputStream.Write (bytes, 0, bytes.Length);
                        ctx.Response.OutputStream.Write (stateAfter, 0, stateAfter.Length);
                    }
                    );
                }
                ctx.Response.OutputStream.Close ();

                Console.WriteLine ("Finished serving request.");
            }
        }

        static void TimeIt (string title, Action action)
        {
            var sw = new System.Diagnostics.Stopwatch ();
            sw.Start ();
            action ();
            sw.Stop ();
            Console.WriteLine ("{0} - {1} ms", title, sw.ElapsedMilliseconds);
        }
    }
}
