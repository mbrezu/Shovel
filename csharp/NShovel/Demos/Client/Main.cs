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
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Client
{
    class MainClass
    {
        public static IEnumerable<Shovel.Callable> Udps ()
        {
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> getSourceAccountNo = (api, args, result) =>
            {
                result.Result = Shovel.Value.MakeInt (2);
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> getDestinationAccountNo = (api, args, result) =>
            {
                result.Result = Shovel.Value.MakeInt (4);
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> getAmount = (api, args, result) =>
            {
                result.Result = Shovel.Value.MakeInt (1);
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> goToServer = (api, args, result) =>
            {
                result.After = Shovel.UdpResult.AfterCall.Nap;
            };
            Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> setTransactionDeadline = (api, args, result) =>
            {
                Console.WriteLine (String.Format ("The bank promised to do it by {0}.", args [0].StringValue));
            };
            return new Shovel.Callable[] {
                Shovel.Callable.MakeUdp ("getSourceAccountNo", getSourceAccountNo, 0),
                Shovel.Callable.MakeUdp ("getDestinationAccountNo", getDestinationAccountNo, 0),
                Shovel.Callable.MakeUdp ("getAmount", getAmount, 0),
                Shovel.Callable.MakeUdp ("goToServer", goToServer, 0),
                Shovel.Callable.MakeUdp ("setTransactionDeadline", setTransactionDeadline, 1),
            };
        }

        public static void Client()
        {
            Console.WriteLine ("***");
            var program = @"
// Setting up the call on the client.
var sourceAccountNo = @getSourceAccountNo()
var destinationAccountNo = @getDestinationAccountNo()
var amount = @getAmount()

// Go to the server.
@goToServer()

// Run the code on the server (the code on the server will wrap
// the code between 'goToServer' and 'goToClient' in a transaction) .

@subtractFromAccount(sourceAccountNo, amount)
@addToAccount(destinationAccountNo, amount) 
var transactionDeadline = @getTransactionDeadline()

// Go back to the client.
@goToClient()

// Handle the results on the client.
@setTransactionDeadline(transactionDeadline)
";
            var sources = Shovel.Api.MakeSources ("request.sho", program);
            var bytecode = Shovel.Api.GetBytecode (sources);
            // Run on the client to fill in request data.
            Shovel.Vm.Vm vm = null;
            TimeIt ("client - initialization", () => {
                vm = Shovel.Api.RunVm (bytecode, sources, Udps ());
            }
            );
            var serializedBytecode = Shovel.Api.SerializeBytecode (bytecode);
            var serializedState = Shovel.Api.SerializeVmState (vm);
            byte[] requestBytes = null;
            using (var ms = new MemoryStream()) {
                byte[] bytes = new byte[4];

                bytes = BitConverter.GetBytes (serializedBytecode.Length);
                ms.Write (bytes, 0, bytes.Length);

                var sourceBytes = Encoding.UTF8.GetBytes (program);

                bytes = BitConverter.GetBytes (sourceBytes.Length);
                ms.Write (bytes, 0, bytes.Length);

                bytes = BitConverter.GetBytes (serializedState.Length);
                ms.Write (bytes, 0, bytes.Length);

                ms.Write (serializedBytecode, 0, serializedBytecode.Length);
                ms.Write (sourceBytes, 0, sourceBytes.Length);
                ms.Write (serializedState, 0, serializedState.Length);

                requestBytes = ms.ToArray ();
            }
            byte[] responseBytes = null;
            // Send the Shovel process to the server.
            TimeIt ("server", () => {
                using (var wc = new WebClient()) {
                    responseBytes = wc.UploadData ("http://localhost:8080/", requestBytes);
                }
            }
            );
            // Extract the new state of the Shovel process from the HTTP response.
            byte[] serializedServerState = null;
            using (var ms = new MemoryStream(responseBytes)) {
                byte[] bytes = new byte[4];

                ms.Read (bytes, 0, 4);
                int serverStateLength = BitConverter.ToInt32 (bytes, 0);

                serializedServerState = new byte[serverStateLength];
                ms.Read (serializedServerState, 0, serializedServerState.Length);
            }
            // Run again on the client to extract the data from the program.
            TimeIt ("client - postprocess", () => {
                Shovel.Api.RunVm (bytecode, sources, Udps (), serializedServerState);
            }
            );
        }

        public static void Main (string[] args)
        {
            Client ();
            Client ();
            Client ();
            Client ();
            Client ();
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
