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

namespace Shovel
{
    public class Callable
    {
        public static readonly int CollectParamsArityModifier = 1 << 20;

        internal string UdpName { get; set; }

        internal string Prim0Name { get; set; }

        internal bool HasCollectParams { get; set; }

        // FIXME: Arity and Program counter should be plain ints (saves memory).
        // For both of them the 'null' case can be encoded as -1.
        internal int? Arity { get; set; }

        // The next two fields are only used if this is a ShovelScript closure.
        internal int? ProgramCounter { get; set; }

        public Vm.VmEnvironment Environment { get; internal set; }

        internal Func<VmApi, Value[], int, int, Value> RequiredPrimitive { get; set; }

        internal Action<VmApi, Value[], UdpResult> UserDefinedPrimitive { get; set; }

        public static Callable MakeUdp (
            string name,
            Action<VmApi, Value[], UdpResult> udp,
            int? arity = null)
        {
            return new Callable () 
            {
                UdpName = name,
                Arity = arity,
                UserDefinedPrimitive = udp
            };
        }

        internal static Callable MakePrim0 (
            string name,
            Func<VmApi, Value[], int, int, Value> hostCallable,
            int? arity = 2)
        {
            return new Callable () 
            {
                Prim0Name = name,
                Arity = arity,
                RequiredPrimitive = hostCallable
            };
        }

        internal static Func<VmApi, Value[], int, int, Value> MakeHostCallable (
            Func<VmApi, Value> callable)
        {
            return (vmapi, args, start, length) => callable (vmapi);
        }

        internal static Func<VmApi, Value[], int, int, Value> MakeHostCallable (
            Func<VmApi, Value, Value> callable)
        {
            return (vmapi, args, start, length) => callable (vmapi, args [start]);
        }

        internal static Func<VmApi, Value[], int, int, Value> MakeHostCallable (
            Func<VmApi, Value, Value, Value> callable)
        {
            return (vmapi, args, start, length) => callable (vmapi, args [start], args [start + 1]);
        }

        internal static Func<VmApi, Value[], int, int, Value> MakeHostCallable (
            Func<VmApi, Value, Value, Value, Value> callable)
        {
            return (vmapi, args, start, length) => callable (vmapi, args [start], args [start + 1], args [start + 2]);
        }
    }
}