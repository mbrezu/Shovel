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
		public string UdpName { get; set; }

		public string Prim0Name { get; set; }

		public int? Arity { get; set; }

		public Func<VmApi, List<object>, object> CachedHostCallable { get; set; }

		public static Callable MakeUdp (
			string name,
			Func<VmApi, List<object>, object> hostCallable,
			int? arity = null)
		{
			return new Callable () 
			{
				UdpName = name,
				Arity = arity,
				CachedHostCallable = hostCallable
			};
		}

		internal static Callable MakePrim0 (
			string name,
			Func<VmApi, List<object>, object> hostCallable,
			int? arity = 2)
		{
			return new Callable () 
			{
				Prim0Name = name,
				Arity = arity,
				CachedHostCallable = hostCallable
			};
		}

		public static Func<VmApi, List<object>, object> MakeHostCallable (
			Func<VmApi, object> callable)
		{
			return (vmapi, args) => callable (vmapi);
		}

		public static Func<VmApi, List<object>, object> MakeHostCallable (
			Func<VmApi, object, object> callable)
		{
			return (vmapi, args) => callable (vmapi, args [0]);
		}

		public static Func<VmApi, List<object>, object> MakeHostCallable (
			Func<VmApi, object, object, object> callable)
		{
			return (vmapi, args) => callable (vmapi, args [0], args [1]);
		}

		public static Func<VmApi, List<object>, object> MakeHostCallable (
			Func<VmApi, object, object, object, object> callable)
		{
			return (vmapi, args) => callable (vmapi, args [0], args [1], args[2]);
		}

	}
}

