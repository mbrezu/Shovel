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
using System.Text;

namespace Shovel
{
	public class Api
	{
		public static int Version = 1;

		public static string PrintCode (List<SourceFile> sources)
		{
			var bytecode = Utils.GetRawBytecode(sources);
			Utils.DecorateByteCode (bytecode, sources);
			var sb = new StringBuilder ();
			foreach (var instruction in bytecode) {
				sb.Append (instruction.ToString ());
			}
			return sb.ToString ();
		}

		public static Instruction[] GetBytecode(List<SourceFile> sources)
		{
			var rawBytecode = Utils.GetRawBytecode(sources);
			return Utils.Assemble(rawBytecode);
		}

		public static List<SourceFile> MakeSources (params string[] namesAndContents)
		{
			List<SourceFile> result = new List<SourceFile> ();
			for (var i = 0; i < namesAndContents.Length; i+=2) {
				result.Add (new SourceFile () {
					FileName = namesAndContents[i],
					Content = namesAndContents[i+1]
				}
				);
			}
			return result;
		}

		public static object NakedRunVm(List<SourceFile> sources)
		{
			var rawBytecode = Utils.GetRawBytecode(sources);
			var bytecode = Utils.Assemble(rawBytecode);
			var vm = Vm.Vm.RunVm(bytecode, sources);
			return vm.CheckStackTop();
		}
	}
}

