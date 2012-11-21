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

namespace Shovel.Compiler
{
	internal static class RawBytecodeOptimizations
	{
		internal static List<Instruction> Optimize(List<Instruction> bytecode)
		{
			bytecode = OptimizeLsetPopLget(bytecode);
			return bytecode;
		}

		private static List<Instruction> OptimizeLsetPopLget(List<Instruction> bytecode)
        {
			var result = new List<Instruction>();
			var i = 0;
			while (i < bytecode.Count) {
				var instruction = bytecode[i];
				if (instruction.Opcode == Instruction.Opcodes.Lset && i <= bytecode.Count - 3) {
					var lsetArgs = (int[])instruction.Arguments;
					var next = bytecode[i+1];
					var next2 = bytecode[i+2];
					if (next.Opcode == Instruction.Opcodes.Pop && next2.Opcode == Instruction.Opcodes.Lget) {
						var lgetArgs = (int[])next2.Arguments;
						if (lsetArgs[0] == lgetArgs[0] && lsetArgs[1] == lgetArgs[1]) {
							result.Add (instruction);
							i += 3;
							continue;
						}
					}
				}
				result.Add (instruction);
				i++;
			}
            return result;
        }

	}
}

