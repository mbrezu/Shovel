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

namespace Shovel
{
	internal static class AssembledBytecodeOptimizations
	{
		internal static Instruction[] Optimize (Instruction[] bytecode)
		{
			bytecode = JumpPropagation (bytecode);
			return bytecode;
		}

		private static Instruction[] JumpPropagation (Instruction[] bytecode)
		{
			foreach (var instruction in bytecode) {
				if (instruction.Opcode == Instruction.Opcodes.Jump
					|| instruction.Opcode == Instruction.Opcodes.Tjump
					|| instruction.Opcode == Instruction.Opcodes.Fjump) {
					instruction.Arguments = FollowJump (instruction, bytecode);
				}
			}
			return bytecode;
		}

		static int FollowJump (Instruction instruction, Instruction[] bytecode)
		{
			var pc = (int)instruction.Arguments;
			if (pc >= bytecode.Length) {
				return pc;
			}
			var newInstruction = bytecode [pc];
			if (newInstruction.Opcode == Instruction.Opcodes.Jump) {
				return FollowJump (newInstruction, bytecode);
			} else if (newInstruction.Opcode == Instruction.Opcodes.Const && pc + 1 < bytecode.Length) {
				if (newInstruction.Arguments is bool) {
					var nextInstruction = bytecode[pc+1];
					if ((bool)newInstruction.Arguments && nextInstruction.Opcode == Instruction.Opcodes.Tjump) {
						return FollowJump(nextInstruction, bytecode);
					} else if (!(bool)newInstruction.Arguments && nextInstruction.Opcode == Instruction.Opcodes.Fjump) {
						return FollowJump(nextInstruction, bytecode);
					}
				}
			}
			return pc;
		}

	}
}

