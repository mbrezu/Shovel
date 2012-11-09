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

namespace Shovel
{
	public class Instruction
	{
		public enum Opcodes
		{ 
			VmVersion,
			VmSourcesMd5,
			VmBytecodeMd5,
			FileName,
			Prim0,
			Return,
			Pop,
			Prim,
			Const,
			Context,
			BlockReturn,
			Block,
			Label,
			PopBlock,
			Call,
			Callj,
			Lget,
			Fjump,
			Jump,
			Lset,
			Fn,
			NewFrame,
			Args,
			DropFrame,
			Tjump
		}

		public Opcodes Opcode { get; set; }

		public object Arguments { get; set; }

		public int? StartPos { get; set; }

		public int? EndPos { get; set; }

		public string Comments { get; set; }

		public object Cache { get; set; }

		public byte NumericOpcode { get; set; }

		public override string ToString ()
		{
			var sb = new StringBuilder();
			if (this.Comments != null) {
				sb.Append (this.Comments);
			}
			if (this.Opcode == Opcodes.Label) {
				sb.AppendLine (this.Arguments.ToString().ToUpper() + ":");
			} else {
				sb.Append ("    ");
				sb.Append (this.Opcode.ToString().ToUpper());
				if (this.Arguments != null) {
					sb.Append (" ");
					if (this.Arguments is System.Collections.IEnumerable && !(this.Arguments is String)) {
						var args = new List<string>();
						foreach (var arg in (System.Collections.IEnumerable)this.Arguments) {
							args.Add (arg.ToString());
						}
						sb.Append (String.Join (", ", args));
					} else {
						sb.Append (this.Arguments.ToString());
					}
				}
				sb.Append (System.Environment.NewLine);
			}
			return sb.ToString();
		}
	}
}

