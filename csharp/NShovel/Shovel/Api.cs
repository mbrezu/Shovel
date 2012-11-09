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
			var tokenizer = new Shovel.Compiler.Tokenizer (sources [0]);
			var parser = new Shovel.Compiler.Parser (tokenizer.Tokens, sources);
			var codeGenerator = new Shovel.Compiler.CodeGenerator (parser.ParseTrees, sources);
			var bytecode = codeGenerator.Bytecode;
			DecorateByteCode (bytecode, sources);
			var sb = new StringBuilder ();
			foreach (var instruction in bytecode) {
				sb.Append (instruction.ToString ());
			}
			return sb.ToString ();
		}

		static void DecorateByteCode (List<Instruction> bytecode, List<SourceFile> sources)
		{
			string[] currentSourceFileSplit = null;
			SourceFile currentSource = null;
			foreach (var instruction in bytecode) {
				if (instruction.Opcode == Instruction.Opcodes.FileName) {
					currentSource = SourceFile.FindSource (sources, (string)instruction.Arguments);
					currentSourceFileSplit = currentSource.Content.Split ('\n');
				} else if (currentSourceFileSplit != null 
					&& instruction.StartPos != null 
					&& instruction.EndPos != null) {
					var startPos = Position.CalculatePosition (
						currentSource, instruction.StartPos.Value);
					var endPos = Position.CalculatePosition (
						currentSource, instruction.EndPos.Value);
					var lines = Utils.ExtractRelevantSource (
						currentSourceFileSplit, startPos, endPos);
					var sb = new StringBuilder ();
					foreach (var line in lines) {
						sb.AppendLine (";; " + line);
					}
					instruction.Comments = sb.ToString ();
				}
			}
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
	}
}

