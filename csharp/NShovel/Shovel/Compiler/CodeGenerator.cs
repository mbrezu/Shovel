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
using System.Linq;
using Shovel.Exceptions;
using Shovel.Compiler.Types;

namespace Shovel.Compiler
{
	internal class CodeGenerator
	{
		int labelCounter = 0;
		string fileName;
		List<ParseTree> ast;
		List<SourceFile> sources;

		public CodeGenerator (List<ParseTree> ast, List<SourceFile> sources)
		{
			this.ast = ast;
			this.sources = sources;
		}

		List<Instruction> bytecode;

		public List<Instruction> Bytecode {
			get {
				if (this.bytecode == null) {
					this.GenerateCode ();
				}
				return this.bytecode;
			}
		}

		void GenerateCode ()
		{
			this.bytecode = new List<Instruction> ();
			this.Gen (Instruction.Opcodes.VmVersion, Api.Version);
			this.Gen (Instruction.Opcodes.VmSourcesMd5, Utils.Md5AsString(Utils.ComputeSourcesMd5 (sources)));
			this.Gen (Instruction.Opcodes.VmBytecodeMd5, "?");
			this.CompileBlock (this.ast, this.EmptyEnv (), true, true);
		}

		void CompileAst (ParseTree ast, Types.Environment env, bool useVal, bool more)
		{
			switch (ast.Label) {
			case ParseTree.Labels.FileName:
				this.fileName = ast.Content;
				this.Gen (Instruction.Opcodes.FileName, this.fileName);
				break;
			case ParseTree.Labels.Var:
				this.CompileVar (ast, env, useVal, more);
				break;
			case ParseTree.Labels.Fn:
				this.CompileFn (ast, env, useVal, more);
				break;
			case ParseTree.Labels.Begin:
				this.CompileBlock (ast.Children, env, useVal, more);
				break;
			case ParseTree.Labels.Assignment:
				this.CompileSet (ast, env, useVal, more);
				break;
			case ParseTree.Labels.If:
				this.CompileIf (ast, env, useVal, more);
				break;
			case ParseTree.Labels.Name:
				this.CompileName (ast, env, useVal, more);
				break;
			case ParseTree.Labels.Call:
				this.CompileFuncall (ast, env, useVal, more);
				break;
			case ParseTree.Labels.Prim0:
				this.CompilePrim0 (ast, useVal, more);
				break;
			case ParseTree.Labels.UserDefinedPrimitive:
				this.CompileUserDefinedPrimitive (ast, useVal, more);
				break;
			case ParseTree.Labels.Number:
				this.CompileAtom (ast, env, useVal, more);
				break;
			case ParseTree.Labels.String:
				this.CompileAtom (ast, env, useVal, more);
				break;
			case ParseTree.Labels.Bool:
				this.CompileAtom (ast, env, useVal, more);
				break;
			case ParseTree.Labels.Void:
				this.CompileAtom (ast, env, useVal, more);
				break;
			case ParseTree.Labels.NamedBlock:
				this.CompileNamedBlock (ast, env, more);
				break;
			case ParseTree.Labels.BlockReturn:
				this.CompileBlockReturn (ast, env);
				break;
			case ParseTree.Labels.Context:
				this.CompileContext (ast, useVal, more);
				break;
			default:
				Utils.Panic ();
				break;
			}
		}

		void CompileContext (ParseTree ast, bool useVal, bool more)
		{
			this.Gen (Instruction.Opcodes.Context, null, ast);
			FinishInstruction (useVal, more);
		}

		void CompileBlockReturn (ParseTree ast, Types.Environment env)
		{
			var blockName = ast.Children.ElementAt (0);
			var result = ast.Children.ElementAt (1);
			this.CompileAst (blockName, env, true, true);
			this.CompileAst (result, env, true, true);
			this.Gen (Instruction.Opcodes.BlockReturn, null, ast);
		}

		void CompileNamedBlock (ParseTree ast, Types.Environment env, bool more)
		{
			var blockName = ast.Children.ElementAt (0);
			var blockContents = ast.Children.ElementAt (1);
			var blockEnd = this.GenLabel ("BE");
			this.CompileAst (blockName, env, true, true);
			this.Gen (Instruction.Opcodes.Block, blockEnd, ast);
			this.CompileAst (blockContents, env, true, true);
			this.Gen (Instruction.Opcodes.Label, blockEnd);
			this.Gen (Instruction.Opcodes.PopBlock, null, ast);
			if (!more) {
				this.Gen (Instruction.Opcodes.Return);
			}
		}

		string GenLabel (string prefix = "L")
		{
			this.labelCounter++;
			return String.Format ("{0}{1}", prefix, this.labelCounter);
		}

		void CompileAtom (ParseTree ast, Types.Environment env, bool useVal, bool more)
		{
			if (useVal) {
				this.Gen (
                    Instruction.Opcodes.Const,
                    this.CompileAtomValue (ast.Label, ast.Content),
                    ast);
				if (!more) {
					this.Gen (Instruction.Opcodes.Return);
				}
			}
		}

		object CompileAtomValue (ParseTree.Labels label, string content)
		{
			switch (label) {
			case ParseTree.Labels.Number:
				{
					double doubleResult;
					long longResult;
					if (long.TryParse (content, out longResult)) {
						return Value.MakeInt (longResult);
					} else if (double.TryParse (content, out doubleResult)) {
						return Value.MakeFloat (doubleResult);
					}
				}
				Utils.Panic ();
				return null;
			case ParseTree.Labels.String:
				return Value.Make (content.Substring (1, content.Length - 2));
			case ParseTree.Labels.Bool:
				switch (content) {
				case "true":
					return Value.Make (true);
				case "false":
					return Value.Make (false);
				default:
					Utils.Panic ();
					return null;
				}
			case ParseTree.Labels.Void:
				return Value.Make ();
			default:
				Utils.Panic ();
				return null;
			}
		}

		void FinishInstruction (bool useVal, bool more)
		{
			if (!more) {
				this.Gen (Instruction.Opcodes.Return);
			}
			if (!useVal) {
				this.Gen (Instruction.Opcodes.Pop);
			}
		}

		void CompileUserDefinedPrimitive (ParseTree ast, bool useVal, bool more)
		{
			this.Gen (Instruction.Opcodes.Prim, ast.Content.Substring(1), ast);
			FinishInstruction (useVal, more);
		}

		static Tuple<string, Instruction.Opcodes>[] compilableAsInstructions =
        new Tuple<string, Instruction.Opcodes>[] {
            Tuple.Create ("/", Instruction.Opcodes.Div),
            Tuple.Create ("%", Instruction.Opcodes.Mod),
            Tuple.Create ("!=", Instruction.Opcodes.Neq),
            Tuple.Create ("<", Instruction.Opcodes.Lt),
            Tuple.Create ("+", Instruction.Opcodes.Add),
            Tuple.Create ("svm_gref", Instruction.Opcodes.Gref),
			Tuple.Create ("==", Instruction.Opcodes.Eq),
            Tuple.Create ("push", Instruction.Opcodes.Apush),
            Tuple.Create ("svm_gref_dot", Instruction.Opcodes.GrefDot),
            Tuple.Create ("-", Instruction.Opcodes.Sub),
            Tuple.Create ("unary_minus", Instruction.Opcodes.Neg),
            Tuple.Create ("*", Instruction.Opcodes.Mul),
            Tuple.Create ("<<", Instruction.Opcodes.Shl),
            Tuple.Create (">>", Instruction.Opcodes.Shr),
            Tuple.Create ("pow", Instruction.Opcodes.Pow),
            Tuple.Create ("floor", Instruction.Opcodes.Floor),
            Tuple.Create ("<=", Instruction.Opcodes.Lte),
            Tuple.Create (">", Instruction.Opcodes.Gt),
            Tuple.Create (">=", Instruction.Opcodes.Gte),
            Tuple.Create ("!", Instruction.Opcodes.Not),
            Tuple.Create ("&", Instruction.Opcodes.And),
            Tuple.Create ("|", Instruction.Opcodes.Ior),
            Tuple.Create ("^", Instruction.Opcodes.Xor),
            Tuple.Create ("keys", Instruction.Opcodes.Keys),
            Tuple.Create ("hasKey", Instruction.Opcodes.HasKey),
            Tuple.Create ("pop", Instruction.Opcodes.Apop),
            Tuple.Create ("svm_set_indexed", Instruction.Opcodes.SetIndexed),
            Tuple.Create ("length", Instruction.Opcodes.Len),
            Tuple.Create ("isString", Instruction.Opcodes.IsString),
            Tuple.Create ("isHash", Instruction.Opcodes.IsHash),
            Tuple.Create ("isBool", Instruction.Opcodes.IsBool),
            Tuple.Create ("isArray", Instruction.Opcodes.IsArray),
            Tuple.Create ("isNumber", Instruction.Opcodes.IsNumber),
            Tuple.Create ("isInteger", Instruction.Opcodes.IsInteger),
            Tuple.Create ("isCallable", Instruction.Opcodes.IsCallable),
        };

		bool CompiledAsInstruction (ParseTree funAst, bool useVal, bool more)
		{
			foreach (var pair in compilableAsInstructions) {
				if (pair.Item1 == funAst.Content) {
					this.Gen (pair.Item2, startPos: funAst.StartPos, endPos: funAst.EndPos);
					this.FinishInstruction (useVal, more);
					return true;
				}
			}
			return false;
		}

		void CompileFuncall (ParseTree ast, Types.Environment env, bool useVal, bool more)
		{
			foreach (var child in ast.Children.Skip(1)) {
				this.CompileAst (child, env, true, true);
			}
			var funAst = ast.Children.First ();
			var compiledAsInstruction = false;
			if (funAst.Label == ParseTree.Labels.Prim0) {
				compiledAsInstruction = this.CompiledAsInstruction (funAst, useVal, more);
			}
			if (!compiledAsInstruction) {
				this.CompileAst (funAst, env, true, true);
				if (more) {
					this.Gen (Instruction.Opcodes.Call, ast.Children.Count () - 1, ast);
					if (!useVal) {
						this.Gen (Instruction.Opcodes.Pop);
					}
				} else {
					this.Gen (Instruction.Opcodes.CallJ, ast.Children.Count () - 1, ast);
				}
			}
		}

		void CompilePrim0 (ParseTree ast, bool useVal, bool more)
		{
			this.Gen (Instruction.Opcodes.Prim0, ast.Content, ast);
			FinishInstruction (useVal, more);
		}

		void CompileName (ParseTree ast, Types.Environment env, bool useVal, bool more)
		{
			var varName = ast.Content;
			this.Gen (Instruction.Opcodes.Lget,
                      this.FindName (varName, env, ast.StartPos, ast.EndPos),
                      ast);
			this.FinishInstruction (useVal, more);
		}

		int[] FindName (string varName, Types.Environment env, int startPos, int endPos, int frameNumber = 0)
		{
			if (env == null) {
				var message = String.Format ("Undefined variable '{0}'.", varName);
				this.RaiseError (startPos, endPos, message);
				return null;
			} else {
				for (var j = 0; j < env.Frame.Vars.Count; j++) {
					if (varName == env.Frame.Vars [j].Name) {
						return new int[] { frameNumber, j };
					}
				}
				return FindName (varName, env.Next, startPos, endPos, frameNumber + 1);
			}
		}

		void RaiseError (int characterStartPos, int characterEndPos, string message)
		{
			string errorFileName = null;
			int? line = null;
			int? column = null;
			if (this.sources != null && this.fileName != null) {
				var sourceFile = SourceFile.FindSource (this.sources, this.fileName);
				var content = sourceFile.Content;
				errorFileName = fileName;
				var startPos = Position.CalculatePosition (sourceFile, characterStartPos);
				var endPos = Position.CalculatePosition (sourceFile, characterEndPos);
				var lines = Utils.ExtractRelevantSource (content.Split ('\n'), startPos, endPos);
				message = String.Format ("{0}\n{1}\n{2}", message, lines [0], lines [1]);
				line = startPos.Line;
				column = startPos.Column;
			}
			throw new ShovelException ()
            {
                Message = message,
                FileName = errorFileName,
                Line = line,
                Column = column,
                AtEof = line == null
            };
		}

		void CompileIf (ParseTree ast, Types.Environment env, bool useVal, bool more)
		{
			if (more) {
				var l1 = this.GenLabel ();
				var l2 = this.GenLabel ();
				var pred = ast.Children.ElementAt (0);
				this.CompileAst (pred, env, true, true);
				this.Gen (Instruction.Opcodes.Fjump, l1);
				var thenAction = ast.Children.ElementAt (1);
				this.CompileAst (thenAction, env, useVal, true);
				this.Gen (Instruction.Opcodes.Jump, l2);
				this.Gen (Instruction.Opcodes.Label, l1);
				var elseAction = ast.Children.ElementAt (2);
				this.CompileAst (elseAction, env, useVal, true);
				this.Gen (Instruction.Opcodes.Label, l2);
			} else {
				var l1 = this.GenLabel ();
				var pred = ast.Children.ElementAt (0);
				this.CompileAst (pred, env, true, true);
				this.Gen (Instruction.Opcodes.Fjump, l1);
				var thenAction = ast.Children.ElementAt (1);
				this.CompileAst (thenAction, env, useVal, false);
				this.Gen (Instruction.Opcodes.Label, l1);
				var elseAction = ast.Children.ElementAt (2);
				this.CompileAst (elseAction, env, useVal, false);
			}
		}

		void CompileSet (ParseTree ast, Types.Environment env, bool useVal, bool more)
		{
			var leftHandSide = ast.Children.First ();
			if (leftHandSide.Label == ParseTree.Labels.Name) {
				this.CompileAst (ast.Children.ElementAt (2), env, true, true);
				this.CompileSetVar (leftHandSide.Content, env, useVal, more, ast);
			} else {
				if (!this.IsGrefCall (leftHandSide)) {
					this.RaiseError (
                        ast.StartPos, ast.EndPos,
                        "Assignment only supported for names, arrays and hashes.");
				}
				var arrayOrHash = leftHandSide.Children.ElementAt (1);
				var index = leftHandSide.Children.ElementAt (2);
				var setOperator = leftHandSide.Children.ElementAt (0);
				var primitiveParseTree = new ParseTree ()
                {
                    Label = ParseTree.Labels.Prim0,
                    StartPos = setOperator.StartPos,
                    EndPos = setOperator.EndPos,
                    Content = "svm_set_indexed"
                };
				var rightHandSide = ast.Children.ElementAt (2);
				var callParseTree = new ParseTree ()
                {
                    Label = ParseTree.Labels.Call,
                    StartPos = ast.StartPos,
                    EndPos = ast.EndPos,
                    Children = new ParseTree[] {
                        primitiveParseTree,
                        arrayOrHash,
                        index,
                        rightHandSide
                    }
                };
				this.CompileAst (callParseTree, env, useVal, more);
			}
		}

		// Gref is short for 'generic reference'.
		bool IsGrefCall (ParseTree ast)
		{
			if (ast.Label == ParseTree.Labels.Call) {
				if (ast.Children.Count () > 0) {
					var fn = ast.Children.First ();
					if (fn.Label == ParseTree.Labels.Prim0) {
						if (fn.Content == "svm_gref" || fn.Content == "svm_gref_dot") {
							return true;
						}
					}
				}
			}
			return false;
		}

		void CompileSetVar (string name, Types.Environment env, bool useVal, bool more, ParseTree astForPos)
		{
			this.Gen (
                Instruction.Opcodes.Lset,
                this.FindName (name, env, astForPos.StartPos, astForPos.EndPos),
                astForPos);
			this.FinishInstruction (useVal, more);
		}

		void CompileFn (ParseTree ast, Types.Environment env, bool useVal, bool more)
		{
			if (useVal) {
				var fn = this.GenLabel ("FN");
				var l = this.GenLabel ();
				this.Gen (Instruction.Opcodes.Jump, l);
				this.Gen (Instruction.Opcodes.Label, fn, ast);
				var args = ast.Children.ElementAt (0);
				var body = ast.Children.ElementAt (1);
				this.CompileFnBody (args, body, env);
				this.Gen (Instruction.Opcodes.Label, l);
				this.Gen (Instruction.Opcodes.Fn, new object[] {
                    fn,
                    args.Children.Count ()
                }
				);
				if (!more) {
					this.Gen (Instruction.Opcodes.Return);
				}
			}
		}

		void CompileFnBody (ParseTree args, ParseTree body, Types.Environment env)
		{
			if (args.Children.Count () > 0) {
				var newEnv = this.EmptyEnv ();
				newEnv.Next = env;
				foreach (var arg in args.Children) {
					this.ExtendFrame (newEnv, arg.Content, arg);
				}
				var varNames = args.Children.Select (arg => arg.Content).ToArray ();
				this.Gen (
                    Instruction.Opcodes.NewFrame, varNames,
                    startPos: args.Children.First ().StartPos,
                    endPos: args.Children.Last ().EndPos);
				this.Gen (Instruction.Opcodes.Args, varNames.Length);
				this.CompileAst (body, newEnv, true, false);
			} else {
				this.CompileAst (body, env, true, false);
			}
		}

		void ExtendFrame (Types.Environment env, string name, ParseTree nameAst)
		{
			var topFrame = env.Frame;
			var currentStartPos = nameAst.StartPos;
			var previousDefinition = topFrame.EntryFor (name);
			if (previousDefinition != null) {
				var sourceFile = SourceFile.FindSource (this.sources, previousDefinition.FileName);
				var pos = Position.CalculatePosition (sourceFile, previousDefinition.StartPos);
				var message = String.Format (
                    "Variable '{0}' is already defined in this frame in file '{1}', at line {2}, column {3}.",
                    name, pos.FileName, pos.Line, pos.Column);
				this.RaiseError (currentStartPos, nameAst.EndPos, message);
			} else {
				var newVar = new EnvVar ()
                {
                    Name = name,
                    FileName = this.fileName,
                    Place = topFrame.Vars.Count,
                    StartPos = currentStartPos
                };
				topFrame.Vars.Add (newVar);
			}
		}

		void CompileVar (ParseTree ast, Types.Environment env, bool useVal, bool more)
		{
			var nameAst = ast.Children.ElementAt (0);
			var name = nameAst.Content;
			this.ExtendFrame (env, name, nameAst);
			this.CompileAst (ast.Children.ElementAt (1), env, true, true);
			this.CompileSetVar (name, env, useVal, more, ast);
		}

		void CompileBlockMeat (IEnumerable<ParseTree> ast, Types.Environment env, bool useVal, bool more)
		{
			if (ast.Count () > 0) {
				var newVars = ast.Where (child => child.Label == ParseTree.Labels.Var);
				var newVarCount = newVars.Count ();
				var dropValueCount = ast.Count () - 1;
				var dropValueAsts = ast.Take (dropValueCount);
				var valueAst = ast.Skip (dropValueCount).First ();
				var newEnv = this.EmptyEnv ();
				newEnv.Next = env;
				if (newVarCount > 0) {
					var newVarNames = newVars
                    .Select (child => child.Children.ElementAt (0).Content).ToArray ();
					this.Gen (
                    Instruction.Opcodes.NewFrame, newVarNames,
                    startPos: newVars.First ().StartPos,
                    endPos: newVars.Last ().EndPos);
					this.CompileStatements (newEnv, dropValueAsts, valueAst, more);
					if (more) {
						this.Gen (Instruction.Opcodes.DropFrame);
					}
					if (!useVal) {
						this.Gen (Instruction.Opcodes.Pop);
					}
				} else {
					this.CompileStatements (env, dropValueAsts, valueAst, more);
					if (!useVal) {
						this.Gen (Instruction.Opcodes.Pop);
					}
				}
			} else {
				this.Gen (Instruction.Opcodes.Const, null);
				if (!useVal) {
					this.Gen (Instruction.Opcodes.Pop);
				}
			}
		}

		int? PositionOf (IEnumerable<ParseTree> ast, Func<ParseTree, bool> pred, bool fromEnd = false)
		{
			int idx, dir, limit;
			if (fromEnd) {
				idx = ast.Count () - 1;
				dir = -1;
				limit = -1;
			} else {
				idx = 0;
				dir = 1;
				limit = ast.Count ();
			}
			while (idx != limit) {
				if (pred (ast.ElementAt (idx))) {
					return idx;
				}
				idx += dir;
			}
			return null;
		}

		void CompileBlock (IEnumerable<ParseTree> ast, Types.Environment env, bool useVal, bool more)
		{
			int? meatStart = PositionOf (ast, pt => pt.Label != ParseTree.Labels.FileName, false);
			int? meatEnd = PositionOf (ast, pt => pt.Label != ParseTree.Labels.FileName, true);

			IEnumerable<ParseTree> prefixFluff, meat, suffixFluff;
			if (meatStart == null && meatEnd == null) {
				prefixFluff = ast;
				meat = new List<ParseTree> ();
				suffixFluff = new List<ParseTree> ();
			} else {
				prefixFluff = ast.Take (meatStart.Value);
				meat = ast.Skip (meatStart.Value).Take (meatEnd.Value - meatStart.Value + 1);
				suffixFluff = ast.Skip (meatEnd.Value + 1);
			}
			foreach (var pt in prefixFluff) {
				CompileAst (pt, this.EmptyEnv (), true, true);
			}
			CompileBlockMeat (meat, env, useVal, more);
			foreach (var pt in suffixFluff) {
				CompileAst (pt, this.EmptyEnv (), true, true);
			}
		}

		void CompileStatements (
            Types.Environment env,
            IEnumerable<ParseTree> dropValueAsts, ParseTree valueAst,
            bool more)
		{
			foreach (var ast in dropValueAsts) {
				this.CompileAst (ast, env, false, true);
			}
			this.CompileAst (valueAst, env, true, more);
		}

		Types.Environment EmptyEnv ()
		{
			return new Types.Environment ();
		}

		void Gen (Instruction.Opcodes opcode,
                  object arguments = null,
                  ParseTree pos = null,
                  string comments = null,
                  int? startPos = null,
                  int? endPos = null)
		{
			var instruction = new Instruction ()
            {
                Opcode = opcode,
                Arguments = arguments,
                Comments = comments
            };
			if (startPos != null) {
				instruction.StartPos = startPos;
			} else if (pos != null && pos.StartPos != -1) {
				instruction.StartPos = pos.StartPos;
			}
			if (endPos != null) {
				instruction.EndPos = endPos;
			} else if (pos != null && pos.EndPos != -1) {
				instruction.EndPos = pos.EndPos;
			}
			this.bytecode.Add (instruction);
		}

	}
}

