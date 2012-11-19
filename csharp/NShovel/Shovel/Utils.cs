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
using System.Security.Cryptography;
using System.IO;
using System.Linq;

namespace Shovel
{
    internal static class Utils
    {
        internal static string ComputeSourcesMd5(List<SourceFile> sources)
        {
            using (MemoryStream ms = new MemoryStream())
            {
                foreach (var sourceFile in sources)
                {
                    var bytes = Encoding.UTF8.GetBytes(sourceFile.Content);
                    ms.Write(bytes, 0, bytes.Length);
                }
                ms.Seek(0, SeekOrigin.Begin);
                byte[] hash;
                using (MD5 md5Hash = MD5.Create())
                {
                    hash = md5Hash.ComputeHash(ms);
                }
                var sb = new StringBuilder();
                foreach (var b in hash)
                {
                    sb.AppendFormat("{0:X2}", b);
                }
                return sb.ToString();
            }
        }

        internal static List<string> ExtractRelevantSource(
            string[] sourceLines, Position startPos, Position endPos, string linePrefix = "")
        {
            var fileName = startPos.FileName;
            var startLine = startPos.Line;
            var endLine = endPos.Line;
            var addEllipsis = endLine > startLine;
            var firstLine = sourceLines[startLine - 1];

            List<string> result = new List<string>();

            StringBuilder sb1 = new StringBuilder();
            sb1.AppendFormat("{0}file '{1}' line {2}: {3}", linePrefix, fileName, startLine, firstLine);
            if (addEllipsis)
            {
                sb1.Append(" [...content snipped...]");
            }
            result.Add(sb1.ToString());

            var underline = Utils.Underline(
                Math.Max(startPos.Column, Utils.FirstNonBlank(firstLine)),
                Math.Min(firstLine.Length, addEllipsis ? firstLine.Length : endPos.Column)
            );
            var underlinedLine = String.Format(
                "{0}file '{1}' line {2}: {3}", linePrefix, fileName, startLine, underline);
            result.Add(underlinedLine);

            return result;
        }

        internal static string Underline(int start, int end)
        {
            StringBuilder sb = new StringBuilder();
            for (var i = 0; i < start - 1; i++)
            {
                sb.Append(' ');
            }
            for (var i = 0; i <= end - start; i++)
            {
                sb.Append('^');
            }
            return sb.ToString();
        }

        internal static int FirstNonBlank(string line)
        {
            int result = 1;
            while (true)
            {
                if (result > line.Length)
                {
                    return 1;
                }
                if (line[result - 1] != ' ' && line[result - 1] != '\t')
                {
                    return result;
                }
                result++;
            }
        }

        internal static void Panic()
        {
            throw new InvalidOperationException("Shovel internal WTF.");
        }

        internal static void DecorateByteCode(IEnumerable<Instruction> bytecode, List<SourceFile> sources)
        {
            string[] currentSourceFileSplit = null;
            SourceFile currentSource = null;
            foreach (var instruction in bytecode)
            {
                if (instruction.Opcode == Instruction.Opcodes.FileName)
                {
                    currentSource = SourceFile.FindSource(sources, (string)instruction.Arguments);
                    currentSourceFileSplit = currentSource.Content.Split('\n');
                }
                else if (currentSourceFileSplit != null
                  && instruction.StartPos != null
                  && instruction.EndPos != null)
                {
                    var startPos = Position.CalculatePosition(
                        currentSource, instruction.StartPos.Value);
                    var endPos = Position.CalculatePosition(
                        currentSource, instruction.EndPos.Value);
                    var lines = Utils.ExtractRelevantSource(
                        currentSourceFileSplit, startPos, endPos);
                    var sb = new StringBuilder();
                    foreach (var line in lines)
                    {
                        sb.AppendLine(";; " + line);
                    }
                    instruction.Comments = sb.ToString();
                }
            }
        }

        internal static List<Instruction> GetRawBytecode(List<SourceFile> sources)
        {
            // Tokenize files.
            var tokenizedFiles = new List<List<Token>>();
            foreach (var sourceFile in sources)
            {
                var tokenizer = new Shovel.Compiler.Tokenizer(sourceFile);
                tokenizedFiles.Add(tokenizer.Tokens);
            }
            // Parse files.
            var parseForests = new List<List<ParseTree>>();
            foreach (var tokenizedFile in tokenizedFiles)
            {
                var parser = new Shovel.Compiler.Parser(tokenizedFile, sources);
                parseForests.Add(parser.ParseTrees);
            }
            // Flatten the forests of parse trees into one for code generation.
            var bigParseForest = new List<ParseTree>();
            foreach (var parseTrees in parseForests)
            {
                bigParseForest.AddRange(parseTrees);
            }
            // Generate the code.
            var codeGenerator = new Shovel.Compiler.CodeGenerator(bigParseForest, sources);
            return codeGenerator.Bytecode;
        }

        public static Instruction[] Assemble(List<Instruction> bytecode)
        {
            int length = 0;
            var labels = new Dictionary<string, int>();
            foreach (var instruction in bytecode)
            {
                if (instruction.Opcode == Instruction.Opcodes.Label)
                {
                    var label = (string)instruction.Arguments;
                    if (labels.ContainsKey(label))
                    {
                        Utils.Panic();
                    }
                    labels[label] = length;
                }
                else
                {
                    length++;
                }
            }
            var result = new Instruction[length];
            var index = 0;
            foreach (var instruction in bytecode)
            {
                if (instruction.Opcode != Instruction.Opcodes.Label)
                {
                    result[index] = instruction;
                    index++;
                    if (instruction.Opcode == Instruction.Opcodes.Fjump
                        || instruction.Opcode == Instruction.Opcodes.Tjump
                        || instruction.Opcode == Instruction.Opcodes.Jump
                        || instruction.Opcode == Instruction.Opcodes.Block)
                    {
                        instruction.Arguments = labels[(string)instruction.Arguments];
                    }
                    else if (instruction.Opcode == Instruction.Opcodes.Fn)
                    {
                        var args = (object[])instruction.Arguments;
                        var label = (string)args[0];
                        instruction.Arguments = new int[] {
                            labels [label],
                            (int)args [1]
                        };
                    }
                    instruction.NumericOpcode = Utils.GetNumericOpcode(instruction.Opcode);
                }
            }
            return result;
        }

        static byte GetNumericOpcode(Instruction.Opcodes opcode)
        {
            switch (opcode)
            {
                case Instruction.Opcodes.Jump:
                    return 0;
                case Instruction.Opcodes.Const:
                    return 1;
                case Instruction.Opcodes.Prim0:
                    return 2;
                case Instruction.Opcodes.Prim:
                    return 3;
                case Instruction.Opcodes.Call:
                    return 4;
                case Instruction.Opcodes.CallJ:
                    return 5;
                case Instruction.Opcodes.Fjump:
                    return 6;
                case Instruction.Opcodes.Lset:
                    return 7;
                case Instruction.Opcodes.Pop:
                    return 8;
                case Instruction.Opcodes.Lget:
                    return 9;
                case Instruction.Opcodes.Fn:
                    return 10;
                case Instruction.Opcodes.NewFrame:
                    return 11;
                case Instruction.Opcodes.DropFrame:
                    return 12;
                case Instruction.Opcodes.Args:
                    return 13;
                case Instruction.Opcodes.Return:
                    return 14;
                case Instruction.Opcodes.Block:
                    return 15;
                case Instruction.Opcodes.PopBlock:
                    return 16;
                case Instruction.Opcodes.BlockReturn:
                    return 17;
                case Instruction.Opcodes.Context:
                    return 18;
                case Instruction.Opcodes.Tjump:
                    return 19;
                case Instruction.Opcodes.FileName:
                    return 20;
                case Instruction.Opcodes.VmVersion:
                    return 20;
                case Instruction.Opcodes.VmSourcesMd5:
                    return 20;
                case Instruction.Opcodes.VmBytecodeMd5:
                    return 20;
                case Instruction.Opcodes.Div:
                    return 21;
                case Instruction.Opcodes.Mod:
                    return 22;
                case Instruction.Opcodes.Neq:
                    return 23;
                case Instruction.Opcodes.Lt:
                    return 24;
                case Instruction.Opcodes.Add:
                    return 25;
                case Instruction.Opcodes.Gref:
                    return 26;
                case Instruction.Opcodes.Eq:
                    return 27;
                case Instruction.Opcodes.Apush:
                    return 28;
                case Instruction.Opcodes.GrefDot:
                    return 29;
                case Instruction.Opcodes.Sub:
                    return 30;
                case Instruction.Opcodes.Neg:
                    return 31;
                case Instruction.Opcodes.Mul:
                    return 32;
                case Instruction.Opcodes.Shl:
                    return 33;
                case Instruction.Opcodes.Shr:
                    return 34;
                case Instruction.Opcodes.Pow:
                    return 35;
                case Instruction.Opcodes.Floor:
                    return 36;
                case Instruction.Opcodes.Lte:
                    return 37;
                case Instruction.Opcodes.Gt:
                    return 38;
                case Instruction.Opcodes.Gte:
                    return 39;
                case Instruction.Opcodes.Not:
                    return 40;
                case Instruction.Opcodes.And:
                    return 41;
                case Instruction.Opcodes.Ior:
                    return 42;
                case Instruction.Opcodes.Xor:
                    return 43;
                case Instruction.Opcodes.Keys:
                    return 44;
                case Instruction.Opcodes.HasKey:
                    return 45;
                case Instruction.Opcodes.Apop:
                    return 46;
                case Instruction.Opcodes.SetIndexed:
                    return 47;
                case Instruction.Opcodes.Len:
                    return 48;
                case Instruction.Opcodes.IsString:
                    return 49;
                case Instruction.Opcodes.IsHash:
                    return 50;
                case Instruction.Opcodes.IsBool:
                    return 51;
                case Instruction.Opcodes.IsArray:
                    return 52;
                case Instruction.Opcodes.IsNumber:
                    return 53;
                case Instruction.Opcodes.IsInteger:
                    return 54;
                case Instruction.Opcodes.IsCallable:
                    return 55;
                default:
                    Utils.Panic();
                    return 0;
            }
        }

        static internal byte Endianess()
        {
            return (byte)(BitConverter.IsLittleEndian ? 1 : 0);
        }

		static internal HashSet<int> GetNumericLabels(Instruction[] bytecode) {
			var result = new HashSet<int>();
			foreach (var instruction in bytecode) {
				switch (instruction.Opcode) {
				case Instruction.Opcodes.Jump:
					result.Add((int)instruction.Arguments);
					break;
				case Instruction.Opcodes.Fjump:
					result.Add((int)instruction.Arguments);
					break;				
				case Instruction.Opcodes.Fn:
					result.Add (((int[])instruction.Arguments)[0]);
					break;
				case Instruction.Opcodes.Block:
					result.Add((int)instruction.Arguments);
					break;
				case Instruction.Opcodes.Tjump:
					result.Add((int)instruction.Arguments);
					break;
				}
			}
			return result;
		}

        static internal string ShovelStdlib()
        {
            return @"
var stdlib = {
   var min = fn (a, b) if a < b a else b
   var max = fn (a, b) if a > b a else b
   var while = fn (condition, body) {
     if condition() {
       body()
       while(condition, body)
     }
   }
   var forIndex = fn (arr, fun) {
     var i = 0
     while (fn () i < length(arr), fn () {
       fun(i)
       i = i + 1
     })
   }
   var forEach = fn (arr, fun) {
     forIndex(arr, fn i fun(arr[i]))
   }
   var forEachWithIndex = fn (arr, fun) {
     forIndex(arr, fn i fun(arr[i], i))
   }
   var map = fn (arr, fun) {
     var result = arrayN(length(arr))
     forIndex(arr, fn i result[i] = fun(arr[i]))
     result
   }
   var mapWithIndex = fn (arr, fun) {
     var result = arrayN(length(arr))
     forIndex(arr, fn i result[i] = fun(arr[i], i))
     result
   }
   var filter = fn (arr, fun) {
     var result = arrayN(length(arr))
     var ptr = 0
     forIndex(arr, fn i if fun(arr[i]) {
       result[ptr] = arr[i]
       ptr = ptr + 1
     })
     slice(result, 0, ptr)
   }
   var reduceFromLeft = fn (arr, initialElement, fun) {
     var result = initialElement
     forEach(arr, fn item result = fun(result, item))
     result
   }
   var qsort = fn (arr, lessThan) {
     if length(arr) == 0 || length(arr) == 1
     arr
     else {
       var pivot = arr[0]
       var butFirst = slice(arr, 1, -1)
       var lesser = filter(butFirst, fn el lessThan(el, pivot))
       var greater = filter(butFirst, fn el !lessThan(el, pivot))
       qsort(lesser, lessThan) + array(pivot) + qsort(greater, lessThan)
     }
   }
   var reverse = fn (arr) {
     var result = arrayN(length(arr))
     forIndex(arr, fn i result[length(arr) - 1 - i] = arr[i])
     result
   }

   var getPrefixedBlockName = {
     var namedBlockCounter = 0
     fn (prefix) {
       namedBlockCounter = namedBlockCounter + 1
       prefix + '_' +string(namedBlockCounter)
     }
   }
   var getBlockName = fn () getPrefixedBlockName('block')

   var tryAndThrow = {
     var tryStack = array()
     var throw = fn (error) {
       var blockName = pop(tryStack)
       push(tryStack, array(error))
       return blockName null
     }
     var try = fn (tryCode, catchCode) {
       var newBlockName = getPrefixedBlockName('tryCatchBlock')
       push(tryStack, newBlockName)
       var exitValue = block newBlockName tryCode()
       var stackTop = pop(tryStack)
       if isArray(stackTop) catchCode(stackTop[0])
       else exitValue
     }
     array(try, throw)
   }

   var repeat = fn (count, fun) {
     var counter = 0
     while (fn () counter < count, fn () {
       fun()
       counter = counter + 1
     })
   }

   hash('min', min,
        'max', max,
        'while', while,
        'forIndex', forIndex,
        'forEach', forEach,
        'forEachWithIndex', forEachWithIndex,
        'map', map,
        'mapWithIndex', mapWithIndex,
        'filter', filter,
        'reduceFromLeft', reduceFromLeft,
        'sort', qsort,
        'reverse', reverse,
        'getPrefixedBlockName', getPrefixedBlockName,
        'getBlockName', getBlockName,
        'try', tryAndThrow[0],
        'throw' , tryAndThrow[1],
        'repeat', repeat
       )
}";
        }

    }
}

