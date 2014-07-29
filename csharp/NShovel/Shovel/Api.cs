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
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using Shovel.Exceptions;
using Shovel.Vm.Types;
using Shovel.Vm;

namespace Shovel
{
    public class Api
    {
        static int version = 5;

        public static int Version {
            get {
                return version;
            }
        }

        public static byte[] SerializeVmState (Vm.Vm vm)
        {
            var ms = Serialization.Utils.SerializeWithMd5CheckSum (str => {
                vm.SerializeState (str);
            }
            );
            return ms.ToArray ();
        }

        public static Stream SerializeVmStateToStream (Vm.Vm vm)
        {
            return Serialization.Utils.SerializeWithMd5CheckSum (str => {
                vm.SerializeState (str);
            }
            );
        }

        public static ShovelException VmProgrammingError (Shovel.Vm.Vm vm)
        {
            return vm.ProgrammingError;
        }

        public static Exception VmUserDefinedPrimitiveError (Shovel.Vm.Vm vm)
        {
            return vm.UserDefinedPrimitiveError;
        }

        public static string PrintRawBytecode (List<SourceFile> sources, bool optimize = false)
        {
            var bytecode = Utils.GetRawBytecode (sources);
            if (optimize) {
                bytecode = Compiler.RawBytecodeOptimizations.Optimize (bytecode);
            }
            Utils.DecorateByteCode (bytecode, sources);
            var sb = new StringBuilder ();
            foreach (var instruction in bytecode) {
                sb.Append (instruction.ToString ());
            }
            return sb.ToString ();
        }

        public static string PrintAssembledBytecode (List<SourceFile> sources)
        {
            var bytecode = Shovel.Api.GetBytecode (sources);
            Utils.DecorateByteCode (bytecode, sources);
            return PrintAssembledBytecode (bytecode);
        }

        public static string PrintAssembledBytecode (Instruction[] bytecode)
        {
            var labels = Utils.GetNumericLabels (bytecode);
            var sb = new StringBuilder ();
            for (var i = 0; i < bytecode.Length; i++) {
                if (labels.Contains (i)) {
                    sb.AppendLine (String.Format ("{0}:", i));
                }
                sb.Append (bytecode [i].ToString ());
            }
            if (labels.Contains (bytecode.Length)) {
                sb.AppendLine (String.Format ("{0}:", bytecode.Length));
            }
            return sb.ToString ();
        }

        public static Instruction[] GetBytecode (List<SourceFile> sources)
        {
            var rawBytecode = Utils.GetRawBytecode (sources);
            rawBytecode = Compiler.RawBytecodeOptimizations.Optimize (rawBytecode);
            var assembled = Utils.Assemble (rawBytecode);            
            assembled = Compiler.AssembledBytecodeOptimizations.Optimize (assembled);
            Utils.SetBytecodeMd5 (assembled, Utils.Md5AsString (Utils.ComputeBytecodeMd5 (assembled)));
            return assembled;
        }

        public static MemoryStream SerializeBytecodeToStream (Instruction[] bytecode)
        {
            return Serialization.BytecodeSerializer.SerializeBytecode (bytecode);
        }

        public static byte[] SerializeBytecode (Instruction[] bytecode)
        {
            return Serialization.BytecodeSerializer.SerializeBytecode (bytecode).ToArray();
        }

        public static Instruction[] DeserializeBytecode (MemoryStream ms)
        {
            return Serialization.BytecodeSerializer.DeserializeBytecode (ms);
        }

        public static Instruction[] DeserializeBytecode (byte[] bytes)
        {
            using (MemoryStream ms = new MemoryStream(bytes)) {
                return Serialization.BytecodeSerializer.DeserializeBytecode (ms);
            }
        }

        public static List<SourceFile> MakeSources (params string[] namesAndContents)
        {
            return MakeSourcesFromIEnumerable (namesAndContents);
        }

        public static List<SourceFile> MakeSourcesFromIEnumerable (IEnumerable<string> namesAndContents)
        {
            List<SourceFile> result = new List<SourceFile> ();
            for (var i = 0; i < namesAndContents.Count(); i += 2) {
                result.Add (new SourceFile ()
                {
                    FileName = namesAndContents.ElementAt(i),
                    Content = namesAndContents.ElementAt(i + 1)
                }
                );
            }
            return result;
        }

        public static List<SourceFile> MakeSourcesWithStdlib (params string[] namesAndContents)
        {
            List<string> sources = new List<string> ();
            sources.Add ("stdlib.sho");
            sources.Add (Utils.ShovelStdlib ());
            sources.AddRange (namesAndContents);
            return MakeSourcesFromIEnumerable (sources);
        }

        public static Value TestRunVm (List<SourceFile> sources)
        {
            var rawBytecode = Utils.GetRawBytecode (sources);
            var bytecode = Utils.Assemble (rawBytecode);
            var vm = Vm.Vm.RunVm (bytecode, sources);
            return vm.CheckStackTop ();
        }

        public static Value TestRunVm (Shovel.Instruction[] bytecode, List<SourceFile> sources)
        {
            var vm = Vm.Vm.RunVm (bytecode, sources);
            return vm.CheckStackTop ();
        }

        public static Vm.Vm RunVm (
            Shovel.Instruction[] bytecode, 
            List<SourceFile> sources,
            IEnumerable<Callable> userPrimitives = null,
            byte[] state = null,
            int? totalTicksQuota = null,
            int? ticksUntilNextNapQuota = null,
            int? usedCellsQuota = null)
        {
            return Vm.Vm.RunVm (
                bytecode, sources, userPrimitives, state, 
                null, usedCellsQuota, totalTicksQuota, ticksUntilNextNapQuota);
        }

        public static Vm.Vm RunVm (
            Vm.Vm vm, 
            List<SourceFile> sources,
            IEnumerable<Callable> userPrimitives = null,
            int? totalTicksQuota = null,
            int? ticksUntilNextNapQuota = null,
            int? usedCellsQuota = null)
        {
            return Vm.Vm.RunVm (
                null, 
                sources: sources, 
                userPrimitives: userPrimitives,
                vm: vm,
                cellsQuota: usedCellsQuota,
                totalTicksQuota: totalTicksQuota,
                untilNextNapTicksQuota: ticksUntilNextNapQuota);
        }

        public static Value CheckStackTop (Vm.Vm vm)
        {
            return vm.CheckStackTop ();
        }

        public static IEnumerable<Value> GetUsedStack( Vm.Vm vm ) {
            return vm.GetUsedStack();
        }

        public static VmEnvironment GetCurrentEnvironment( Vm.Vm vm ) { 
            return vm.GetCurrentEnvironment(); 
        }

        public static void WakeUpVm (Vm.Vm vm)
        {
            vm.WakeUp ();
        }

        public static bool VmIsLive (Vm.Vm vm)
        {
            return vm.IsLive ();
        }

        public static bool VmExecutionComplete (Vm.Vm vm)
        {
            return vm.ExecutionComplete ();
        }

        public static int VmUsedCells (Vm.Vm vm)
        {
            return vm.UsedCells;
        }

        public static long VmExecutedTicks (Vm.Vm vm)
        {
            return vm.ExecutedTicks;
        }

        public static long VmExecutedTicksSinceLastNap (Vm.Vm vm)
        {
            return vm.ExecutedTicksSinceLastNap;
        }

        public static string SideBySide (string str1, string str2, int halfSize = 38)
        {
            return Utils.SideBySide (str1, str2, halfSize);
        }

        public static Value GetStructInstanceValue(StructInstance str, string key, Value defaultValue)
        {
            for (int i = 0; i < str.Struct.Fields.Length; i++) {
                if (str.Struct.Fields[i] == key) {
                    return str.values[i];
                }
            }
            return defaultValue;
        }

        public static bool SetStructInstanceValue(StructInstance str, string key, Value newValue)
        {
            for (int i = 0; i < str.Struct.Fields.Length; i++) {
                if (str.Struct.Fields[i] == key) {
                    str.values[i] = newValue;
                    return true;
                }
            }
            return false;
        }

        public static string[] ListStructInstanceKeys(StructInstance str) 
        {
            var count = str.Struct.Fields.Length;
            var result = new string[count];
            Array.Copy(str.Struct.Fields, result, count);
            return result;
        }

    }
}

