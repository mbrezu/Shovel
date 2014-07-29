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
using System.Linq;
using System.Globalization;
using Shovel.Exceptions;
using Shovel.Vm.Types;
using Shovel.Compiler.Types;
using System.IO;

namespace Shovel.Vm
{
    public class Vm
    {

        #region Private Storage
        Instruction[] bytecode = null;
        object[] cache = null;
        int programCounter = 0;
        VmEnvironment currentEnvironment = null;
        Stack stack = new Stack ();
        Dictionary<string, Callable> userPrimitives = null;
        int usedCells = 0;
        long executedTicks = 0;
        long executedTicksSinceLastNap = 0;
        List<SourceFile> sources;
        bool shouldTakeANap = false;
        Exception userDefinedPrimitiveError = null;
        ShovelException programmingError = null;
        int? cellsQuota = null;
        long? totalTicksQuota = null;
        long? untilNextNapTicksQuota = null;
        VmApi api = null;
        #endregion

        #region Public API

        internal ShovelException ProgrammingError {
            get {
                return programmingError;
            }
        }

        internal Exception UserDefinedPrimitiveError {
            get {
                return userDefinedPrimitiveError;
            }
        }

        internal int UsedCells {
            get {
                this.usedCells = CountCells ();
                return usedCells;
            }
        }

        internal long ExecutedTicks {
            get {
                return executedTicks;
            }
        }

        internal long ExecutedTicksSinceLastNap {
            get {
                return executedTicksSinceLastNap;
            }
        }

        internal static Vm RunVm (
            Instruction[] bytecode, 
            List<SourceFile> sources = null,
            IEnumerable<Callable> userPrimitives = null,
            byte[] state = null,
            Vm vm = null,
            int? cellsQuota = null,
            long? totalTicksQuota = null,
            long? untilNextNapTicksQuota = null)
        {
            if (vm == null) {
                vm = new Vm ();
                vm.bytecode = bytecode;
                vm.cache = new object[bytecode.Length];
                vm.programCounter = 0;
                vm.currentEnvironment = null;
                vm.stack = new Stack ();
                vm.sources = sources;
                vm.userPrimitives = new Dictionary<string, Callable> ();
            }
            vm.cellsQuota = cellsQuota;
            vm.totalTicksQuota = totalTicksQuota;
            vm.untilNextNapTicksQuota = untilNextNapTicksQuota;
            if (state != null) {
//                Utils.TimeIt ("Deserialize VM state", () => {
                vm.DeserializeState (state);
//                }
//                );
            }
            if (userPrimitives != null) {
                foreach (var udp in userPrimitives) {
                    vm.userPrimitives [udp.UdpName] = udp;
                }
            }
            vm.executedTicksSinceLastNap = 0;
            vm.api = new VmApi (
                    raiseShovelError: vm.RaiseShovelError, 
                    ticksIncrementer: vm.IncrementTicks,
                    cellsIncrementer: vm.IncrementCells,
                    cellsIncrementHerald: vm.IncrementCellsHerald);
            do {
            } while (vm.StepVm());
            return vm;
        }

        internal void WakeUp ()
        {
            this.shouldTakeANap = false;
        }

        internal Value CheckStackTop ()
        {
            if (this.stack.Count != 1) {
                Utils.Panic ();
            }
            return this.stack.Top ();
        }

        internal IEnumerable<Value> GetUsedStack() { return this.stack.GetUsedStack(); }
        internal VmEnvironment GetCurrentEnvironment() { return this.currentEnvironment; }

        #endregion

        #region Assembly-internal API
        internal void SerializeState (Stream s)
        {
            CheckVmWithoutError ();
            var ser = new Serialization.VmStateSerializer ();
            var usedStack = this.stack.GetUsedStack ();
            int stackIndex = ser.Serialize (usedStack);
            int envIndex = ser.Serialize (this.currentEnvironment);
            Serialization.Utils.WriteBytes (s, BitConverter.GetBytes (Shovel.Api.Version));
            Serialization.Utils.WriteBytes (s, BitConverter.GetBytes (stackIndex));
            Serialization.Utils.WriteBytes (s, BitConverter.GetBytes (envIndex));
            Serialization.Utils.WriteBytes (s, BitConverter.GetBytes (this.programCounter));
            Serialization.Utils.WriteBytes (s, Encoding.UTF8.GetBytes (Utils.GetBytecodeMd5 (this.bytecode)));
            Serialization.Utils.WriteBytes (s, Encoding.UTF8.GetBytes (Utils.GetSourcesMd5 (this.bytecode)));
            Serialization.Utils.WriteBytes (s, BitConverter.GetBytes (this.executedTicks));
            Serialization.Utils.WriteBytes (s, BitConverter.GetBytes (this.usedCells));
            ser.WriteToStream (s);
        }
        #endregion

        #region Private Helper Functions
        void DeserializeState (byte[] serializedState)
        {
            using (var ms = new MemoryStream(serializedState)) {
                Serialization.Utils.DeserializeWithMd5CheckSum (ms, str => {
                    var version = Serialization.Utils.ReadInt (str);
                    if (version > Shovel.Api.Version) {
                        throw new Exceptions.VersionNotSupportedException ();
                    }
                    var stackIndex = Serialization.Utils.ReadInt (str);
                    var envIndex = Serialization.Utils.ReadInt (str);
                    this.programCounter = Serialization.Utils.ReadInt (str);
                    var bytes = new byte[32];
                    str.Read (bytes, 0, 32);
                    var actualBytecodeMd5 = Encoding.UTF8.GetString (bytes);
                    if (actualBytecodeMd5 != Utils.GetBytecodeMd5 (this.bytecode)) {
                        throw new Exceptions.BytecodeDoesntMatchState ();
                    }
                    // Read and ignore the source MD5.
                    str.Read (bytes, 0, 32);
                    // Read the number of ticks executed so far.
                    this.executedTicks = Serialization.Utils.ReadLong (ms);
                    // Read the number of used cells.
                    this.usedCells = Serialization.Utils.ReadInt (ms);
                    var ser = new Serialization.VmStateSerializer ();
                    ser.Deserialize (str, version, reader => {
                        this.stack = new Stack ((Value[])reader (stackIndex));
                        this.currentEnvironment = (VmEnvironment)reader (envIndex);
                    }
                    );
                    return null;
                });
            }
        }

        static string DumpShovelValue (VmApi api, Value obj)
        {
            if (obj.Kind == Value.Kinds.String) {
                return Prim0.ShovelStringRepresentation (api, obj).stringValue;
            } else if (obj.Kind == Value.Kinds.Array) {
                return Prim0.ShovelStringRepresentation (api, obj).stringValue;
            } else if (obj.Kind == Value.Kinds.Integer) {
                return Prim0.ShovelStringRepresentation (api, obj).stringValue;
            } else if (obj.Kind == Value.Kinds.Double) {
                return Prim0.ShovelStringRepresentation (api, obj).stringValue;
            } else if (obj.Kind == Value.Kinds.Hash) {
                return Prim0.ShovelStringRepresentation (api, obj).stringValue;
            } else if (obj.Kind == Value.Kinds.Callable) {
                return Prim0.ShovelStringRepresentation (api, obj).stringValue;
            } else if (obj.Kind == Value.Kinds.Bool) {
                return Prim0.ShovelStringRepresentation (api, obj).stringValue;
            } else if (obj.Kind == Value.Kinds.Null) {
                return Prim0.ShovelStringRepresentation (api, obj).stringValue;
            } else if (obj.Kind == Value.Kinds.ReturnAddress) {
                return String.Format ("Return to {0}", obj.returnAddressValue.ProgramCounter);
            } else if (obj.Kind == Value.Kinds.NamedBlock) {
                return String.Format ("Named block {0} to {0}", obj.namedBlockValue.Name, obj.namedBlockValue.BlockEnd);
            } else {
                throw new InvalidOperationException ();
            }
        }

        void TraceInstruction (Instruction instruction)
        {
            Console.WriteLine ("*****");
            Console.WriteLine (instruction.ToString ());
            StringBuilder sb = new StringBuilder ();
            this.PrintLineFor (sb, this.programCounter, instruction.StartPos, instruction.EndPos);
            Console.WriteLine (sb.ToString ());
            Console.WriteLine ("Stack before:");
            for (var i = 0; i < this.stack.Count; i++) {
                Console.WriteLine (DumpShovelValue (this.api, this.stack.Storage [i]));
            }
        }

        bool StepVm ()
        {
            if (this.IsLive ()) {
                this.CheckVmWithoutError ();
                this.CheckQuotas ();
                try {
                    var instruction = this.CurrentInstruction();
                    //TraceInstruction(instruction);
                    Vm.handlers[instruction.NumericOpcode](this);
                    this.executedTicks++;
                    this.executedTicksSinceLastNap++;
                    return true;
                } catch (ShovelException ex) {
                    this.programmingError = ex;
                    throw;
                }
            } else {
                return false;
            }
        }

        static Action<Vm>[] handlers = new Action<Vm>[] {
            Vm.HandleJump,                     // 0
            Vm.HandleConst,                    // 1
            Vm.HandlePrim0,                    // 2
            Vm.HandlePrim,                     // 3
            Vm.HandleCall,                     // 4
            Vm.HandleCallj,                    // 5
            Vm.HandleFjump,                    // 6
            Vm.HandleLset,                     // 7
            Vm.HandlePop,                      // 8
            Vm.HandleLget,                     // 9
            Vm.HandleFn,                       // 10
            Vm.HandleNewFrame,                 // 11
            Vm.HandleDropFrame,                // 12
            Vm.HandleArgs,                     // 13
            Vm.HandleReturn,                   // 14
            Vm.HandleBlock,                    // 15
            Vm.HandlePopBlock,                 // 16
            Vm.HandleBlockReturn,              // 17
            Vm.HandleContext,                  // 18
            Vm.HandleTjump,                    // 19
            Vm.HandleNop,                      // 20
            Vm.HandleDiv,                      // 21
            Vm.HandleMod,                      // 22
            Vm.HandleNeq,                      // 23
            Vm.HandleLt,                       // 24
            Vm.HandleAdd,                      // 25
            Vm.HandleGref,                     // 26
            Vm.HandleEq,                       // 27
            Vm.HandleApush,                    // 28
            Vm.HandleGrefDot,                  // 29
            Vm.HandleSub,                      // 30
            Vm.HandleNeg,                      // 31
            Vm.HandleMul,                      // 32
            Vm.HandleShl,                      // 33
            Vm.HandleShr,                      // 34
            Vm.HandlePow,                      // 35
            Vm.HandleFloor,                    // 36
            Vm.HandleLte,                      // 37
            Vm.HandleGt,                       // 38
            Vm.HandleGte,                      // 39
            Vm.HandleNot,                      // 40
            Vm.HandleAnd,                      // 41
            Vm.HandleIor,                      // 42
            Vm.HandleXor,                      // 43
            Vm.HandleKeys,                     // 44
            Vm.HandleHasKey,                   // 45
            Vm.HandleApop,                     // 46
            Vm.HandleSetIndexed,               // 47
            Vm.HandleLen,                      // 48
            Vm.HandleIsString,                 // 49
            Vm.HandleIsHash,                   // 50
            Vm.HandleIsBool,                   // 51
            Vm.HandleIsArray,                  // 52
            Vm.HandleIsNumber,                 // 53
            Vm.HandleIsInteger,                // 54
            Vm.HandleIsCallable,               // 55
            Vm.HandleDelete,                   // 56
            Vm.HandleIsStruct,                 // 57
            Vm.HandleIsStructInstance,         // 58
            Vm.HandleSetDotIndexed,            // 59
            Vm.HandleApply,                    // 60
        };

        Instruction CurrentInstruction ()
        {
            return this.bytecode [this.programCounter];
        }

        internal object GetCurrentCache ()
        {
            return this.cache [this.programCounter];
        }

        internal void SetCurrentCache (object cache)
        {
            this.cache [this.programCounter] = cache;
        }

        static void HandleDiv (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.Divide (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }
    
        static void HandleMod (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.Modulo (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;            
        }

        static void HandleApop (Vm vm)
        {
            Prim0.ArrayPop (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleLen (Vm vm)
        {
            Prim0.GetLength (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleIsString (Vm vm)
        {
            Prim0.IsString (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleIsStruct (Vm vm)
        {
            Prim0.IsStruct (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleIsStructInstance (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.IsStructInstance (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleIsHash (Vm vm)
        {
            Prim0.IsHash (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleIsBool (Vm vm)
        {
            Prim0.IsBool (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleIsArray (Vm vm)
        {
            Prim0.IsArray (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleIsNumber (Vm vm)
        {
            Prim0.IsNumber (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleIsInteger (Vm vm)
        {
            Prim0.IsInteger (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleIsCallable (Vm vm)
        {
            Prim0.IsCallable (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleDelete (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.DeleteDictionary (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleLt (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.LessThan (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleAdd (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.Add (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleGref (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var callGetter = !Prim0.ArrayOrHashGet (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            if (callGetter)
            {
                var obj = vm.stack.Storage[start];
                if (obj.Kind == Value.Kinds.Hash)
                {
                    vm.stack.Push(obj.hashValue.IndirectGet);
                }
                else if (obj.Kind == Value.Kinds.Array)
                {
                    vm.stack.Push(obj.arrayValue.IndirectGet);
                }
                HandleCallImpl(vm, 2, true);
            }
            else
            {
                vm.stack.Pop();
                vm.programCounter++;
            }
        }

        static void HandleEq (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.AreEqual (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleNeq (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.AreNotEqual (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleLte (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.LessThanOrEqual (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleGt (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.GreaterThan (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleGte (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.GreaterThanOrEqual (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleApush (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.ArrayPush (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleGrefDot (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var callGetter = !Prim0.HashOrStructGetDot (vm, vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            if (callGetter)
            {
                var obj = vm.stack.Storage[start];
                if (obj.Kind == Value.Kinds.Hash) {
                    vm.stack.Push(obj.hashValue.IndirectGet);
                    HandleCallImpl(vm, 2, true);
                }
            } else { 
                vm.stack.Pop ();
                vm.programCounter++;
            }
        }

        static void HandleSetDotIndexed (Vm vm)
        {
            var start = vm.stack.Count - 3;
            var callSetter = !Prim0.HashOrStructDotSet(vm, vm.api,
                                          ref vm.stack.Storage[start],
                                          ref vm.stack.Storage[start + 1],
                                          ref vm.stack.Storage[start + 2]);
            if (!callSetter) { 
                vm.stack.PopMany(2);
                vm.programCounter++;
            }
            else
            {
                vm.stack.Push(vm.stack.Storage[start].hashValue.IndirectSet);
                HandleCallImpl(vm, 3, true);
            }
        }

        static void HandleSub (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.Subtract (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleAnd (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.BitwiseAnd (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleIor (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.BitwiseOr (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleXor (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.BitwiseXor (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleSetIndexed (Vm vm)
        {
            var start = vm.stack.Count - 3;
            var callSetter = !Prim0.ArrayOrHashSet (vm.api, 
                                  ref vm.stack.Storage [start], 
                                  ref vm.stack.Storage [start + 1],
                                  ref vm.stack.Storage [start + 2]);

            if (!callSetter)
            {
                vm.stack.PopMany(2);
                vm.programCounter++;
            }
            else
            {
                var obj = vm.stack.Storage[start];
                if (obj.Kind == Value.Kinds.Hash) 
                { 
                    vm.stack.Push(obj.hashValue.IndirectSet);
                }
                else if (obj.Kind == Value.Kinds.Array)
                {
                    vm.stack.Push(obj.arrayValue.IndirectSet);
                }
                HandleCallImpl(vm, 3, true);
            }
        }

        static void HandleNeg (Vm vm)
        {
            Prim0.UnaryMinus (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleNot (Vm vm)
        {
            Prim0.LogicalNot (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleKeys (Vm vm)
        {
            Prim0.Keys (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleMul (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.Multiply (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleHasKey (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.HasKey (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleShl (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.ShiftLeft (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleShr (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.ShiftRight (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandlePow (Vm vm)
        {
            var start = vm.stack.Count - 2;
            Prim0.Pow (vm.api, ref vm.stack.Storage [start], ref vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleFloor (Vm vm)
        {
            Prim0.Floor (vm.api, ref vm.stack.Storage [vm.stack.Count - 1]);
            vm.programCounter++;
        }

        static void HandleJump (Vm vm)
        {           
            vm.programCounter = (int)vm.CurrentInstruction ().Arguments;
        }

        static void HandleConst (Vm vm)
        {
            vm.stack.Push ((Value)vm.CurrentInstruction ().Arguments);
            vm.programCounter++;
            vm.IncrementCells (1);
        }

        static Dictionary<string, Callable> prim0Hash = null;

        static Dictionary<string, Callable> Prim0Hash {
            get {
                if (Vm.prim0Hash == null) {
                    Vm.prim0Hash = Prim0.GetPrim0Hash ();
                }
                return prim0Hash;
            }
        }

        static void HandlePrim0 (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            if (vm.GetCurrentCache () == null) {
                var primName = (string)instruction.Arguments;
                if (!Vm.Prim0Hash.ContainsKey (primName)) {
                    vm.RaiseShovelError (String.Format (
                        "Cannot take address of primitive '{0}' (implemented as instruction).",
                        primName)
                    );
                }
                vm.SetCurrentCache (Value.Make (Vm.Prim0Hash [primName]));
            }
            vm.stack.Push ((Value)vm.GetCurrentCache ());
            vm.IncrementTicks (1);
            vm.programCounter++;
        }

        static Callable GetUdpByName (Vm vm, string udpName)
        {
            if (vm.userPrimitives == null || !vm.userPrimitives.ContainsKey (udpName)) {
                vm.RaiseShovelError (String.Format (
                        "Unknown user primitive '{0}'.", udpName)
                );
            }
            return vm.userPrimitives [udpName];
        }

        static void HandlePrim (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            if (vm.GetCurrentCache () == null) {
                var udpName = (string)instruction.Arguments;
                vm.SetCurrentCache (Value.Make (GetUdpByName (vm, udpName)));
            }
            vm.stack.Push ((Value)vm.GetCurrentCache ());
            vm.IncrementTicks (1);
            vm.programCounter++;
        }

        static void HandleCall (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var numArgs = (int)instruction.Arguments;
            Vm.HandleCallImpl (vm, numArgs, true);
        }

        static void HandleApply(Vm vm)
        {
            var maybeArray = vm.stack.PopTop();
            var maybeCallable = vm.stack.PopTop();
            if (maybeArray.Kind != Value.Kinds.Array)
            {
                vm.RaiseShovelError(String.Format(
                    "Object [{0}] is not an array.", Prim0.ShovelStringRepresentation(vm.api, maybeArray))
                );
            }
            var numArgs = maybeArray.arrayValue.Count;
            foreach (var value in maybeArray.arrayValue)
            {
                vm.stack.Push((Value)value);
                vm.IncrementCells(1);
            }
            vm.stack.Push(maybeCallable);
            Vm.HandleCallImpl(vm, numArgs, true, true);
        }

        static void HandleCallImpl (Vm vm, int numArgs, bool saveReturnAddress, bool inApply = false)
        {
            var maybeCallable = vm.stack.Top ();
            if (maybeCallable.Kind != Value.Kinds.Callable) {
                vm.RaiseShovelError (String.Format (
                    "Object [{0}] is not callable.", Prim0.ShovelStringRepresentation (vm.api, maybeCallable))
                );
            }
            var callable = maybeCallable.callableValue;
            if (callable.ProgramCounter.HasValue) {
                vm.stack.Pop ();
                CallFunction (callable, vm, numArgs, saveReturnAddress, inApply);
                if (saveReturnAddress) {
                    vm.IncrementCells (1);
                }
            } else {
                CallPrimitive (callable, vm, numArgs, saveReturnAddress, inApply);
            }
        }

        static void CallFunction (Callable callable, Vm vm, int numArgs, bool saveReturnAddress, bool inApply)
        {
            if (callable.Arity != null)
            {
                // FIXME: Find out why/when the arity can be null.
                if (callable.HasCollectParams)
                {
                    if (callable.Arity.Value <= numArgs)
                    {
                        var extraArgs = numArgs - callable.Arity.Value;
                        var values = new ArrayInstance();
                        for (int i = 0; i < extraArgs; i++) {
                            values.Add(vm.stack.Storage[i + vm.stack.Count - extraArgs]);
                        }
                        vm.stack.PopMany(extraArgs);
                        vm.stack.Push(Value.Make(values));
                    }
                    else
                    {
                        ArityError(vm, callable.Arity.Value, numArgs, inApply);
                    }
                }
                else if (callable.Arity.Value != numArgs)
                {
                    ArityError(vm, callable.Arity.Value, numArgs, inApply);
                }
            }
            if (saveReturnAddress)
            {
                vm.stack.Push (Value.Make (new ReturnAddress () {
                    ProgramCounter = vm.programCounter + 1,
                    Environment = vm.currentEnvironment
                }));
            }
            vm.currentEnvironment = callable.Environment;
            vm.programCounter = callable.ProgramCounter.Value;
        }

        static void FinishPrimitiveCall (
            Vm vm, int numArgs, bool saveReturnAddress, Value result)
        {
            vm.stack.PopMany (numArgs);
            if (saveReturnAddress) {
                vm.programCounter ++;
            } else {
                var maybeRa = vm.stack.PopTop ();
                if (maybeRa.Kind == Value.Kinds.ReturnAddress) {
                    vm.ApplyReturnAddress (maybeRa.returnAddressValue);
                } else {
                    Utils.Panic ();
                }
            }
            vm.stack.Push (result);
            vm.IncrementCells (1);
        }

        static void CallPrimitive (Callable callable, Vm vm, int numArgs, bool saveReturnAddress, bool inApply)
        {
            bool isRequiredPrimitive = false;
            if (callable.Prim0Name != null) {
                isRequiredPrimitive = true;
                if (callable.RequiredPrimitive == null) {
                    callable.RequiredPrimitive = Vm.Prim0Hash [callable.Prim0Name].RequiredPrimitive;
                }
            } else if (callable.UserDefinedPrimitive == null) {
                callable.UserDefinedPrimitive = GetUdpByName (vm, callable.UdpName).UserDefinedPrimitive;
            }

            if (callable.Arity != null && callable.Arity.Value != numArgs) {
                ArityError (vm, callable.Arity.Value, numArgs, inApply);
            }

            Value[] array;
            int start;
            if (isRequiredPrimitive) {
                vm.stack.Pop (); // Remove the callable.
                vm.stack.GetTopRange (numArgs, out array, out start);
                FinishPrimitiveCall (
                    vm, numArgs, saveReturnAddress, 
                    callable.RequiredPrimitive (vm.api, array, start, numArgs));
            } else {
                vm.stack.GetTopRange (numArgs + 1, out array, out start);
                var udpResult = new UdpResult ();
                Value[] args = new Value[numArgs];
                Array.Copy (vm.stack.Storage, start, args, 0, numArgs);
                try {
                    callable.UserDefinedPrimitive (vm.api, args, udpResult);
                } catch (Exception ex) {
                    vm.userDefinedPrimitiveError = ex;
                    vm.shouldTakeANap = true;
                }
                switch (udpResult.After) {
                case UdpResult.AfterCall.Continue:
                    vm.stack.Pop ();
                    FinishPrimitiveCall (vm, numArgs, saveReturnAddress, udpResult.Result);
                    break;
                case UdpResult.AfterCall.Nap:
                    vm.stack.Pop ();
                    FinishPrimitiveCall (vm, numArgs, saveReturnAddress, udpResult.Result);
                    vm.shouldTakeANap = true;
                    break;
                case UdpResult.AfterCall.NapAndRetryOnWakeUp:
                    vm.shouldTakeANap = true;
                    break;
                }
            }
        }

        static void ArityError (Vm vm, int expectedArity, int actualArity, bool inApply)
        {
            var message = "Function of {0} arguments called with {1} arguments.";
            if (inApply)
            {
                message = "The first argument of 'apply', a function of {0} arguments, was called with {1} arguments.";
            }
            vm.RaiseShovelError (String.Format (
                message,
                expectedArity, actualArity));
        }

        static void HandleCallj (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var numArgs = (int)instruction.Arguments;
            Vm.HandleCallImpl (vm, numArgs, false);
        }

        void CheckBool ()
        {
            if (this.stack.Top ().Kind != Value.Kinds.Bool) {
                RaiseShovelError ("Argument must be a boolean.");
            }
        }

        static void HandleFjump (Vm vm)
        {
            var args = (int)vm.CurrentInstruction ().Arguments;
//            vm.CheckBool ();
            if (!vm.stack.PopTop ().boolValue) {
                vm.programCounter = args;
            } else {
                vm.programCounter ++;
            }
        }

        static void HandleTjump (Vm vm)
        {
            var args = (int)vm.CurrentInstruction ().Arguments;
//            vm.CheckBool ();
            if (vm.stack.PopTop ().boolValue) {
                vm.programCounter = args;
            } else {
                vm.programCounter ++;
            }
        }

        static void HandleLset (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var args = (int[])instruction.Arguments;
            SetInEnvironment (vm.currentEnvironment, args [0], args [1], vm.stack.Top ());
            vm.programCounter++;
        }

        static void SetInEnvironment (
            VmEnvironment env, int frameNumber, int varIndex, Value value)
        {
            FindFrame (env, frameNumber).ValuesInernal [varIndex] = value;
        }

        static VmEnvFrame FindFrame (VmEnvironment env, int frameNumber)
        {
            while (env != null && frameNumber > 0) {
                env = env.Next;
                frameNumber --;
            }
            if (env == null) {
                Utils.Panic ();
            }
            return env.Frame;
        }

        static void HandlePop (Vm vm)
        {
            vm.stack.Pop ();
            vm.programCounter++;
        }

        static void HandleLget (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var args = (int[])instruction.Arguments;
            vm.stack.Push (FindFrame (vm.currentEnvironment, args [0]).ValuesInernal [args [1]]);
            vm.IncrementCells (1);
            vm.programCounter++;
        }

        static Value GetFromEnvironment (VmEnvironment env, int frameNumber, int varIndex)
        {
            return FindFrame (env, frameNumber).ValuesInernal [varIndex];
        }

        static void HandleFn (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var args = (int[])instruction.Arguments;
            var arity = args[1];
            var hasCollectParameters = arity >= Callable.CollectParamsArityModifier;
            if (hasCollectParameters)
            {
                arity -= Callable.CollectParamsArityModifier;
            }
            var callable = new Callable () {
                ProgramCounter = args[0],
                Arity = arity,
                HasCollectParams = hasCollectParameters,
                Environment = vm.currentEnvironment
            };
            vm.stack.Push (Value.Make (callable));
            vm.IncrementCells (5);
            vm.programCounter++;
        }

        static void HandleNewFrame (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var args = (string[])instruction.Arguments;
            var frame = new VmEnvFrame () {
                VarNamesInternal = args,
                ValuesInernal = new Value[args.Length],
                IntroducedAtProgramCounter = vm.programCounter
            };
            var newEnv = new VmEnvironment () {
                Frame = frame,
                Next = vm.currentEnvironment
            };
            vm.currentEnvironment = newEnv;
            vm.IncrementCells (args.Length * 3 + 5);
            vm.programCounter++;
        }

        static void HandleDropFrame (Vm vm)
        {
            vm.currentEnvironment = vm.currentEnvironment.Next;
            vm.programCounter++;
        }

        static void HandleArgs (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var argCount = (int)instruction.Arguments;
            if (argCount > 0) {
                Value? returnAddress = null;
                if (vm.stack.Top ().Kind == Value.Kinds.ReturnAddress) {
                    returnAddress = vm.stack.PopTop ();
                }
                var values = vm.currentEnvironment.Frame.ValuesInernal;
                Array.Copy (
                    vm.stack.Storage, vm.stack.Count - argCount, 
                    values, 0, argCount);
                vm.stack.PopMany (argCount);
                if (returnAddress.HasValue) {
                    vm.stack.Push (returnAddress.Value);
                }
            }
            vm.programCounter++;
        }

        static void HandleArgs1 (Vm vm)
        {
            if (vm.stack.TopIsReturnAddress ()) {
                vm.currentEnvironment.Frame.ValuesInernal [0] = vm.stack.UnderTopOne ();
                vm.stack.UnderPopOneAndCopyTop ();
            } else {
                vm.currentEnvironment.Frame.ValuesInernal [0] = vm.stack.PopTop ();
            }
            vm.programCounter++;
        }

        static void HandleArgs2 (Vm vm)
        {
            if (vm.stack.TopIsReturnAddress ()) {
                vm.currentEnvironment.Frame.ValuesInernal [0] = vm.stack.UnderTop (2);
                vm.currentEnvironment.Frame.ValuesInernal [1] = vm.stack.UnderTop (1);
                vm.stack.UnderPopAndCopyTop (2);
            } else {
                vm.currentEnvironment.Frame.ValuesInernal [1] = vm.stack.PopTop ();
                vm.currentEnvironment.Frame.ValuesInernal [0] = vm.stack.PopTop ();
            }
            vm.programCounter++;
        }

        static void HandleArgs3 (Vm vm)
        {
            if (vm.stack.TopIsReturnAddress ()) {
                vm.currentEnvironment.Frame.ValuesInernal [0] = vm.stack.UnderTop (3);
                vm.currentEnvironment.Frame.ValuesInernal [1] = vm.stack.UnderTop (2);
                vm.currentEnvironment.Frame.ValuesInernal [2] = vm.stack.UnderTop (1);
                vm.stack.UnderPopAndCopyTop (3);
            } else {
                vm.currentEnvironment.Frame.ValuesInernal [2] = vm.stack.PopTop ();
                vm.currentEnvironment.Frame.ValuesInernal [1] = vm.stack.PopTop ();
                vm.currentEnvironment.Frame.ValuesInernal [0] = vm.stack.PopTop ();
            }
            vm.programCounter++;
        }

        static void HandleReturn (Vm vm)
        {
            // SLOW: maybe there's a faster way to manipulate the stack here?
            var result = vm.stack.PopTop ();
            var maybeRa = vm.stack.PopTop ();
            if (maybeRa.Kind == Value.Kinds.ReturnAddress) {
                vm.ApplyReturnAddress (maybeRa.returnAddressValue);
            } else {
                Utils.Panic ();
            }
            vm.stack.Push (result);
        }

        void ApplyReturnAddress (ReturnAddress returnAddress)
        {
            this.programCounter = returnAddress.ProgramCounter;
            this.currentEnvironment = returnAddress.Environment;
        }

        static void HandleBlock (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var blockEnd = (int)instruction.Arguments;
            var name = vm.stack.PopTop ();
            if (name.Kind != Value.Kinds.String) {
                vm.RaiseShovelError ("The name of a block must be a string.");
            }
            vm.stack.Push (Value.Make (new NamedBlock () {
                Name = name.stringValue,
                BlockEnd = blockEnd,
                Environment = vm.currentEnvironment
            }
            )
            );
            vm.IncrementCells (3);
            vm.programCounter++;
        }

        static void HandlePopBlock (Vm vm)
        {
            var returnValue = vm.stack.PopTop ();
            var namedBlock = vm.stack.PopTop ();
            if (namedBlock.Kind != Value.Kinds.NamedBlock) {
                vm.RaiseShovelError ("Invalid context for POP_BLOCK.");
            }
            vm.stack.Push (returnValue);
            vm.programCounter++;
        }

        static void HandleBlockReturn (Vm vm)
        {
            var returnValue = vm.stack.PopTop ();
            var name = vm.stack.PopTop ();
            if (name.Kind != Value.Kinds.String) {
                vm.RaiseShovelError ("The name of a block must be a string.");
            }
            var namedBlockIndex = vm.FindNamedBlock (name.stringValue);
            if (vm.stack.Count > namedBlockIndex + 1) {
                vm.stack.RemoveRange (namedBlockIndex + 1, vm.stack.Count - namedBlockIndex - 1);
            }
            var namedBlock = vm.stack.Top ().namedBlockValue;
            vm.stack.Push (returnValue);
            vm.programCounter = namedBlock.BlockEnd;
            vm.currentEnvironment = namedBlock.Environment;
        }

        int FindNamedBlock (string blockName)
        {
            for (var i = this.stack.Count - 1; i >= 0; i--) {
                if (this.stack.Storage [i].Kind == Value.Kinds.NamedBlock
                    && this.stack.Storage [i].namedBlockValue.Name == blockName) {
                    return i;
                }
            }
            this.RaiseShovelError (
                String.Format ("Cannot find block '{0}'.", blockName));
            return -1;
        }

        static void HandleContext (Vm vm)
        {
            var stackTraceSb = new StringBuilder ();
            vm.WriteStackTrace (stackTraceSb);
            var stackTrace = stackTraceSb.ToString ();
            var currentEnvironmentSb = new StringBuilder ();
            vm.WriteCurrentEnvironment (currentEnvironmentSb);
            var currentEnvironment = currentEnvironmentSb.ToString ();
            var result = new HashInstance ();
            result.Add (Value.Make ("stack"), Value.Make (stackTrace));
            result.Add (Value.Make ("environment"), Value.Make (currentEnvironment));
            vm.IncrementCells (6 + stackTrace.Length + currentEnvironment.Length);
            vm.stack.Push (Value.Make (result));
            vm.programCounter ++;
            return;
        }

        static void HandleNop (Vm vm)
        {
            vm.programCounter++;
        }

        internal bool IsLive ()
        {
            return !(this.programCounter == this.bytecode.Length || this.shouldTakeANap);
        }

        internal bool ExecutionComplete ()
        {
            return this.programCounter == this.bytecode.Length;
        }

        int CountCellsSvList (List<Value> list, HashSet<object> visited)
        {
            var sum = list.Count;
            visited.Add (list);
            foreach (var el in list) {
                sum += CountCellsImpl (el, visited);
            }
            return sum;
        }

        int CountCellsHash (Dictionary<Value, Value> hash, HashSet<object> visited)
        {
            var sum = hash.Count * 2;
            visited.Add (hash);
            foreach (var kv in hash) {
                sum += CountCellsImpl (kv.Key, visited);
                sum += CountCellsImpl (kv.Value, visited);
            }
            return sum;
        }

        int CountCellsString (string s)
        {
            if (s == null) {
                return 0;
            } else {
                return s.Length;
            }
        }

        int CountCellsNullableInt (int? i)
        {
            if (i.HasValue) {
                return 1;
            } else {
                return 0;
            }
        }

        int CountCellsCallable (Callable callable, HashSet<object> visited)
        {
            var sum = 5;
            visited.Add (callable);
            sum += CountCellsString (callable.UdpName);
            sum += CountCellsString (callable.Prim0Name);
            sum += CountCellsImpl (callable.Environment, visited);
            return sum;
        }

        int CountCellsReturnAddress (ReturnAddress returnAddress, HashSet<object> visited)
        {
            var sum = 2;
            visited.Add (returnAddress);

            sum += CountCellsImpl (returnAddress.Environment, visited);

            return sum;
        }

        int CountCellsNamedBlock (NamedBlock namedBlock, HashSet<object> visited)
        {
            var sum = 3;
            visited.Add (namedBlock);

            sum += CountCellsString (namedBlock.Name);
            sum += CountCellsImpl (namedBlock.Environment, visited);

            return sum;
        }

        int CountCellsSvArray (Value[] values, HashSet<object> visited)
        {
            var sum = values.Length;
            visited.Add (values);
            foreach (var el in values) {
                sum += CountCellsImpl (el, visited);
            }
            return sum;
        }

        int CountCellsEnvironment (VmEnvironment env, HashSet<object> visited)
        {
            var sum = 3;
            visited.Add (env);

            sum += CountCellsImpl (env.Frame, visited);
            sum += CountCellsImpl (env.Next, visited);

            return sum;
        }

        int CountCellsEnvFrame (VmEnvFrame envFrame, HashSet<object> visited)
        {
            var sum = 3;
            visited.Add (envFrame);

            sum += CountCellsImpl (envFrame.VarNamesInternal, visited);
            sum += CountCellsImpl (envFrame.ValuesInernal, visited);

            return sum;
        }

        int CountCellsStringArray (string[] strings, HashSet<object> visited)
        {
            var sum = strings.Length;
            visited.Add (strings);
            foreach (var str in strings) {
                sum += CountCellsString (str);
            }

            return sum;
        }

        int CountCellsImpl (object obj, HashSet<object> visited)
        {
            if (obj == null) {
                return 0;
            } else if (obj is Value) {
                var sv = (Value)obj;
                switch (sv.Kind) {
                case Value.Kinds.Null:
                    return 1;
                case Value.Kinds.Integer:
                    return 1;
                case Value.Kinds.String:
                    return sv.stringValue.Length + 1;
                case Value.Kinds.Double:
                    return 1;
                case Value.Kinds.Bool:
                    return 1;
                case Value.Kinds.Array:
                    return 1 + CountCellsSvList (sv.arrayValue, visited);
                case Value.Kinds.Hash:
                    return 1 + CountCellsHash (sv.hashValue, visited);
                case Value.Kinds.Callable:
                    return 1 + CountCellsCallable (sv.callableValue, visited);
                case Value.Kinds.ReturnAddress:
                    return 1 + CountCellsReturnAddress (sv.returnAddressValue, visited);
                case Value.Kinds.NamedBlock:
                    return 1 + CountCellsNamedBlock (sv.namedBlockValue, visited);
                case Value.Kinds.Struct:
                    return 1 + CountCellsStringArray (sv.StructValue.Fields, visited);
                case Value.Kinds.StructInstance:
                    return 1 + CountCellsSvArray (sv.structInstanceValue.values, visited);
                default:
                    Utils.Panic ();
                    return 0;
                }
            } else if (visited.Contains (obj)) {
                return 0;
            } else if (obj is Value[]) {
                return CountCellsSvArray ((Value[])obj, visited);
            } else if (obj is List<Value>) {
                return CountCellsSvList ((List<Value>)obj, visited);
            } else if (obj is VmEnvironment) {
                return CountCellsEnvironment ((VmEnvironment)obj, visited);
            } else if (obj is VmEnvFrame) {
                return CountCellsEnvFrame ((VmEnvFrame)obj, visited);
            } else if (obj is string[]) {
                return CountCellsStringArray ((string[])obj, visited);
            } else {
                Utils.Panic ();
                return 0;
            }
        }

        int CountCells ()
        {
            var visited = new HashSet<object> ();
            return CountCellsImpl (this.stack.GetUsedStack (), visited) 
                + CountCellsImpl (this.currentEnvironment, visited);
        }

        void CheckQuotas ()
        {
            if (this.cellsQuota.HasValue) {
                if (this.usedCells > this.cellsQuota.Value) {
                    this.usedCells = this.CountCells ();
                    if (this.usedCells > this.cellsQuota.Value) {
                        throw new Shovel.Exceptions.ShovelCellQuotaExceededException ();
                    }
                }
            }

            if (this.totalTicksQuota.HasValue) {
                if (this.executedTicks > this.totalTicksQuota.Value) {
                    throw new Shovel.Exceptions.ShovelTicksQuotaExceededException ();
                }
            }

            if (this.untilNextNapTicksQuota.HasValue) {
                if (this.executedTicksSinceLastNap > this.untilNextNapTicksQuota) {
                    this.shouldTakeANap = true;
                }
            }
        }

        void CheckVmWithoutError ()
        {
            if (this.userDefinedPrimitiveError != null) {
                throw new ShovelException( "An exception has been thrown in a user-defined primitive.", this.userDefinedPrimitiveError );
            }
            if (this.programmingError != null) {
                throw new ShovelException( "An exception has occurred in the Shovel program.", this.programmingError );
            }
        }

        void RaiseShovelError (string message)
        {
            var sb = new StringBuilder ();
            sb.AppendLine (message);
            sb.AppendLine ();
            sb.AppendLine ("Current stack trace:");
            this.WriteStackTrace (sb);
            sb.AppendLine ();
            sb.AppendLine ("Current environment:");
            sb.AppendLine ();
            this.WriteCurrentEnvironment (sb);
            var fileName = this.FindFileName (this.programCounter);
            int? line = null, column = null;
            if (fileName != null && this.sources != null) {
                var source = SourceFile.FindSource (this.sources, fileName);
                if (source != null) {
                    int? startPos, endPos;
                    this.FindStartEndPos (out startPos, out endPos);
                    if (startPos != null) {
                        var pos = Position.CalculatePosition (source, startPos.Value);
                        if (pos != null) {
                            line = pos.Line;
                            column = pos.Column;
                        }
                    }
                }
            }
            throw new ShovelException (){
                    ShovelMessage = sb.ToString(),
                    FileName = fileName,
                    Line = line,
                    Column = column
                };
        }

        string FindFileName (int programCounter)
        {
            for (var i = programCounter; i >= 0; i--) {
                var instruction = this.bytecode [i];
                if (instruction.Opcode == Instruction.Opcodes.FileName) {
                    return (string)instruction.Arguments;
                }
            }
            return null;
        }

        void FindStartEndPos (out int? startPos, out int? endPos)
        {
            startPos = null;
            endPos = null;
            for (var i = this.programCounter; i >= 0; i--) {
                var instruction = this.bytecode [i];
                if (instruction.StartPos != null && instruction.EndPos != null) {
                    startPos = instruction.StartPos;
                    endPos = instruction.EndPos;
                    return;
                }
            }
        }

        void WriteCurrentEnvironment (StringBuilder sb)
        {
            for (var env = this.currentEnvironment; env != null; env = env.Next) {
                if (env.Frame.IntroducedAtProgramCounter != null) {
                    sb.AppendLine ("Frame starts at:");
                    var pc = env.Frame.IntroducedAtProgramCounter.Value;
                    var instruction = this.bytecode [pc];
                    this.PrintLineFor (sb, pc, instruction.StartPos, instruction.StartPos);
                }
                sb.AppendLine ("Frame variables are:");
                for (var i = 0; i < env.Frame.VarNamesInternal.Length; i++) {
                    sb.AppendLine (String.Format (
                        "{0} = {1}",
                        env.Frame.VarNamesInternal [i],
                        Prim0.ShovelStringRepresentation (this.api, env.Frame.ValuesInernal [i]).stringValue)
                    );
                }
                sb.AppendLine ();
            }
        }

        void PrintLineFor (StringBuilder sb, int pc, int? characterStartPos, int? characterEndPos)
        {
            var foundLocation = false;
            if (characterStartPos != null && characterEndPos != null) {
                if (this.sources != null) {
                    var fileName = this.FindFileName (pc);
                    var sourceFile = SourceFile.FindSource (this.sources, fileName);
                    if (sourceFile != null) {
                        var startPos = Position.CalculatePosition (sourceFile, characterStartPos.Value);
                        var endPos = Position.CalculatePosition (sourceFile, characterEndPos.Value);
                        var lines = Utils.ExtractRelevantSource (sourceFile.Content.Split ('\n'), startPos, endPos);
                        foreach (var line in lines) {
                            sb.AppendLine (line);
                        }
                        foundLocation = true;
                    }
                }
            }
            if (!foundLocation) {
                sb.AppendLine (String.Format (
                    "... unknown source location, program counter {0} ...",
                    pc)
                );
            }
        }

        void WriteStackTrace (StringBuilder sb)
        {
            int? startPos, endPos;
            this.FindStartEndPos (out startPos, out endPos);
            this.PrintLineFor (sb, this.programCounter, startPos, endPos);
            for (var i = this.stack.Count - 1; i >= 0; i--) {
                if (this.stack.Storage [i].Kind == Value.Kinds.ReturnAddress) {
                    var ra = this.stack.Storage [i].returnAddressValue;
                    var pc = ra.ProgramCounter;
                    var callSite = this.bytecode [pc - 1];
                    this.PrintLineFor (sb, pc, callSite.StartPos, callSite.EndPos);
                }
            }
        }

        void IncrementTicks (int ticks)
        {
            this.executedTicks += ticks;
            this.executedTicksSinceLastNap += ticks;
        }

        void IncrementCells (int cells)
        {
            this.usedCells += cells;
        }

        void IncrementCellsHerald (int cells)
        {
            if (this.cellsQuota != null && cells > this.cellsQuota.Value) {
                throw new ShovelCellQuotaExceededException ();
            }
        }
        #endregion
    }
}

