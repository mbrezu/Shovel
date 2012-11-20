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

namespace Shovel.Vm
{
    public class Vm
    {
        Instruction[] bytecode = null;
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
        Action<Vm>[] runs = null;
        VmApi api = null;

        public static Vm RunVm (
            Instruction[] bytecode, 
            List<SourceFile> sources = null,
            List<Callable> userPrimitives = null,
            byte[] state = null,
            Vm vm = null,
            int? cellsQuota = null,
            long? totalTicksQuota = null,
            long? untilNextNapTicksQuota = null)
        {
            if (vm == null) {
                vm = new Vm ();
                vm.bytecode = bytecode;
                vm.programCounter = 0;
                vm.currentEnvironment = null;
                vm.stack = new Stack ();
                vm.sources = sources;
                vm.userPrimitives = new Dictionary<string, Callable> ();
                vm.runs = new Action<Vm>[bytecode.Length];
            }
            vm.cellsQuota = cellsQuota;
            vm.totalTicksQuota = totalTicksQuota;
            vm.untilNextNapTicksQuota = untilNextNapTicksQuota;
            if (state != null) {
                // TODO: implement state serialization/deserialization.
                throw new NotImplementedException ();
            }
            if (userPrimitives != null) {
                foreach (var udp in userPrimitives) {
                    vm.userPrimitives [udp.UdpName] = udp;
                }
            }
            vm.executedTicksSinceLastNap = 0;
            try {
                vm.api = new VmApi (
                    raiseShovelError: vm.RaiseShovelError, 
                    ticksIncrementer: vm.IncrementTicks,
                    cellsIncrementer: vm.IncrementCells,
                    cellsIncrementHerald: vm.IncrementCellsHerald);
            } catch (ShovelException ex) {
                vm.programmingError = ex;
            }
            while (vm.IsLive()) {
                vm.StepVm ();
            }
            return vm;
        }

        public ShovelValue CheckStackTop ()
        {
            if (this.stack.Count != 1) {
                Utils.Panic ();
            }
            return this.stack.Top ();
        }

        Action<Vm> GenRun ()
        {
            Instruction.Opcodes? lastOpcode = null;
            string lastPrim0 = null;
            var iter = this.programCounter;
            var lgets = new Dictionary<Tuple<int, int>, int> ();
            while (true) {
                var instruction = this.bytecode [iter];
                var op = instruction.Opcode;
                if (op == Instruction.Opcodes.Prim0) {
                    lastPrim0 = (string)instruction.Arguments;
                }
                if ((op == Instruction.Opcodes.Call) && lastOpcode != Instruction.Opcodes.Prim0) {
                    break;
                }
                if ((op == Instruction.Opcodes.Lset) 
                    || (op == Instruction.Opcodes.Jump)
                    || (op == Instruction.Opcodes.Fjump)
                    || (op == Instruction.Opcodes.Tjump)
                    || (op == Instruction.Opcodes.DropFrame)
                    || (op == Instruction.Opcodes.Args)
                    || (op == Instruction.Opcodes.Return)
                    || (op == Instruction.Opcodes.CallJ)
                    || (op == Instruction.Opcodes.BlockReturn)) {
                    break;
                }
                if ((op == Instruction.Opcodes.NewFrame) 
                    && (this.bytecode [iter + 1].Opcode != Instruction.Opcodes.Args)) {
                    break;
                }
                if ((op == Instruction.Opcodes.Call) 
                    && (lastOpcode == Instruction.Opcodes.Prim0)
                    && this.IsRunStopper (lastPrim0)) {
                    break;
                }
                if (iter == this.bytecode.Length - 1) {
                    break;
                }
                lastOpcode = op;
                if (op == Instruction.Opcodes.Lget) {
                    var args = (int[])instruction.Arguments;
                    var key = Tuple.Create (args [0], args [1]);
                    if (!lgets.ContainsKey (key)) {
                        lgets [key] = 0;
                    }
                    lgets [key]++;
                }
                iter++;
            }
            var runActions = new Action<Vm>[iter - this.programCounter + 1];
            var optimizableLgetCount = lgets.Keys.Select (key => lgets [key] > 1).Count ();
            var values = new ShovelValue[optimizableLgetCount];
            var lgetValueIndexes = new Dictionary<Tuple<int, int>, int> ();
            var runningIndex = 0;
            for (var i = this.programCounter; i <= iter; i++) {
                var instruction = this.bytecode [i];
                var alreadyCompiled = false;
                if (instruction.Opcode == Instruction.Opcodes.Lget) {
                    var args = (int[])instruction.Arguments;
                    var key = Tuple.Create (args [0], args [1]);
                    var count = lgets [key];
                    if (count > 1) {
                        if (lgetValueIndexes.ContainsKey (key)) {
                            var index = lgetValueIndexes [key];
                            runActions [i - this.programCounter] = vm => {
                                vm.stack.Push (values [index]);
                                vm.IncrementCells (1);
                                vm.programCounter++;
                            };
                            alreadyCompiled = true;
                        } else {
                            var index = runningIndex;
                            runningIndex ++;
                            lgetValueIndexes [key] = index;
                            runActions [i - this.programCounter] = vm => {
                                var result = GetFromEnvironment (vm.currentEnvironment, args [0], args [1]);
                                values [index] = result;
                                vm.stack.Push (result);
                                vm.IncrementCells (1);
                                vm.programCounter++;
                            };
                            alreadyCompiled = true;
                        }
                    }
                }
                if (instruction.Opcode == Instruction.Opcodes.Args) {
                    var args = (int)instruction.Arguments;
                    if (args == 1) {
                        runActions [i - this.programCounter] = HandleArgs1;
                        alreadyCompiled = true;
                    } else if (args == 2) {
                        runActions [i - this.programCounter] = HandleArgs2;
                        alreadyCompiled = true;
                    } else if (args == 3) {
                        runActions [i - this.programCounter] = HandleArgs3;
                        alreadyCompiled = true;
                    }
                }
                if (!alreadyCompiled) {
                    runActions [i - this.programCounter] = Vm.handlers [instruction.NumericOpcode];
                }
            }
            return vm => {
                foreach (var action in runActions) {
                    action (vm);
                }
            };
        }

        bool IsRunStopper (string lastPrim0)
        {
            return lastPrim0 == "arrayN" 
                || lastPrim0 == "stringRepresentation"
                || lastPrim0 == "svm_set_indexed";
        }

        static string DumpShovelValue (VmApi api, ShovelValue obj)
        {
            if (obj.Kind == ShovelValue.Kinds.String) {
                return obj.StringValue;
            } else if (obj.Kind == ShovelValue.Kinds.Array) {
                return "[...array...]";
            } else if (obj.Kind == ShovelValue.Kinds.Integer) {
                return obj.IntegerValue.ToString (CultureInfo.InvariantCulture);
            } else if (obj.Kind == ShovelValue.Kinds.Double) {
                return obj.DoubleValue.ToString (CultureInfo.InvariantCulture);
            } else if (obj.Kind == ShovelValue.Kinds.Hash) {
                return "[...hash...]";
            } else if (obj.Kind == ShovelValue.Kinds.Callable) {
                return "[...callable...]";
            } else if (obj.Kind == ShovelValue.Kinds.Bool) {
                return obj.BoolValue.ToString ().ToLower ();
            } else if (obj.Kind == ShovelValue.Kinds.Null) {
                return "null";
            } else if (obj.Kind == ShovelValue.Kinds.ReturnAddress) {
                return String.Format ("Return to {0}", obj.ReturnAddressValue.ProgramCounter);
            } else {
                throw new InvalidOperationException ();
            }
        }

        bool StepVm ()
        {
            this.CheckVmWithoutError ();
            this.CheckTicksQuota ();
            this.CheckCellsQuota ();
            if (this.IsLive ()) {
                if (this.runs [this.programCounter] == null) {
                    this.runs [this.programCounter] = this.GenRun ();
                }
                this.runs [this.programCounter] (this);

//                var instruction = this.CurrentInstruction ();
//                Console.WriteLine ("*****");
//                Console.WriteLine (instruction.ToString ());
//                for (var i = 0; i < this.stack.Count; i++) {
//                    Console.WriteLine (DumpShovelValue (this.api, this.stack.Storage [i]));
//                }
//                Vm.handlers [instruction.NumericOpcode] (this);
            }
            return this.IsLive ();
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
        };

        Instruction CurrentInstruction ()
        {
            return this.bytecode [this.programCounter];
        }

        static void HandleDiv (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.Divide (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }
    
        static void HandleMod (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.Modulo (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;            
        }

        static void HandleNeq (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.AreNotEqual (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleApop (Vm vm)
        {
            vm.stack.SetTop (Prim0.ArrayPop (vm.api, vm.stack.Storage [vm.stack.Count - 1]));
            vm.programCounter++;
        }

        static void HandleLen (Vm vm)
        {
            vm.stack.SetTop (Prim0.GetLength (vm.api, vm.stack.Storage [vm.stack.Count - 1]));
            vm.programCounter++;
        }

        static void HandleIsString (Vm vm)
        {
            vm.stack.SetTop (Prim0.IsString (vm.api, vm.stack.Storage [vm.stack.Count - 1]));
            vm.programCounter++;
        }

        static void HandleIsHash (Vm vm)
        {
            vm.stack.SetTop (Prim0.IsHash (vm.api, vm.stack.Storage [vm.stack.Count - 1]));
            vm.programCounter++;
        }

        static void HandleIsBool (Vm vm)
        {
            vm.stack.SetTop (Prim0.IsBool (vm.api, vm.stack.Storage [vm.stack.Count - 1]));
            vm.programCounter++;
        }

        static void HandleIsArray (Vm vm)
        {
            vm.stack.SetTop (Prim0.IsArray (vm.api, vm.stack.Storage [vm.stack.Count - 1]));
            vm.programCounter++;
        }

        static void HandleIsNumber (Vm vm)
        {
            vm.stack.SetTop (Prim0.IsNumber (vm.api, vm.stack.Storage [vm.stack.Count - 1]));
            vm.programCounter++;
        }

        static void HandleIsInteger (Vm vm)
        {
            vm.stack.SetTop (Prim0.IsInteger (vm.api, vm.stack.Storage [vm.stack.Count - 1]));
            vm.programCounter++;
        }

        static void HandleIsCallable (Vm vm)
        {
            vm.stack.SetTop (Prim0.IsCallable (vm.api, vm.stack.Storage [vm.stack.Count - 1]));
            vm.programCounter++;
        }

        static void HandleLt (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.LessThan (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleAdd (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.Add (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleGref (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.ArrayOrHashGet (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleEq (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.AreEqual (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleLte (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.LessThanOrEqual (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleGt (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.GreaterThan (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleGte (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.GreaterThanOrEqual (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleApush (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.ArrayPush (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleGrefDot (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.HashGetDot (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleSub (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.Subtract (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleAnd (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.BitwiseAnd (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleIor (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.BitwiseOr (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleXor (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.BitwiseXor (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleSetIndexed (Vm vm)
        {
            var start = vm.stack.Count - 3;
            var result = Prim0.ArrayOrHashSet (vm.api, 
                                               vm.stack.Storage [start], 
                                               vm.stack.Storage [start + 1],
                                               vm.stack.Storage [start + 2]);
            vm.stack.PopMany (2);
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleNeg (Vm vm)
        {
            vm.stack.SetTop (Prim0.UnaryMinus (vm.api, vm.stack.Top ()));
            vm.programCounter++;
        }

        static void HandleNot (Vm vm)
        {
            vm.stack.SetTop (Prim0.LogicalNot (vm.api, vm.stack.Top ()));
            vm.programCounter++;
        }

        static void HandleKeys (Vm vm)
        {
            vm.stack.SetTop (Prim0.Keys (vm.api, vm.stack.Top ()));
            vm.programCounter++;
        }

        static void HandleMul (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.Multiply (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleHasKey (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.HasKey (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleShl (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.ShiftLeft (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleShr (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.ShiftRight (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandlePow (Vm vm)
        {
            var start = vm.stack.Count - 2;
            var result = Prim0.Pow (vm.api, vm.stack.Storage [start], vm.stack.Storage [start + 1]);
            vm.stack.Pop ();
            vm.stack.SetTop (result);
            vm.programCounter++;
        }

        static void HandleFloor (Vm vm)
        {
            vm.stack.SetTop (Prim0.Floor (vm.api, vm.stack.Top ()));
            vm.programCounter++;
        }

        static void HandleJump (Vm vm)
        {           
            vm.programCounter = (int)vm.CurrentInstruction ().Arguments;
        }

        static void HandleConst (Vm vm)
        {
            vm.stack.Push ((ShovelValue)vm.CurrentInstruction ().Arguments);
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
            if (instruction.Cache == null) {
                var primName = (string)instruction.Arguments;
                if (!Vm.Prim0Hash.ContainsKey (primName)) {
                    vm.RaiseShovelError (String.Format (
                        "Cannot take address of primitive '{0}' (implemented as instruction).",
                        primName)
                    );
                }
                instruction.Cache = ShovelValue.Make (Vm.Prim0Hash [primName]);
            }
            vm.stack.Push ((ShovelValue)instruction.Cache);
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
            if (instruction.Cache == null) {
                var udpName = (string)instruction.Arguments;
                instruction.Cache = GetUdpByName (vm, udpName);
            }
            vm.stack.Push ((ShovelValue)instruction.Cache);
            vm.IncrementTicks (1);
            vm.programCounter++;
        }

        static void HandleCall (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var numArgs = (int)instruction.Arguments;
            Vm.HandleCallImpl (vm, numArgs, true);
        }

        static void HandleCallImpl (Vm vm, int numArgs, bool saveReturnAddress)
        {
            var maybeCallable = vm.stack.PopTop ();
            if (maybeCallable.Kind != ShovelValue.Kinds.Callable) {
                vm.RaiseShovelError (String.Format (
                    "Object [{0}] is not callable.", Prim0.ShovelStringRepresentation (vm.api, maybeCallable))
                );
            }
            var callable = maybeCallable.CallableValue;
            if (callable.ProgramCounter.HasValue) {
                CallFunction (callable, vm, numArgs, saveReturnAddress);
                if (saveReturnAddress) {
                    vm.IncrementCells (1);
                }
            } else {
                CallPrimitive (callable, vm, numArgs, saveReturnAddress);
            }
        }

        static void CallFunction (Callable callable, Vm vm, int numArgs, bool saveReturnAddress)
        {
            if (saveReturnAddress) {
                vm.stack.Push (ShovelValue.Make (new ReturnAddress () {
                    ProgramCounter = vm.programCounter + 1,
                    Environment = vm.currentEnvironment
                }
                )
                );
                vm.currentEnvironment.IncreaseUsesLocally ();
            }
            if (callable.Arity != null && callable.Arity.Value != numArgs) {
                ArityError (vm, callable.Arity.Value, numArgs);
            }
            vm.currentEnvironment = callable.Environment;
            vm.programCounter = callable.ProgramCounter.Value;
        }

        static void CallPrimitive (Callable callable, Vm vm, int numArgs, bool saveReturnAddress)
        {
            if (callable.HostCallable == null) {
                if (callable.UdpName != null) {
                    callable.HostCallable = GetUdpByName (vm, callable.UdpName).HostCallable;
                } else if (callable.Prim0Name == null) {
                    callable.HostCallable = Vm.Prim0Hash [callable.Prim0Name].HostCallable;
                } else {
                    Utils.Panic ();
                }
            }
            if (callable.Arity != null && callable.Arity.Value != numArgs) {
                ArityError (vm, callable.Arity.Value, numArgs);
            }
            ShovelValue[] array;
            int start;
            vm.stack.GetTopRange (numArgs, out array, out start);
            vm.stack.PopMany (numArgs);
            var result = callable.HostCallable (vm.api, array, start, numArgs);
            if (saveReturnAddress) {
                vm.programCounter ++;
            } else {
                var maybeRa = vm.stack.PopTop ();
                if (maybeRa.Kind == ShovelValue.Kinds.ReturnAddress) {
                    vm.ApplyReturnAddress (maybeRa.ReturnAddressValue);
                } else {
                    Utils.Panic ();
                }
            }
            vm.stack.Push (result);
            vm.IncrementCells (1);
        }

        static void ArityError (Vm vm, int expectedArity, int actualArity)
        {
            vm.RaiseShovelError (String.Format (
                "Function of {0} arguments called with {1} arguments.",
                expectedArity, actualArity)
            );
        }

        static void HandleCallj (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var numArgs = (int)instruction.Arguments;
            Vm.HandleCallImpl (vm, numArgs, false);
        }

        void CheckBool ()
        {
            if (this.stack.Top ().Kind != ShovelValue.Kinds.Bool) {
                RaiseShovelError ("Argument must be a boolean.");
            }
        }

        static void HandleFjump (Vm vm)
        {
            var args = (int)vm.CurrentInstruction ().Arguments;
//            vm.CheckBool ();
            if (!vm.stack.PopTop ().BoolValue) {
                vm.programCounter = args;
            } else {
                vm.programCounter ++;
            }
        }

        static void HandleTjump (Vm vm)
        {
            var args = (int)vm.CurrentInstruction ().Arguments;
//            vm.CheckBool ();
            if (vm.stack.PopTop ().BoolValue) {
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
            VmEnvironment env, int frameNumber, int varIndex, ShovelValue value)
        {
            FindFrame (env, frameNumber).Values [varIndex] = value;
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
            vm.stack.Push (GetFromEnvironment (vm.currentEnvironment, args [0], args [1]));
            vm.IncrementCells (1);
            vm.programCounter++;
        }

        static ShovelValue GetFromEnvironment (VmEnvironment env, int frameNumber, int varIndex)
        {
            return FindFrame (env, frameNumber).Values [varIndex];
        }

        static void HandleFn (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var args = (int[])instruction.Arguments;
            var callable = new Callable () {
                ProgramCounter = args[0],
                Arity = args[1],
                Environment = vm.currentEnvironment
            };
            if (vm.currentEnvironment != null) {
                vm.currentEnvironment.IncreaseUses ();
            }
            vm.stack.Push (ShovelValue.Make (callable));
            vm.IncrementCells (5);
            vm.programCounter++;
        }

        static VmEnvironment FreshFrame (Vm vm, Instruction instruction)
        {
            var args = (string[])instruction.Arguments;
            var frame = new VmEnvFrame () {
                VarNames = args,
                Values = new ShovelValue[args.Length],
                IntroducedAtProgramCounter = vm.programCounter
            };
            var result = new VmEnvironment () {
                Frame = frame,
            };
            instruction.Cache = result;
            return result;
        }

        static void HandleNewFrame (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            if (instruction.Cache == null) {
                FreshFrame (vm, instruction);
            }
            var newEnv = (VmEnvironment)instruction.Cache;
            if (newEnv.IsUsed) {
                newEnv = FreshFrame (vm, instruction);
            }
            var values = newEnv.Frame.Values;
            for (var i = 0; i < values.Length; i++) {
                values [i].Kind = ShovelValue.Kinds.Null;
            }
            newEnv.Next = vm.currentEnvironment;
            vm.currentEnvironment = newEnv;
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
                ShovelValue? returnAddress = null;
                if (vm.stack.Top ().Kind == ShovelValue.Kinds.ReturnAddress) {
                    returnAddress = vm.stack.PopTop ();
                }
                var values = vm.currentEnvironment.Frame.Values;
                Array.Copy (
                    vm.stack.Storage, vm.stack.Count - argCount, 
                    values, 0, argCount);
                vm.stack.PopMany(argCount);
                if (returnAddress.HasValue) {
                    vm.stack.Push (returnAddress.Value);
                }
            }
            vm.programCounter++;
        }

        static void HandleArgs1(Vm vm) {
            if (vm.stack.TopIsReturnAddress()) {
                vm.currentEnvironment.Frame.Values[0] = vm.stack.UnderTopOne();
                vm.stack.UnderPopOneAndCopyTop();
            } else {
                vm.currentEnvironment.Frame.Values[0] = vm.stack.PopTop ();
            }
            vm.programCounter++;
        }

        static void HandleArgs2(Vm vm) {
            if (vm.stack.TopIsReturnAddress()) {
                vm.currentEnvironment.Frame.Values[0] = vm.stack.UnderTop(2);
                vm.currentEnvironment.Frame.Values[1] = vm.stack.UnderTop(1);
                vm.stack.UnderPopAndCopyTop(2);
            } else {
                vm.currentEnvironment.Frame.Values[1] = vm.stack.PopTop ();
                vm.currentEnvironment.Frame.Values[0] = vm.stack.PopTop ();
            }
            vm.programCounter++;
        }

        static void HandleArgs3(Vm vm) {
            if (vm.stack.TopIsReturnAddress()) {
                vm.currentEnvironment.Frame.Values[0] = vm.stack.UnderTop(3);
                vm.currentEnvironment.Frame.Values[1] = vm.stack.UnderTop(2);
                vm.currentEnvironment.Frame.Values[2] = vm.stack.UnderTop(1);
                vm.stack.UnderPopAndCopyTop(3);
            } else {
                vm.currentEnvironment.Frame.Values[2] = vm.stack.PopTop ();
                vm.currentEnvironment.Frame.Values[1] = vm.stack.PopTop ();
                vm.currentEnvironment.Frame.Values[0] = vm.stack.PopTop ();
            }
            vm.programCounter++;
        }

        static void HandleReturn (Vm vm)
        {
            // SLOW: maybe there's a faster way to manipulate the stack here?
            var result = vm.stack.PopTop ();
            var maybeRa = vm.stack.PopTop ();
            if (maybeRa.Kind == ShovelValue.Kinds.ReturnAddress) {
                vm.ApplyReturnAddress (maybeRa.ReturnAddressValue);
            } else {
                Utils.Panic ();
            }
            vm.stack.Push (result);
        }

        void ApplyReturnAddress (ReturnAddress returnAddress)
        {
            this.programCounter = returnAddress.ProgramCounter;
            this.currentEnvironment = returnAddress.Environment;
            this.currentEnvironment.DecreaseUsesLocally ();
        }

        static void HandleBlock (Vm vm)
        {
            var instruction = vm.CurrentInstruction ();
            var blockEnd = (int)instruction.Arguments;
            var name = vm.stack.PopTop ();
            if (name.Kind != ShovelValue.Kinds.String) {
                vm.RaiseShovelError ("The name of a block must be a string.");
            }
            vm.stack.Push (ShovelValue.Make (new NamedBlock () {
                Name = name.StringValue,
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
            if (namedBlock.Kind != ShovelValue.Kinds.NamedBlock) {
                vm.RaiseShovelError ("Invalid context for POP_BLOCK.");
            }
            vm.stack.Push (returnValue);
            vm.programCounter++;
        }

        static void HandleBlockReturn (Vm vm)
        {
            var returnValue = vm.stack.PopTop ();
            var name = vm.stack.PopTop ();
            if (name.Kind != ShovelValue.Kinds.String) {
                vm.RaiseShovelError ("The name of a block must be a string.");
            }
            var namedBlockIndex = vm.FindNamedBlock (name.StringValue);
            if (vm.stack.Count > namedBlockIndex + 1) {
                for (var i = namedBlockIndex + 1; i < vm.stack.Count; i++) {
                    if (vm.stack.Storage [i].Kind == ShovelValue.Kinds.ReturnAddress) {
                        vm.stack.Storage [i].ReturnAddressValue.Environment.DecreaseUsesLocally ();
                    }
                }
                vm.stack.RemoveRange (namedBlockIndex + 1, vm.stack.Count - namedBlockIndex - 1);
            }
            var namedBlock = vm.stack.Top ().NamedBlockValue;
            vm.stack.Push (returnValue);
            vm.programCounter = namedBlock.BlockEnd;
            vm.currentEnvironment = namedBlock.Environment;
        }

        int FindNamedBlock (string blockName)
        {
            for (var i = this.stack.Count - 1; i >= 0; i--) {
                if (this.stack.Storage [i].Kind == ShovelValue.Kinds.NamedBlock
                    && this.stack.Storage [i].NamedBlockValue.Name == blockName) {
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
            var result = new Dictionary<ShovelValue, ShovelValue> ();
            result.Add (ShovelValue.Make ("stack"), ShovelValue.Make (stackTrace));
            result.Add (ShovelValue.Make ("environment"), ShovelValue.Make (currentEnvironment));
            vm.IncrementCells (6 + stackTrace.Length + currentEnvironment.Length);
            vm.stack.Push (ShovelValue.Make (result));
            vm.programCounter ++;
            return;
        }

        static void HandleNop (Vm vm)
        {
            vm.programCounter++;
        }

        bool IsLive ()
        {
            return !this.ExecutionComplete () && !this.shouldTakeANap;
        }

        bool ExecutionComplete ()
        {
            return this.programCounter == this.bytecode.Length;
        }

        void CheckCellsQuota ()
        {
            // FIXME: Check cells quota.
        }

        void CheckTicksQuota ()
        {
            // FIXME: Check ticks quota.
        }

        void CheckVmWithoutError ()
        {
            if (this.userDefinedPrimitiveError != null) {
                throw this.userDefinedPrimitiveError;
            }
            if (this.programmingError != null) {
                throw this.programmingError;
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
            throw new ShovelException (){
                    Message = message,
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
                    this.PrintLineFor (sb, pc, instruction.StartPos, instruction.EndPos);
                }
                sb.AppendLine ("Frame variables are:");
                for (var i = 0; i < env.Frame.VarNames.Length; i++) {
                    sb.AppendLine (String.Format (
                        "{0} = {1}",
                        env.Frame.VarNames [i],
                        Prim0.ShovelStringRepresentation (this.api, env.Frame.Values [i]).StringValue)
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
                    var startPos = Position.CalculatePosition (sourceFile, characterStartPos.Value);
                    var endPos = Position.CalculatePosition (sourceFile, characterEndPos.Value);
                    var lines = Utils.ExtractRelevantSource (sourceFile.Content.Split ('\n'), startPos, endPos);
                    foreach (var line in lines) {
                        sb.AppendLine (line);
                    }
                    foundLocation = true;
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
                if (this.stack.Storage [i].Kind == ShovelValue.Kinds.ReturnAddress) {
                    var ra = this.stack.Storage [i].ReturnAddressValue;
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
    }
}

