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

namespace Shovel.Vm
{
	public class Vm
	{
		Instruction[] bytecode = null;
		int programCounter = 0;
		VmEnvironment currentEnvironment = null;
		List<object> stack = new List<object> ();
		Dictionary<string, Callable> userPrimitives = new Dictionary<string, Callable> ();
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
		// FIXME: Add 'runs' later, I'm just implementing a basic version now.
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
				vm.stack = new List<object> ();
				vm.sources = sources;
				vm.userPrimitives = new Dictionary<string, Callable> ();
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

		public object CheckStackTop()
		{
			if (this.stack.Count != 1) {
				Utils.Panic ();
			}
			return this.Top();
		}

		bool StepVm ()
		{
			this.CheckVmWithoutError ();
			this.CheckTicksQuota ();
			this.CheckCellsQuota ();
			if (this.IsLive ()) {
				var instruction = this.CurrentInstruction();
//				Console.WriteLine("*****");
//				Console.WriteLine (instruction.Opcode);
//				for (var i = 0; i < this.stack.Count; i++) {
//					Console.WriteLine(this.stack[i]);
//				}
				Vm.handlers [instruction.NumericOpcode] (this);
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
			Vm.HandleNop                       // 20
		};

		Instruction CurrentInstruction ()
		{
			return this.bytecode [this.programCounter];
		}

		void Push (object obj)
		{
			this.stack.Add (obj);
		}

		object Pop ()
		{
			var result = this.Top ();
			this.stack.RemoveAt (this.stack.Count - 1);
			return result;
		}

		object Top (int n = 1)
		{
			return this.stack [this.stack.Count - n];
		}

		static void HandleJump (Vm vm)
		{			
			vm.programCounter = (int)vm.CurrentInstruction ().Arguments;
		}

		static void HandleConst (Vm vm)
		{
			vm.Push (vm.CurrentInstruction ().Arguments);
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
				instruction.Cache = Vm.Prim0Hash [(string)instruction.Arguments];
			}
			vm.Push (instruction.Cache);
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
				instruction.Cache = GetUdpByName(vm, udpName);
			}
			vm.Push (instruction.Cache);
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
			var maybeCallable = vm.Pop ();
			if (!(maybeCallable is Callable)) {
				vm.RaiseShovelError (String.Format (
					"Object [{0}] is not callable.", Prim0.ShovelStringRepresentation (vm.api, maybeCallable))
				);
			}
			var callable = (Callable)maybeCallable;
			if (callable.UdpName != null || callable.Prim0Name != null) {
				CallPrimitive (callable, vm, numArgs, saveReturnAddress);
			} else {
				CallFunction (callable, vm, numArgs, saveReturnAddress);
				if (saveReturnAddress) {
					vm.IncrementCells (1);
				}
			}
		}

		static void CallFunction (Callable callable, Vm vm, int numArgs, bool saveReturnAddress)
		{
			if (saveReturnAddress) {
				vm.Push (new ReturnAddress() {
					ProgramCounter = vm.programCounter + 1,
					Environment = vm.currentEnvironment
				});
			}
			if (callable.Arity != null && callable.Arity.Value != numArgs) {
				ArityError(vm, callable.Arity.Value, numArgs);
			}
			vm.currentEnvironment = callable.Environment;
			vm.programCounter = callable.ProgramCounter.Value;
		}

		static void CallPrimitive (Callable callable, Vm vm, int numArgs, bool saveReturnAddress)
		{
			if (callable.HostCallable == null) {
				if (callable.UdpName != null) {
					callable.HostCallable = GetUdpByName(vm, callable.UdpName).HostCallable;
				} else if (callable.Prim0Name == null) {
					callable.HostCallable = Vm.Prim0Hash [callable.Prim0Name].HostCallable;
				} else {
					Utils.Panic ();
				}
			}
			if (callable.Arity != null && callable.Arity.Value != numArgs) {
				ArityError(vm, callable.Arity.Value, numArgs);
			}
			var args = new object[numArgs];
			for (var i = args.Length - 1; i >= 0; i--) {
				args[i] = vm.Pop ();
			}
			if (saveReturnAddress) {
				vm.programCounter ++;
			} else {
				vm.ApplyReturnAddress(vm.Pop () as ReturnAddress);
			}
			vm.Push (callable.HostCallable(vm.api, args));
			vm.IncrementCells(1);
		}

		static void ArityError (Vm vm, int expectedArity, int actualArity)
		{
			vm.RaiseShovelError(String.Format (
				"Function of {0} arguments called with {1} arguments.",
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
			if (!(this.Top () is bool)) {
				RaiseShovelError ("Argument must be a boolean.");
			}
		}

		static void HandleFjump (Vm vm)
		{
			var args = vm.CurrentInstruction ().Arguments;
			vm.CheckBool ();
			// SLOW: the first argument could be optimized to !(bool)vm.Pop().
			// Not done because I'm not sure of all implications yet (and it doesn't
			// show up as one of the top time consumers in profiler reports).
			vm.JumpIf ((bool)Prim0.LogicalNot (vm.api, vm.Pop ()), (int)args);
		}

		static void HandleTjump (Vm vm)
		{
			var args = vm.CurrentInstruction ().Arguments;
			vm.CheckBool ();
			vm.JumpIf ((bool)vm.Pop (), (int)args);
		}

		void JumpIf (bool shouldJump, int jumpDestination)
		{
			if (shouldJump) {
				this.programCounter = jumpDestination;
			} else {
				this.programCounter++;
			}
		}

		static void HandleLset (Vm vm)
		{
			var instruction = vm.CurrentInstruction ();
			var args = (int[])instruction.Arguments;
			SetInEnvironment (vm.currentEnvironment, args [0], args [1], vm.Top ());
			vm.programCounter++;
		}

		static void SetInEnvironment (
			VmEnvironment env, int frameNumber, int varIndex, object value)
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
			vm.Pop ();
			vm.programCounter++;
		}

		static void HandleLget (Vm vm)
		{
			var instruction = vm.CurrentInstruction ();
			var args = (int[])instruction.Arguments;
			vm.Push (GetFromEnvironment (vm.currentEnvironment, args [0], args [1]));
			vm.IncrementCells (1);
			vm.programCounter++;
		}

		static object GetFromEnvironment (VmEnvironment env, int frameNumber, int varIndex)
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
			vm.Push (callable);
			vm.IncrementCells (5);
			vm.programCounter++;
		}

		static void HandleNewFrame (Vm vm)
		{
			var instruction = vm.CurrentInstruction ();
			var args = (string[])instruction.Arguments;
			var frame = new VmEnvFrame () {
				VarNames = args,
				Values = new object[args.Length]
			};
			var newEnv = new VmEnvironment () {
				Frame = frame,
				Next = vm.currentEnvironment
			};
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
				ReturnAddress returnAddress = null;
				if (vm.Top () is ReturnAddress) {
					returnAddress = vm.Pop () as ReturnAddress;
				}
				for (var i = argCount - 1; i >= 0; i--) {
					vm.currentEnvironment.Frame.Values [i] = vm.Pop ();
				}
				if (returnAddress != null) {
					vm.Push (returnAddress);
				}
			}
			vm.programCounter++;
		}

		static void HandleReturn (Vm vm)
		{
			// SLOW: maybe there's a faster way to manipulate the stack here?
			var result = vm.Pop ();
			var returnAddress = vm.Pop () as ReturnAddress;
			vm.ApplyReturnAddress (returnAddress);
			vm.Push (result);
		}

		void ApplyReturnAddress (ReturnAddress returnAddress)
		{
			if (returnAddress == null) {
				Utils.Panic ();
			}
			this.programCounter = returnAddress.ProgramCounter;
			this.currentEnvironment = returnAddress.Environment;
		}

		static void HandleBlock (Vm vm)
		{
			var instruction = vm.CurrentInstruction ();
			var blockEnd = (int)instruction.Arguments;
			var name = vm.Pop ();
			if (!(name is String)) {
				vm.RaiseShovelError ("The name of a block must be a string.");
			}
			vm.Push (new NamedBlock () {
				Name = (string)name,
				BlockEnd = blockEnd,
				Environment = vm.currentEnvironment
			}
			);
			vm.IncrementCells (3);
			vm.programCounter++;
		}

		static void HandlePopBlock (Vm vm)
		{
			var returnValue = vm.Pop ();
			var namedBlock = vm.Pop ();
			if (!(namedBlock is NamedBlock)) {
				vm.RaiseShovelError ("Invalid context for POP_BLOCK.");
			}
			vm.Push (returnValue);
			vm.programCounter++;
		}

		static void HandleBlockReturn (Vm vm)
		{
			var returnValue = vm.Pop ();
			var name = vm.Pop ();
			if (!(name is String)) {
				vm.RaiseShovelError ("The name of a block must be a string.");
			}
			var namedBlockIndex = vm.FindNamedBlock ((string)name);
			if (vm.stack.Count > namedBlockIndex + 1) {
				vm.stack.RemoveRange (namedBlockIndex + 1, vm.stack.Count - namedBlockIndex - 1);
			}
			var namedBlock = (NamedBlock)vm.Top ();
			vm.Push (returnValue);
			vm.programCounter = namedBlock.BlockEnd;
			vm.currentEnvironment = namedBlock.Environment;
		}

		int FindNamedBlock (string blockName)
		{
			for (var i = this.stack.Count - 1; i >= 0; i--) {
				if (this.stack [i] is NamedBlock && ((NamedBlock)this.stack [i]).Name == blockName) {
					return i;
				}
			}
			this.RaiseShovelError (
				String.Format ("Cannot find block '{0}'.", blockName));
			return -1;
		}

		static void HandleContext (Vm obj)
		{
			throw new NotImplementedException ();
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
			// FIXME: need to implement the real thing.
			throw new InvalidOperationException(message);
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
				throw new ShovelCellQuotaExceeded ();
			}
		}
	}
}

