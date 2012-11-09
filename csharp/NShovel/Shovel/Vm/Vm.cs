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
		Environment currentEnvironment = null;
		List<object> stack = new List<object> ();
		Dictionary<string, UserDefinedPrimitive> userPrimitives = new Dictionary<string, UserDefinedPrimitive> ();
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

		public static void RunVm (
			Instruction[] bytecode, 
			List<SourceFile> sources = null,
			List<UserDefinedPrimitive> userPrimitives = null,
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
				vm.userPrimitives = new Dictionary<string, UserDefinedPrimitive> ();
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
					vm.userPrimitives [udp.Name] = udp;
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
		}

		bool StepVm ()
		{
			this.CheckVmWithoutError ();
			this.CheckTicksQuota ();
			this.CheckCellsQuota ();
			if (this.IsLive ()) {
				Vm.handlers [this.bytecode [this.programCounter].NumericOpcode] (this);
				throw new NotImplementedException ();
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

		static void HandleJump (Vm vm)
		{
			throw new NotImplementedException ();
		}

		static void HandleConst (Vm vm)
		{
			throw new NotImplementedException ();
		}

		static void HandlePrim0 (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandlePrim (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleCall (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleCallj (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleFjump (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleLset (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandlePop (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleLget (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleFn (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleNewFrame (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleDropFrame (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleArgs (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleReturn (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleBlock (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandlePopBlock (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleBlockReturn (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleContext (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleTjump (Vm obj)
		{
			throw new NotImplementedException ();
		}

		static void HandleNop (Vm obj)
		{
			throw new NotImplementedException ();
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
			throw new NotImplementedException ();
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

