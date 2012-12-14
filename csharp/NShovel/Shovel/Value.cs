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
using System.Runtime.InteropServices;
using System.Collections.Generic;
using Shovel.Vm.Types;

namespace Shovel
{
    [StructLayout(LayoutKind.Explicit)]
    public struct Value
    {
        public enum Kinds
        {
            Null,
            Integer,
            String,
            Double,
            Bool,
            Array,
            Hash,
            Callable,
            ReturnAddress,
            NamedBlock,
            Struct,
            StructInstance,
        }
        ;

        // Mono and .NET 32 bit seem to be OK with the offset below set at 12.
        // Mono on 64 bit wants it to be set to 20. No idea why (it looks like it should be 16).
        // Need to check what .NET 64 bit requires.
        // The lower the better, performance wise.
        [FieldOffset(20)]
        public Kinds
            Kind;
        [FieldOffset(0)]
        public long
            IntegerValue;
        [FieldOffset(0)]
        public double
            DoubleValue;
        [FieldOffset(0)]
        public bool
            BoolValue;
        [FieldOffset(8)]
        public string
            StringValue;
        [FieldOffset(8)]
        public List<Value>
            ArrayValue;
        [FieldOffset(8)]
        public Dictionary<Value, Value>
            HashValue;
        [FieldOffset(8)]
        internal Callable
            CallableValue;
        [FieldOffset(8)]
        internal ReturnAddress
            ReturnAddressValue;
        [FieldOffset(8)]
        internal NamedBlock
            NamedBlockValue;
        [FieldOffset(8)]
        internal Struct
            StructValue;
        [FieldOffset(8)]
        public StructInstance
            StructInstanceValue;

        static Value value;

        public static Value Make ()
        {
            return value;
        }

        public static Value Make(Struct structFields)
        {
            Value result = value;
            result.Kind = Kinds.Struct;
            result.StructValue = structFields;
            return result;
        }

        public static Value Make(StructInstance structInstance) 
        {
            Value result = value;
            result.Kind = Kinds.StructInstance;
            result.StructInstanceValue = structInstance;
            return result;
        }

        public static Value Make (bool b)
        {
            Value result = value;
            result.Kind = Kinds.Bool;
            result.BoolValue = b;
            return result;
        }

        public static Value MakeInt (long l)
        {
            Value result = value;
            result.Kind = Kinds.Integer;
            result.IntegerValue = l;
            return result;
        }

        public static Value Make (string s)
        {
            if (s == null) {
                return value;
            }
            Value result = value;
            result.Kind = Kinds.String;
            result.StringValue = s;
            return result;
        }

        public static Value MakeFloat (double d)
        {
            Value result = value;
            result.Kind = Kinds.Double;
            result.DoubleValue = d;
            return result;
        }

        public static Value Make (List<Value> a)
        {
            if (a == null) {
                return value;
            }
            Value result = value;
            result.Kind = Kinds.Array;
            result.ArrayValue = a;
            return result;
        }

        public static Value Make (Dictionary<Value, Value> d)
        {
            if (d == null) {
                return value;
            }
            Value result = value;
            result.Kind = Kinds.Hash;
            result.HashValue = d;
            return result;
        }

        internal static Value Make (Callable c)
        {
            Value result = value;
            result.Kind = Kinds.Callable;
            result.CallableValue = c;
            return result;
        }

        internal static Value Make (ReturnAddress ra)
        {
            Value result = value;
            result.Kind = Kinds.ReturnAddress;
            result.ReturnAddressValue = ra;
            return result;
        }

        internal static Value Make (NamedBlock nb)
        {
            Value result = value;
            result.Kind = Kinds.NamedBlock;
            result.NamedBlockValue = nb;
            return result;
        }

        public override bool Equals (object obj)
        {
            if (!(obj is Value)) {
                return false;
            }
            var sv = (Value)obj;
            switch (this.Kind) {
            case Kinds.String:
                return sv.Kind == Kinds.String && this.StringValue == sv.StringValue;
            case Kinds.Integer:
                return sv.Kind == Kinds.Integer && this.IntegerValue == sv.IntegerValue;
            case Kinds.Null:
                return sv.Kind == Kinds.Null;
            case Kinds.Double:
                return sv.Kind == Kinds.Double && this.DoubleValue == sv.DoubleValue;
            case Kinds.Bool:
                return sv.Kind == Kinds.Bool && this.BoolValue == sv.BoolValue;
            case Kinds.Array:
                return sv.Kind == Kinds.Array && this.ArrayValue == sv.ArrayValue;
            case Kinds.Hash:
                return sv.Kind == Kinds.Hash && this.HashValue == sv.HashValue;
            case Kinds.Callable:
                return sv.Kind == Kinds.Callable && this.CallableValue == sv.CallableValue;
            case Kinds.ReturnAddress:
                return sv.Kind == Kinds.ReturnAddress && this.ReturnAddressValue == sv.ReturnAddressValue;
            case Kinds.NamedBlock:
                return sv.Kind == Kinds.NamedBlock && this.NamedBlockValue == sv.NamedBlockValue;
            case Kinds.Struct:
                return sv.Kind == Kinds.Struct && this.StructValue == sv.StructValue;
            case Kinds.StructInstance:
                return sv.Kind == Kinds.StructInstance && this.StructInstanceValue == sv.StructInstanceValue;
            default:
                Utils.Panic ();
                throw new InvalidOperationException ();
            }
        }

        public override int GetHashCode ()
        {
            switch (this.Kind) {
            case Kinds.String:
                return this.StringValue.GetHashCode ();
            case Kinds.Integer:
                return this.IntegerValue.GetHashCode ();
            case Kinds.Null:
                return 0;
            case Kinds.Double:
                return this.DoubleValue.GetHashCode ();
            case Kinds.Bool:
                return this.BoolValue.GetHashCode ();
            case Kinds.Array:
                return this.ArrayValue.GetHashCode ();
            case Kinds.Hash:
                return this.HashValue.GetHashCode ();
            case Kinds.Callable:
                return this.CallableValue.GetHashCode ();
            case Kinds.ReturnAddress:
                return this.ReturnAddressValue.GetHashCode ();
            case Kinds.NamedBlock:
                return this.NamedBlockValue.GetHashCode ();
            case Kinds.Struct:
                return this.StructValue.GetHashCode();
            case Kinds.StructInstance:
                return this.StructInstanceValue.GetHashCode();
            default:
                Utils.Panic ();
                throw new InvalidOperationException ();
            }
        }

        static VmApi dummyVmApi = null;

        public override string ToString ()
        {
            if (dummyVmApi == null) {
                dummyVmApi = new VmApi (
                    raiseShovelError: str => {},
                ticksIncrementer: ticks => {},
                cellsIncrementer: cells => {},
                cellsIncrementHerald: cells => {}
                );
            }
            return Vm.Prim0.ShovelString (dummyVmApi, this).StringValue;
        }

    }
}

