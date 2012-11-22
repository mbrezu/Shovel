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
        }
        ;

        [FieldOffset(12)]
        public Kinds Kind;

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
        public Callable
            CallableValue;
        [FieldOffset(8)]
        public ReturnAddress
            ReturnAddressValue;
        [FieldOffset(8)]
        public NamedBlock
            NamedBlockValue;

        static Value value;

        public static Value Make ()
        {
            return value;
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
            Value result = value;
            result.Kind = Kinds.Array;
            result.ArrayValue = a;
            return result;
        }

        public static Value Make (Dictionary<Value, Value> d)
        {
            Value result = value;
            result.Kind = Kinds.Hash;
            result.HashValue = d;
            return result;
        }

        public static Value Make (Callable c)
        {
            Value result = value;
            result.Kind = Kinds.Callable;
            result.CallableValue = c;
            return result;
        }

        public static Value Make (ReturnAddress ra)
        {
            Value result = value;
            result.Kind = Kinds.ReturnAddress;
            result.ReturnAddressValue = ra;
            return result;
        }

        public static Value Make (NamedBlock nb)
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

