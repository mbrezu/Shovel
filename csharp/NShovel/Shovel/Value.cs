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
using Shovel.Exceptions;

namespace Shovel
{
    public class Either<T>
    {
        private T value;
        public T Value {
            get {
                if (!HasValue) {
                    throw Error;
                }
                return value;
            }
            private set {
                this.value = value;
            }
        }
        public ShovelException Error { get; private set; }
        public bool HasValue { get; private set; }

        public Either(T value)
        {
            this.Value = value;
            HasValue = true;
        }

        public Either(ShovelException error)
        {
            this.Error = error;
        }

        public override string ToString()
        {
            return this.Value.ToString();
        }
    }

    public class ArrayInstance : List<Value>
    {
        public Value IndirectGet { get; set; }
        public Value IndirectSet { get; set; }

        public ArrayInstance() : base()
        {            
        }

        public ArrayInstance(List<Value> init) : base(init)
        {
        }

        public ArrayInstance GetRange2(int start, int length)
        {
            var result = new ArrayInstance();
            result.AddRange(GetRange(start, length));
            return result;
        }
    }

    public class HashInstance : Dictionary<Value, Value>
    {
        public Value IndirectGet { get; set; }
        public Value IndirectSet { get; set; }

        public HashInstance() : base()
        {
        }

        public HashInstance(Dictionary<Value, Value> init) : base(init)
        {
        }
    }

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
        };

        // Mono and .NET 32 bit seem to be OK with the offset below set at 12.
        // Mono on 64 bit wants it to be set to 20. No idea why (it looks like it should be 16).
        // Need to check what .NET 64 bit requires.
        // The lower the better, performance wise.
        [FieldOffset(20)]
        public Kinds Kind;
        
        [FieldOffset(0)]
        internal long integerValue;

        public Either<long> Integer
        {
            get
            {
                if (Kind == Kinds.Integer)
                {
                    return new Either<long>(this.integerValue);
                }
                else
                {
                    return new Either<long>(new ShovelException("Value is not an integer.", null));
                }
            }
        }

        [FieldOffset(0)]
        internal double doubleValue;

        public Either<double> Double
        {
            get
            {
                if (Kind == Kinds.Double)
                {
                    return new Either<double>(this.doubleValue);
                }
                else
                {
                    return new Either<double>(new ShovelException("Value is not a double.", null));
                }
            }
        }

        [FieldOffset(0)]
        internal bool boolValue;

        public Either<bool> Bool
        {
            get
            {
                if (Kind == Kinds.Bool)
                {
                    return new Either<bool>(this.boolValue);
                }
                else
                {
                    return new Either<bool>(new ShovelException("Value is not a bool.", null));
                }
            }
        }

        [FieldOffset(8)]
        internal string stringValue;

        public Either<string> String
        {
            get
            {
                if (Kind == Kinds.String)
                {
                    return new Either<string>(this.stringValue);
                }
                else
                {
                    return new Either<string>(new ShovelException("Value is not a string.", null));
                }
            }
        }

        [FieldOffset(8)]
        internal ArrayInstance arrayValue;

        public Either<List<Value>> Array
        {
            get
            {
                if (Kind == Kinds.Array)
                {
                    return new Either<List<Value>>(this.arrayValue);
                }
                else
                {
                    return new Either<List<Value>>(new ShovelException("Value is not an array.", null));
                }
            }
        }

        [FieldOffset(8)]
        internal HashInstance hashValue;

        public Either<Dictionary<Value, Value>> Hash
        {
            get
            {
                if (Kind == Kinds.Hash)
                {
                    return new Either<Dictionary<Value, Value>>(this.hashValue);
                }
                else
                {
                    return new Either<Dictionary<Value, Value>>(new ShovelException("Value is not a hash.", null));
                }
            }
        }

        [FieldOffset(8)]
        internal Callable callableValue;

        public Either<Callable> CallableValue {
            get {
                if ( Kind == Kinds.Callable ) {
                    return new Either<Callable>( this.callableValue );
                }
                else {
                    return new Either<Callable>( new ShovelException( "Value is not a callable.", null ) );
                }
            }
        }

        [FieldOffset(8)]
        internal StructInstance structInstanceValue;

        public Either<StructInstance> StructInstanceValue {
            get {
                if ( Kind == Kinds.StructInstance ) {
                    return new Either<StructInstance>( this.structInstanceValue );
                }
                else {
                    return new Either<StructInstance>( new ShovelException( "Value is not a struct instance.", null ) );
                }
            }
        }

        [FieldOffset( 8 )]
        internal NamedBlock namedBlockValue;

        public Either<NamedBlock> NamedBlockValue {
            get {
                if ( Kind == Kinds.NamedBlock ) {
                    return new Either<NamedBlock>( this.namedBlockValue );
                }
                else {
                    return new Either<NamedBlock>( new ShovelException( "Value is not a named block.", null ) );
                }
            }
        }

        [FieldOffset(8)]
        internal ReturnAddress returnAddressValue;

        public Either<ReturnAddress> ReturnAddressValue {
            get {
                if ( Kind == Kinds.ReturnAddress ) {
                    return new Either<ReturnAddress>( this.returnAddressValue );
                }
                else {
                    return new Either<ReturnAddress>( new ShovelException( "Value is not a return address.", null ) );
                }
            }
        }

        [FieldOffset(8)]
        internal Struct StructValue;

        [FieldOffset(8)]
        internal StructInstance structInstanceValue;

        public Either<StructInstance> StructInstance
        {
            get
            {
                if (Kind == Kinds.StructInstance)
                {
                    return new Either<StructInstance>(this.structInstanceValue);
                }
                else
                {
                    return new Either<StructInstance>(new ShovelException("Value is not a struct instance.", null));
                }
            }
        }

        static Value value;

        public static Value Make ()
        {
            return value;
        }

        internal static Value Make(Struct structFields)
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
            result.structInstanceValue = structInstance;
            return result;
        }

        public static Value Make (bool b)
        {
            Value result = value;
            result.Kind = Kinds.Bool;
            result.boolValue = b;
            return result;
        }

        public static Value MakeInt (long l)
        {
            Value result = value;
            result.Kind = Kinds.Integer;
            result.integerValue = l;
            return result;
        }

        public static Value Make (string s)
        {
            if (s == null) {
                return value;
            }
            Value result = value;
            result.Kind = Kinds.String;
            result.stringValue = s;
            return result;
        }

        public static Value MakeFloat (double d)
        {
            Value result = value;
            result.Kind = Kinds.Double;
            result.doubleValue = d;
            return result;
        }

        public static Value Make (ArrayInstance a)
        {
            if (a == null) {
                return value;
            }
            Value result = value;
            result.Kind = Kinds.Array;
            result.arrayValue = a;
            return result;
        }

        public static Value Make (HashInstance d)
        {
            if (d == null) {
                return value;
            }
            Value result = value;
            result.Kind = Kinds.Hash;
            result.hashValue = d;
            return result;
        }

        internal static Value Make (Callable c)
        {
            Value result = value;
            result.Kind = Kinds.Callable;
            result.callableValue = c;
            return result;
        }

        internal static Value Make (ReturnAddress ra)
        {
            Value result = value;
            result.Kind = Kinds.ReturnAddress;
            result.returnAddressValue = ra;
            return result;
        }

        internal static Value Make (NamedBlock nb)
        {
            Value result = value;
            result.Kind = Kinds.NamedBlock;
            result.namedBlockValue = nb;
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
                return sv.Kind == Kinds.String && this.stringValue == sv.stringValue;
            case Kinds.Integer:
                return sv.Kind == Kinds.Integer && this.integerValue == sv.integerValue;
            case Kinds.Null:
                return sv.Kind == Kinds.Null;
            case Kinds.Double:
                return sv.Kind == Kinds.Double && this.doubleValue == sv.doubleValue;
            case Kinds.Bool:
                return sv.Kind == Kinds.Bool && this.boolValue == sv.boolValue;
            case Kinds.Array:
                return sv.Kind == Kinds.Array && this.arrayValue == sv.arrayValue;
            case Kinds.Hash:
                return sv.Kind == Kinds.Hash && this.hashValue == sv.hashValue;
            case Kinds.Callable:
                return sv.Kind == Kinds.Callable && Equals( this.callableValue, sv.callableValue );
            case Kinds.ReturnAddress:
                return sv.Kind == Kinds.ReturnAddress && Equals( this.returnAddressValue, sv.returnAddressValue );
            case Kinds.NamedBlock:
                return sv.Kind == Kinds.NamedBlock && Equals( this.namedBlockValue, sv.namedBlockValue );
            case Kinds.Struct:
                return sv.Kind == Kinds.Struct && Equals( this.StructValue, sv.StructValue );
            case Kinds.StructInstance:
                return sv.Kind == Kinds.StructInstance && Equals( this.structInstanceValue, sv.structInstanceValue );
            default:
                Utils.Panic ();
                throw new InvalidOperationException ();
            }
        }

        public override int GetHashCode ()
        {
            switch (this.Kind) {
            case Kinds.String:
                return this.stringValue.GetHashCode ();
            case Kinds.Integer:
                return this.integerValue.GetHashCode ();
            case Kinds.Null:
                return 0;
            case Kinds.Double:
                return this.doubleValue.GetHashCode ();
            case Kinds.Bool:
                return this.boolValue.GetHashCode ();
            case Kinds.Array:
                return this.arrayValue.GetHashCode ();
            case Kinds.Hash:
                return this.hashValue.GetHashCode ();
            case Kinds.Callable:
                return this.callableValue.GetHashCode ();
            case Kinds.ReturnAddress:
                return this.returnAddressValue.GetHashCode ();
            case Kinds.NamedBlock:
                return this.namedBlockValue.GetHashCode ();
            case Kinds.Struct:
                return this.StructValue.GetHashCode();
            case Kinds.StructInstance:
                return this.structInstanceValue.GetHashCode();
            default:
                Utils.Panic ();
                throw new InvalidOperationException ();
            }
        }

        public override string ToString ()
        {
            return Vm.Prim0.ShovelString (VmApi.DummyApi, this).stringValue;
        }

    }
}

