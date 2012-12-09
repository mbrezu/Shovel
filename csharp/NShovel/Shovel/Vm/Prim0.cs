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
using System.Globalization;
using System.Linq;
using Shovel.Vm.Types;

namespace Shovel.Vm
{
    internal static class Prim0
    {
        static void AddPrim0 (Dictionary<string, Callable> result, Callable callable)
        {
            result [callable.Prim0Name] = callable;
        }

        public static Dictionary<string, Callable> GetPrim0Hash ()
        {
            var result = new Dictionary<string, Callable> ();

            // Hash constructor.
            AddPrim0 (result, Callable.MakePrim0 ("hash", HashConstructor, null));

            // Array constructors.
            AddPrim0 (result, Callable.MakePrim0 ("array", ArrayConstructor, null));
            AddPrim0 (result, Callable.MakePrim0 ("arrayN", Callable.MakeHostCallable (SizedArrayConstructor), 1));

            // String or array slice.
            AddPrim0 (result, Callable.MakePrim0 ("slice", Callable.MakeHostCallable (GetSlice), 3));

            // String 'upper' and 'lower' conversion functions.
            AddPrim0 (result, Callable.MakePrim0 ("upper", Callable.MakeHostCallable (StringUpper), 1));
            AddPrim0 (result, Callable.MakePrim0 ("lower", Callable.MakeHostCallable (StringLower), 1));

            // Current date/time.
            AddPrim0 (result, Callable.MakePrim0 (
                "utcSecondsSinceUnixEpoch", Callable.MakeHostCallable (UtcSecondsSinceUnixEpoch), 0)
            );

            // Date/time construction/deconstruction.
            AddPrim0 (result, Callable.MakePrim0 ("decodeTime", Callable.MakeHostCallable (DecodeTime), 1));
            AddPrim0 (result, Callable.MakePrim0 ("encodeTime", Callable.MakeHostCallable (EncodeTime), 1));

            // Stringification.
            AddPrim0 (result, Callable.MakePrim0 ("string", Callable.MakeHostCallable (ShovelString), 1));
            AddPrim0 (result, Callable.MakePrim0 (
                "stringRepresentation", Callable.MakeHostCallable (ShovelStringRepresentation), 1)
            );

            // Parsing numbers.
            AddPrim0 (result, Callable.MakePrim0 ("parseInt", Callable.MakeHostCallable (ParseInt), 1));
            AddPrim0 (result, Callable.MakePrim0 ("parseFloat", Callable.MakeHostCallable (ParseFloat), 1));

            // Exception throwing.
            AddPrim0 (result, Callable.MakePrim0 ("panic", Callable.MakeHostCallable (Panic), 1));

            return result;
        }

        static void AddError (VmApi api)
        {
            api.RaiseShovelError (
                    "Arguments must have the same type (numbers or strings or arrays).");
        }

        internal static void Add (VmApi api, ref Value t1, ref Value t2)
        {
            switch (t1.Kind) {
            case Value.Kinds.Integer:
                switch (t2.Kind) {
                case Value.Kinds.Integer:
                    t1.Kind = Value.Kinds.Integer;
                    t1.IntegerValue = t1.IntegerValue + t2.IntegerValue;
                    break;
                case Value.Kinds.Double:
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.IntegerValue + t2.DoubleValue;
                    break;
                default:
                    AddError (api);
                    break;
                }
                break;
            case Value.Kinds.Double:
                switch (t2.Kind) {
                case Value.Kinds.Integer:
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.DoubleValue + t2.IntegerValue;
                    break;
                case Value.Kinds.Double:
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.DoubleValue + t2.DoubleValue;
                    break;
                default:
                    AddError (api);
                    break;
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    t1.Kind = Value.Kinds.String;
                    t1.StringValue = t1.StringValue + t2.StringValue;
                } else {
                    AddError (api);
                }
                break;
            case Value.Kinds.Array:
                if (t2.Kind == Value.Kinds.Array) {
                    var result = new List<Value> ();
                    result.AddRange (t1.ArrayValue);
                    result.AddRange (t2.ArrayValue);
                    t1.Kind = Value.Kinds.Array;
                    t1.ArrayValue = result;
                } else {
                    AddError (api);
                }
                break;
            default:
                AddError (api);
                break;
            }
        }

        static void BothNumbersError (VmApi api)
        {
            api.RaiseShovelError ("Both arguments must be numbers.");
        }

        internal static void Subtract (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer) {
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Integer;
                    t1.IntegerValue = t1.IntegerValue - t2.IntegerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.IntegerValue - t2.DoubleValue;
                } else {
                    BothNumbersError (api);
                }
            } else if (t1.Kind == Value.Kinds.Double) {
                if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.DoubleValue - t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.DoubleValue - t2.IntegerValue;
                } else {
                    BothNumbersError (api);
                }
            } else {
                BothNumbersError (api);
            }
        }

        internal static void UnaryMinus (VmApi api, ref Value t1)
        {
            if (t1.Kind == Value.Kinds.Integer) {
                t1.IntegerValue = -t1.IntegerValue;
            } else if (t1.Kind == Value.Kinds.Double) {
                t1.DoubleValue = -t1.DoubleValue;
            } else {
                api.RaiseShovelError ("Argument must be number.");
            }
        }

        internal static void Multiply (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer) {
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Integer;
                    t1.IntegerValue = t1.IntegerValue * t2.IntegerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.IntegerValue * t2.DoubleValue;
                } else {
                    BothNumbersError (api);
                }
            } else if (t1.Kind == Value.Kinds.Double) {
                if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.DoubleValue * t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.DoubleValue * t2.IntegerValue;
                } else {
                    BothNumbersError (api);
                }
            } else {
                BothNumbersError (api);
            }
        }

        internal static void Divide (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer) {
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Integer;
                    t1.IntegerValue = t1.IntegerValue / t2.IntegerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.IntegerValue / t2.DoubleValue;
                } else {
                    BothNumbersError (api);
                }
            } else if (t1.Kind == Value.Kinds.Double) {
                if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.DoubleValue / t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = t1.DoubleValue / t2.IntegerValue;
                } else {
                    BothNumbersError (api);
                }
            } else {
                BothNumbersError (api);
            }
        }

        internal static void ShiftLeft (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.IntegerValue = t1.IntegerValue << (int)t2.IntegerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        internal static void ShiftRight (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.IntegerValue = t1.IntegerValue >> (int)t2.IntegerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        internal static void Modulo (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.IntegerValue = t1.IntegerValue % t2.IntegerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        internal static long Expt (long b, long e)
        {
            long result = 1;
            while (e > 0) {
                if (e % 2 == 0) {
                    e = e >> 1;
                    b = b * b;
                } else {
                    result *= b;
                    e --;
                }
            }
            return result;
        }

        internal static void Pow (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer) {
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Integer;
                    t1.IntegerValue = Expt (t1.IntegerValue, t2.IntegerValue);
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = Math.Pow (t1.IntegerValue, t2.DoubleValue);
                } else {
                    BothNumbersError (api);
                }
            } else if (t1.Kind == Value.Kinds.Double) {
                if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = Math.Pow (t1.DoubleValue, t2.DoubleValue);
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Double;
                    t1.DoubleValue = Math.Pow (t1.DoubleValue, t2.IntegerValue);
                } else {
                    BothNumbersError (api);
                }
            } else {
                BothNumbersError (api);
            }
        }

        internal static void Floor (VmApi api, ref Value t1)
        {
            if (t1.Kind == Value.Kinds.Double) {
                t1.Kind = Value.Kinds.Integer;
                t1.IntegerValue = (long)Math.Floor (t1.DoubleValue);
            } else if (t1.Kind == Value.Kinds.Integer) {
            } else {
                api.RaiseShovelError ("Argument must be number.");
            }
        }

        internal static int CompareStrings (string s1, string s2)
        {
            if (s1 == null) {
                if (s2 == null) {
                    return 0;
                } else {
                    return 2;
                }
            } else if (s2 == null) {
                return 2;
            } else {
                return s1.CompareTo (s2);
            }
        }

        static void LessThanError (VmApi api)
        {
            api.RaiseShovelError (
                "Arguments must have the same type (numbers or strings).");
        }

        internal static void LessThan (VmApi api, ref Value t1, ref Value t2)
        {
            switch (t1.Kind) {
            case Value.Kinds.Integer:
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.IntegerValue < t2.IntegerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.IntegerValue < t2.DoubleValue;
                } else {
                    LessThanError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    t1.BoolValue = CompareStrings (t1.StringValue, t2.StringValue) == -1;
                } else {
                    LessThanError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.DoubleValue < t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.DoubleValue < t2.IntegerValue;
                } else {
                    LessThanError (api);
                }
                break;
            }
            t1.Kind = Value.Kinds.Bool;
        }

        internal static void LessThanOrEqual (VmApi api, ref Value t1, ref Value t2)
        {
            switch (t1.Kind) {
            case Value.Kinds.Integer:
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.IntegerValue <= t2.IntegerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.IntegerValue <= t2.DoubleValue;
                } else {
                    LessThanError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    var comparison = CompareStrings (t1.StringValue, t2.StringValue);
                    t1.BoolValue = comparison == -1 || comparison == 0;
                } else {
                    LessThanError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.DoubleValue <= t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.DoubleValue <= t2.IntegerValue;
                } else {
                    LessThanError (api);
                }
                break;
            }
            t1.Kind = Value.Kinds.Bool;
        }

        internal static void GreaterThan (VmApi api, ref Value t1, ref Value t2)
        {
            switch (t1.Kind) {
            case Value.Kinds.Integer:
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.IntegerValue > t2.IntegerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.IntegerValue > t2.DoubleValue;
                } else {
                    LessThanError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    var comparison = CompareStrings (t1.StringValue, t2.StringValue);
                    t1.BoolValue = comparison == 1;
                } else {
                    LessThanError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.DoubleValue > t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.DoubleValue > t2.IntegerValue;
                } else {
                    LessThanError (api);
                }
                break;
            }
            t1.Kind = Value.Kinds.Bool;
        }

        internal static void GreaterThanOrEqual (VmApi api, ref Value t1, ref Value t2)
        {
            switch (t1.Kind) {
            case Value.Kinds.Integer:
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.IntegerValue >= t2.IntegerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.IntegerValue >= t2.DoubleValue;
                } else {
                    LessThanError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    var comparison = CompareStrings (t1.StringValue, t2.StringValue);
                    t1.BoolValue = comparison == 1 || comparison == 0;
                } else {
                    LessThanError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.DoubleValue >= t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.DoubleValue >= t2.IntegerValue;
                } else {
                    LessThanError (api);
                }
                break;
            }
            t1.Kind = Value.Kinds.Bool;
        }

        static void AreEqualError (VmApi api)
        {
            api.RaiseShovelError (
                        "Arguments must have the same type (numbers or strings), or at least one must be null.");
        }

        internal static void AreEqual (VmApi api, ref Value t1, ref Value t2)
        {
            switch (t1.Kind) {
            case Value.Kinds.Integer:
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.IntegerValue == t2.IntegerValue;
                } else if (t2.Kind == Value.Kinds.Null) {
                    t1.BoolValue = false;
                } else {
                    AreEqualError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    int comparison = CompareStrings (t1.StringValue, t2.StringValue);
                    t1.BoolValue = comparison == 0;
                } else if (t2.Kind == Value.Kinds.Null) {
                    t1.BoolValue = false;
                } else {
                    AreEqualError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.DoubleValue == t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Null) {
                    t1.BoolValue = false;
                } else {
                    AreEqualError (api);
                }
                break;
            case Value.Kinds.Bool:
                if (t2.Kind == Value.Kinds.Bool) {
                    t1.BoolValue = t1.BoolValue == t2.BoolValue;
                } else if (t2.Kind == Value.Kinds.Null) {
                    t1.BoolValue = false;
                } else {
                    AreEqualError (api);
                }
                break;
            default:
                if (t1.Kind == Value.Kinds.Null && t2.Kind == Value.Kinds.Null) {
                    t1.BoolValue = true;
                } else if (t1.Kind == Value.Kinds.Null || t2.Kind == Value.Kinds.Null) {
                    t2.BoolValue = false;
                } else {
                    AreEqualError (api);
                }
                break;
            }
            t1.Kind = Value.Kinds.Bool;
        }

        internal static void AreNotEqual (VmApi api, ref Value t1, ref Value t2)
        {
            AreEqual (api, ref t1, ref t2);
            t1.BoolValue = !t1.BoolValue;
        }

        internal static void LogicalNot (VmApi api, ref Value argument)
        {
            if (argument.Kind == Value.Kinds.Bool) {
                argument.BoolValue = !argument.BoolValue;
            } else {
                api.RaiseShovelError ("Argument must be boolean.");
            }
        }

        internal static void BitwiseAnd (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.IntegerValue = t1.IntegerValue & t2.IntegerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        internal static void BitwiseOr (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.IntegerValue = t1.IntegerValue | t2.IntegerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        internal static void BitwiseXor (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.IntegerValue = t1.IntegerValue ^ t2.IntegerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        static Value HashConstructor (VmApi api, Value[] args, int start, int length)
        {
            if (args.Length % 2 != 0) {
                api.RaiseShovelError ("Must provide an even number of arguments.");
            }
            var sizeIncrease = 1 + 2 * args.Length;
            api.CellsIncrementHerald (sizeIncrease);
            var result = new Dictionary<Value, Value> ();
            for (var i = start; i < start + length; i += 2) {
                if (args [i].Kind == Value.Kinds.String) {
                    result [args [i]] = args [i + 1];
                } else {
                    api.RaiseShovelError ("Keys must be strings");
                }
            }
            api.CellsIncrementer (sizeIncrease);
            return Value.Make (result);
        }

        internal static void HasKey (VmApi api, ref Value hash, ref Value key)
        {
            if (hash.Kind != Value.Kinds.Hash) {
                api.RaiseShovelError ("First argument must be a hash table.");
            }
            if (key.Kind != Value.Kinds.String) {
                api.RaiseShovelError ("Second argument must be a string.");
            }
            hash.BoolValue = hash.HashValue.ContainsKey (key);
            hash.Kind = Value.Kinds.Bool;
        }

        internal static void Keys (VmApi api, ref Value hash)
        {
            if (hash.Kind != Value.Kinds.Hash) {
                api.RaiseShovelError ("First argument must be a hash table.");
            }
            var result = new List<Value> ();
            result.AddRange (hash.HashValue.Keys);
            hash.ArrayValue = result;
            hash.Kind = Value.Kinds.Array;
        }

        static Value ArrayConstructor (VmApi api, Value[] args, int start, int length)
        {
            var result = new List<Value> ();
            for (var i = start; i < start+length; i++) {
                result.Add (args [i]);
            }
            return Value.Make (result);
        }

        static Value SizedArrayConstructor (VmApi api, Value size)
        {
            if (size.Kind != Value.Kinds.Integer) {
                api.RaiseShovelError ("Argument must be an integer.");
            }
            var result = new List<Value> ();
            for (var i = 0; i < size.IntegerValue; i++) {
                result.Add (Value.Make ());
            }
            return Value.Make (result);
        }

        static void CheckVector (VmApi api, ref Value vector)
        {
            if (vector.Kind != Value.Kinds.Array) {
                api.RaiseShovelError ("First argument must be a vector.");
            }
        }

        internal static void ArrayPush (VmApi api, ref Value array, ref Value value)
        {
            CheckVector (api, ref array);
            array.ArrayValue.Add (value);
            array = value;
        }

        internal static void ArrayPop (VmApi api, ref Value array)
        {
            CheckVector (api, ref array);
            var vector = array.ArrayValue;
            if (vector.Count == 0) {
                api.RaiseShovelError ("Can't pop from an empty array.");
            }
            array = vector [vector.Count - 1];
            vector.RemoveAt (vector.Count - 1);
        }

        internal static void ArrayOrHashGet 
            (VmApi api, ref Value arrayOrHashOrString, ref Value index)
        {
            if (arrayOrHashOrString.Kind == Value.Kinds.Array) {
                if (index.Kind == Value.Kinds.Integer) {
                    arrayOrHashOrString = arrayOrHashOrString.ArrayValue [(int)index.IntegerValue];
                } else {
                    api.RaiseShovelError ("Getting an array element requires an integer index.");
                }
            } else if (arrayOrHashOrString.Kind == Value.Kinds.Hash) {
                if (index.Kind == Value.Kinds.String) {
                    if (!arrayOrHashOrString.HashValue.ContainsKey(index)) {
                        api.RaiseShovelError(String.Format ("Key '{0}' not found.", index.StringValue));
                    } 
                    arrayOrHashOrString = arrayOrHashOrString.HashValue [index];
                } else {
                    api.RaiseShovelError ("Getting a hash table value requires a key that is a string.");
                }
            } else if (arrayOrHashOrString.Kind == Value.Kinds.String) {
                if (index.Kind == Value.Kinds.Integer) {
                    arrayOrHashOrString = Value.Make (
                        arrayOrHashOrString.StringValue [(int)index.IntegerValue].ToString ());
                } else {
                    api.RaiseShovelError ("Getting an string element requires an integer index.");
                }
            } else {
                api.RaiseShovelError ("First argument must be a hash or an array or a string.");
            }
        }

        internal static void HashGetDot (VmApi api, ref Value hash, ref Value index)
        {
            if (hash.Kind != Value.Kinds.Hash) {
                api.RaiseShovelError ("First argument must be a hash table.");
            }
            if (index.Kind != Value.Kinds.String) {
                api.RaiseShovelError ("Second argument must be a string.");
            }
            if (!hash.HashValue.ContainsKey (index)) {
                api.RaiseShovelError ("Key not found in hash table.");
            }
            hash = hash.HashValue [index];
        }

        internal static void ArrayOrHashSet (
            VmApi api, ref Value arrayOrHashOrString, ref Value index, ref Value value)
        {
            if (arrayOrHashOrString.Kind == Value.Kinds.Array) {
                if (index.Kind == Value.Kinds.Integer) {
                    arrayOrHashOrString.ArrayValue [(int)index.IntegerValue] = value;
                    arrayOrHashOrString = value;
                } else {
                    api.RaiseShovelError ("Setting an array element requires an integer index.");
                }
            } else if (arrayOrHashOrString.Kind == Value.Kinds.Hash) {
                if (index.Kind == Value.Kinds.String) {
                    arrayOrHashOrString.HashValue [index] = value;
                    arrayOrHashOrString = value;
                } else {
                    api.RaiseShovelError ("Setting a hash table value requires a key that is a string.");
                }
            } else {
                api.RaiseShovelError ("First argument must be a hash or an array.");
            }
        }

        internal static void GetLength (VmApi api, ref Value arrayOrString)
        {
            if (arrayOrString.Kind == Value.Kinds.Array) {
                arrayOrString.Kind = Value.Kinds.Integer;
                arrayOrString.IntegerValue = arrayOrString.ArrayValue.Count;
            } else if (arrayOrString.Kind == Value.Kinds.String) {
                arrayOrString.Kind = Value.Kinds.Integer;
                arrayOrString.IntegerValue = (long)arrayOrString.StringValue.Length;
            } else {
                api.RaiseShovelError ("Argument must be a string or an array.");
            }
        }

        static void AdjustRealStartEnd (VmApi api, ref int realStart, ref int realEnd, int length)
        {
            if (realStart < 0) {
                realStart += length;
            }
            if (realEnd < 0) {
                realEnd += length + 1;
            }
            if (realStart > realEnd) {
                api.RaiseShovelError (String.Format (
                        "Starting index ({0}) is larger than ending index ({1}).",
                        realStart, realEnd)
                );
            }
            if (realStart < 0) {
                api.RaiseShovelError (String.Format (
                        "Starting index ({0}) is less than 0.", realStart)
                );
            }
            if (realEnd > length) {
                api.RaiseShovelError (String.Format (
                    "End index ({0}) is larger than the length of the sequence ({1}).",
                        realEnd, length)
                );
            }
        }

        static Value GetSlice (VmApi api, Value arrayOrString, Value start, Value end)
        {
            if (start.Kind != Value.Kinds.Integer) {
                api.RaiseShovelError ("The start index must be an integer.");
                throw new InvalidOperationException ();
            }
            if (end.Kind != Value.Kinds.Integer) {
                api.RaiseShovelError ("The end index must be an integer.");
                throw new InvalidOperationException ();
            }
            if (arrayOrString.Kind == Value.Kinds.Array) {
                var length = arrayOrString.ArrayValue.Count;
                var realStart = (int)start.IntegerValue;
                var realEnd = (int)end.IntegerValue;
                AdjustRealStartEnd (api, ref realStart, ref realEnd, length);
                return Value.Make (
                    arrayOrString.ArrayValue.GetRange (realStart, realEnd - realStart));
            } else if (arrayOrString.Kind == Value.Kinds.String) {
                var length = arrayOrString.StringValue.Length;
                var realStart = (int)start.IntegerValue;
                var realEnd = (int)end.IntegerValue;
                AdjustRealStartEnd (api, ref realStart, ref realEnd, length);
                return Value.Make (
                    arrayOrString.StringValue.Substring (realStart, realEnd - realStart));
            } else {
                api.RaiseShovelError ("Argument must be a string or an array.");
                throw new InvalidOperationException ();
            }
        }

        static void CheckString (VmApi api, Value str)
        {
            if (str.Kind != Value.Kinds.String) {
                api.RaiseShovelError ("Argument must be a string.");
            }
        }

        static Value StringUpper (VmApi api, Value str)
        {
            CheckString (api, str);
            api.CellsIncrementer (str.StringValue.Length);
            return Value.Make (str.StringValue.ToUpper ());
        }

        static Value StringLower (VmApi api, Value str)
        {
            CheckString (api, str);
            api.CellsIncrementer (str.StringValue.Length);
            return Value.Make (str.StringValue.ToLower ());
        }

        static DateTime unixEpoch = new DateTime (1970, 1, 1);

        static Value UtcSecondsSinceUnixEpoch (VmApi api)
        {
            return Value.MakeInt (
                (long)(DateTime.UtcNow - unixEpoch).TotalSeconds);
        }

        static Value DecodeTime (VmApi api, Value timeInSeconds)
        {
            if (timeInSeconds.Kind != Value.Kinds.Integer) {
                api.RaiseShovelError ("Argument must be an integer.");
            }
            var epochTimeSpan = TimeSpan.FromSeconds (timeInSeconds.IntegerValue);
            var date = Prim0.unixEpoch + epochTimeSpan;
            var result = new Dictionary<Value, Value> ();
            result [Value.Make ("year")] = Value.MakeInt (date.Year);
            result [Value.Make ("month")] = Value.MakeInt (date.Month);
            result [Value.Make ("day")] = Value.MakeInt (date.Day);
            var dayOfWeekKey = Value.Make ("dayOfWeek");
            switch (date.DayOfWeek) {
            case DayOfWeek.Monday:
                result [dayOfWeekKey] = Value.MakeInt (1);
                break;
            case DayOfWeek.Tuesday:
                result [dayOfWeekKey] = Value.MakeInt (2);
                break;
            case DayOfWeek.Wednesday:
                result [dayOfWeekKey] = Value.MakeInt (3);
                break;
            case DayOfWeek.Thursday:
                result [dayOfWeekKey] = Value.MakeInt (4);
                break;
            case DayOfWeek.Friday:
                result [dayOfWeekKey] = Value.MakeInt (5);
                break;
            case DayOfWeek.Saturday:
                result [dayOfWeekKey] = Value.MakeInt (6);
                break;
            case DayOfWeek.Sunday:
                result [dayOfWeekKey] = Value.MakeInt (7);
                break;
            }
            result [Value.Make ("hour")] = Value.MakeInt (date.Hour);
            result [Value.Make ("minute")] = Value.MakeInt (date.Minute);
            result [Value.Make ("second")] = Value.MakeInt (date.Second);
            return Value.Make (result);
        }

        static void CheckBoundedInteger (
            VmApi api, 
            Value hash, string key, 
            long min, long max, string errorMessageFormat)
        {
            var shKey = Value.Make (key);
            if (!hash.HashValue.ContainsKey (shKey)) {
                api.RaiseShovelError (String.Format (errorMessageFormat, "NIL"));
            }
            var value = hash.HashValue [shKey];
            var isValid = (value.Kind == Value.Kinds.Integer) 
                && (value.IntegerValue >= min) 
                && (value.IntegerValue <= max);
            if (!isValid) {
                api.RaiseShovelError (String.Format (errorMessageFormat, value));
            }
        }

        static Value EncodeTime (VmApi api, Value hash)
        {
            if (hash.Kind != Value.Kinds.Hash) {
                api.RaiseShovelError ("Argument must be a valid time hash (as returned by 'decodeTime').");
            }
            CheckBoundedInteger (
                api, hash, "second", 0, 59,
                "Argument must be a valid time hash (as returned by 'decodeTime') - invalid second '{0}'.");
            CheckBoundedInteger (
                api, hash, "minute", 0, 59,
                "Argument must be a valid time hash (as returned by 'decodeTime') - invalid minute '{0}'.");
            CheckBoundedInteger (
                api, hash, "hour", 0, 59,
                "Argument must be a valid time hash (as returned by 'decodeTime') - invalid hour '{0}'.");
            CheckBoundedInteger (
                api, hash, "day", 1, 31,
                "Argument must be a valid time hash (as returned by 'decodeTime') - invalid day '{0}'.");
            CheckBoundedInteger (
                api, hash, "month", 1, 12,
                "Argument must be a valid time hash (as returned by 'decodeTime') - invalid month '{0}'.");
            var date = new DateTime (
                (int)hash.HashValue [Value.Make ("year")].IntegerValue, 
                (int)hash.HashValue [Value.Make ("month")].IntegerValue, 
                (int)hash.HashValue [Value.Make ("day")].IntegerValue, 
                (int)hash.HashValue [Value.Make ("hour")].IntegerValue, 
                (int)hash.HashValue [Value.Make ("minute")].IntegerValue, 
                (int)hash.HashValue [Value.Make ("second")].IntegerValue);
            return Value.MakeInt ((long)(date - unixEpoch).TotalSeconds);
        }

        internal static void IsString (VmApi api, ref Value obj)
        {
            obj.BoolValue = obj.Kind == Value.Kinds.String;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsHash (VmApi api, ref Value obj)
        {
            obj.BoolValue = obj.Kind == Value.Kinds.Hash;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsBool (VmApi api, ref Value obj)
        {
            obj.BoolValue = obj.Kind == Value.Kinds.Bool;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsArray (VmApi api, ref Value obj)
        {
            obj.BoolValue = obj.Kind == Value.Kinds.Array;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsNumber (VmApi api, ref Value obj)
        {
            obj.BoolValue = 
                obj.Kind == Value.Kinds.Integer || obj.Kind == Value.Kinds.Double;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsInteger (VmApi api, ref Value obj)
        {
            obj.BoolValue = obj.Kind == Value.Kinds.Integer;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsCallable (VmApi api, ref Value obj)
        {
            obj.BoolValue = obj.Kind == Value.Kinds.Callable;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static Value ShovelString (VmApi api, Value obj)
        {
            return Value.Make (ShovelStringImpl (api, obj));
        }

        static string ShovelStringImpl (VmApi api, Value obj)
        {
            if (obj.Kind == Value.Kinds.String) {
                return obj.StringValue;
            } else if (obj.Kind == Value.Kinds.Array) {
                return "[...array...]";
            } else if (obj.Kind == Value.Kinds.Integer) {
                return obj.IntegerValue.ToString (CultureInfo.InvariantCulture);
            } else if (obj.Kind == Value.Kinds.Double) {
                return obj.DoubleValue.ToString (CultureInfo.InvariantCulture);
            } else if (obj.Kind == Value.Kinds.Hash) {
                return "[...hash...]";
            } else if (obj.Kind == Value.Kinds.Callable) {
                return "[...callable...]";
            } else if (obj.Kind == Value.Kinds.Bool) {
                return obj.BoolValue.ToString ().ToLower ();
            } else if (obj.Kind == Value.Kinds.Null) {
                return "null";
            } else {
                UnknownTypeError (api);
            }
            throw new InvalidOperationException ();
        }

        static void UnknownTypeError (VmApi api)
        {
            api.RaiseShovelError ("ShovelValue of unknown type.");
        }

        internal static Value ShovelStringRepresentation (VmApi api, Value obj)
        {
            var result = Value.Make (ShovelStringRepresentationImpl (api, obj, new HashSet<object> ()));
            api.CellsIncrementer (result.StringValue.Length);
            return result;
        }

        private static string ShovelStringRepresentationImpl (
            VmApi api, Value obj, HashSet<object> visited)
        {
            if (visited.Contains (obj)) {
                return "[...loop...]";
            } else if (obj.Kind == Value.Kinds.String) {
                return String.Format ("\"{0}\"", obj.StringValue.Replace ("\"", "\\\""));
            } else if (obj.Kind == Value.Kinds.Array) {
                visited.Add (obj);
                var stringReps = obj.ArrayValue
                    .Select (elem => ShovelStringRepresentationImpl (api, elem, visited));
                return String.Format ("array({0})", String.Join (", ", stringReps));
            } else if (obj.Kind == Value.Kinds.Hash) {
                visited.Add (obj);
                var stringReps = new List<string> ();
                foreach (var key in obj.HashValue.Keys) {
                    stringReps.Add ((string)ShovelStringRepresentationImpl (api, key, visited));
                    stringReps.Add ((string)ShovelStringRepresentationImpl (api, obj.HashValue [key], visited));
                }
                return String.Format ("hash({0})", String.Join (", ", stringReps));
            } else if (obj.Kind == Value.Kinds.Null
                || obj.Kind == Value.Kinds.Integer
                || obj.Kind == Value.Kinds.Double
                || obj.Kind == Value.Kinds.Bool
                || obj.Kind == Value.Kinds.Callable) {
                return ShovelStringImpl (api, obj);
            } else {
                UnknownTypeError (api);
            }
            return null;
        }

        static Value ParseInt (VmApi api, Value str)
        {
            CheckString (api, str);
            return Value.MakeInt (long.Parse (str.StringValue));
        }

        static Value ParseFloat (VmApi api, Value str)
        {
            CheckString (api, str);
            return Value.MakeFloat (double.Parse (str.StringValue));
        }

        static Value Panic (VmApi api, Value str)
        {
            CheckString (api, str);
            api.RaiseShovelError (str.StringValue);
            throw new InvalidOperationException ();
        }

    }
}

