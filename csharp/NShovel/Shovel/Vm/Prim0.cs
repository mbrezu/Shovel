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
using System.Text;

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

            // Key-not-found and index-out-of-range handlers.
            AddPrim0 (result, Callable.MakePrim0 ("setHandlers", Callable.MakeHostCallable(SetHandlers), 3));

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

            // Structs.
            AddPrim0 (result, Callable.MakePrim0 ("defstruct", Defstruct, 1));
            AddPrim0 (result, Callable.MakePrim0 ("make", InstantiateStruct, null));
            AddPrim0 (result, Callable.MakePrim0 ("structToHash", StructToHash, 1));
            AddPrim0 (result, Callable.MakePrim0 ("hashToStruct", HashToStruct, 2));

            return result;
        }

        static Value HashToStruct (VmApi api, Value[] args, int start, int length)
        {
            if (args [start].Kind != Value.Kinds.Struct) {
                api.RaiseShovelError ("First argument must be a struct.");
            }
            if (args [start + 1].Kind != Value.Kinds.Hash) {
                api.RaiseShovelError ("Second argument must be a hash.");
            }

            var ztruct = args [start].StructValue;
            var result = new StructInstance ();
            result.Struct = ztruct;
            result.Values = new Value[ztruct.Fields.Length];
            var hash = args [start + 1].HashValue;
            var sizeIncrease = 1 + ztruct.Fields.Length;
            api.CellsIncrementHerald (sizeIncrease);
            for (int i = 0; i < ztruct.Fields.Length; i++) {
                var svKey = Value.Make (ztruct.Fields [i]);
                if (hash.ContainsKey (svKey)) {
                    result.Values [i] = hash [svKey];
                }
            }
            api.CellsIncrementer (sizeIncrease);
            return Value.Make (result);
        }

        static Value StructToHash (VmApi api, Value[] args, int start, int length)
        {
            if (args [start].Kind != Value.Kinds.StructInstance) {
                api.RaiseShovelError ("First argument must be a struct instance.");
            }
            var result = new HashInstance ();
            var structInstance = args [start].StructInstanceValue;
            var ztruct = structInstance.Struct;
            var sizeIncrease = 1 + 2 * ztruct.Fields.Length;
            api.CellsIncrementHerald (sizeIncrease);
            for (int i = 0; i < ztruct.Fields.Length; i++) {
                result [Value.Make (ztruct.Fields [i])] = structInstance.Values [i];
            }
            api.CellsIncrementer (sizeIncrease);
            return Value.Make (result);
        }

        static Value InstantiateStruct (VmApi api, Value[] args, int start, int length)
        {
            if (length == 0) {
                api.RaiseShovelError ("Must provide at least one argument.");
            }
            if (args [start].Kind != Value.Kinds.Struct) {
                api.RaiseShovelError ("First argument must be a struct.");
            }
            var ztruct = args [start].StructValue;
            var sizeIncrease = 1 + ztruct.Fields.Length;
            api.CellsIncrementHerald (sizeIncrease);
            var result = new StructInstance ();
            result.Struct = ztruct;
            result.Values = new Value[ztruct.Fields.Length];
            for (int i = 1; i < length; i++) {
                result.Values [i - 1] = args [start + i];
            }
            api.CellsIncrementer (sizeIncrease);
            return Value.Make (result);
        }

        static Value Defstruct (VmApi api, Value[] args, int start, int length)
        {
            if (args [start].Kind != Value.Kinds.Array) {
                api.RaiseShovelError ("Argument must be an array of strings.");
            }
            var fieldNames = args [start].ArrayValue;
            var sizeIncrease = 1 + length;
            api.CellsIncrementHerald (sizeIncrease);
            Struct newStruct = new Struct ();
            newStruct.Fields = new string[fieldNames.Count];
            for (int i = 0; i < newStruct.Fields.Length; i++) {
                if (fieldNames [i].Kind != Value.Kinds.String) {
                    api.RaiseShovelError ("Argument must be an array of strings.");
                }
                newStruct.Fields [i] = fieldNames [i].StringValue;
            }
            api.CellsIncrementer (sizeIncrease);
            return Value.Make (newStruct);
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
                    var result = new ArrayInstance ();
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

        static void RelationalError (VmApi api)
        {
            api.RaiseShovelError (
                "Arguments must have the same type (numbers or strings).");
        }

        internal static void DeleteDictionary (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind != Value.Kinds.Hash) {
                api.RaiseShovelError ("First argument must be a hash.");
            } else if (t2.Kind != Value.Kinds.String) {
                api.RaiseShovelError ("Second argument must be a string.");
            } else {
                t1.HashValue.Remove (t2);
            }
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
                    RelationalError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    t1.BoolValue = CompareStrings (t1.StringValue, t2.StringValue) == -1;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.DoubleValue < t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.DoubleValue < t2.IntegerValue;
                } else {
                    RelationalError (api);
                }
                break;
            default:
                RelationalError(api);
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
                    RelationalError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    var comparison = CompareStrings (t1.StringValue, t2.StringValue);
                    t1.BoolValue = comparison == -1 || comparison == 0;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.DoubleValue <= t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.DoubleValue <= t2.IntegerValue;
                } else {
                    RelationalError (api);
                }
                break;
            default:
                RelationalError(api);
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
                    RelationalError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    var comparison = CompareStrings (t1.StringValue, t2.StringValue);
                    t1.BoolValue = comparison == 1;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.DoubleValue > t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.DoubleValue > t2.IntegerValue;
                } else {
                    RelationalError (api);
                }
                break;
            default:
                RelationalError(api);
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
                    RelationalError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    var comparison = CompareStrings (t1.StringValue, t2.StringValue);
                    t1.BoolValue = comparison == 1 || comparison == 0;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.BoolValue = t1.DoubleValue >= t2.DoubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.BoolValue = t1.DoubleValue >= t2.IntegerValue;
                } else {
                    RelationalError (api);
                }
                break;
            default:
                RelationalError(api);
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
            if (length % 2 != 0) {
                api.RaiseShovelError ("Must provide an even number of arguments.");
            }
            var sizeIncrease = 1 + 2 * length;
            api.CellsIncrementHerald (sizeIncrease);
            var result = new HashInstance ();
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
            var result = new ArrayInstance ();
            var sizeIncrease = 1 + hash.HashValue.Count;
            api.CellsIncrementHerald (sizeIncrease);
            result.AddRange (hash.HashValue.Keys);
            hash.ArrayValue = result;
            hash.Kind = Value.Kinds.Array;
            api.CellsIncrementer (sizeIncrease);
        }

        static Value ArrayConstructor (VmApi api, Value[] args, int start, int length)
        {
            var result = new ArrayInstance ();
            var sizeIncrease = 1 + length;
            api.CellsIncrementHerald (sizeIncrease);
            for (var i = start; i < start+length; i++) {
                result.Add (args [i]);
            }
            api.CellsIncrementer (sizeIncrease);
            return Value.Make (result);
        }

        static Value SizedArrayConstructor (VmApi api, Value size)
        {
            if (size.Kind != Value.Kinds.Integer) {
                api.RaiseShovelError ("Argument must be an integer.");
            }
            var result = new ArrayInstance ();
            var sizeIncrease = 1 + (int)size.IntegerValue;
            api.CellsIncrementHerald (sizeIncrease);
            for (var i = 0; i < size.IntegerValue; i++) {
                result.Add (Value.Make ());
            }
            api.CellsIncrementer (sizeIncrease);
            return Value.Make (result);
        }

        static void CheckVector(VmApi api, ref Value vector)
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
            api.CellsIncrementer (1);
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

        internal static bool ArrayOrHashGet 
            (VmApi api, ref Value arrayOrHashOrString, ref Value index)
        {
            if (arrayOrHashOrString.Kind == Value.Kinds.Array) {
                if (index.Kind == Value.Kinds.Integer) {
                    var idx = (int)index.IntegerValue;
                    if (idx < 0 || idx >= arrayOrHashOrString.ArrayValue.Count)
                    {
                        if (arrayOrHashOrString.ArrayValue.IndirectGet.Kind == Value.Kinds.Callable)
                        {
                            return false;
                        }
                        else { 
                            api.RaiseShovelError("Index out of range.");
                        }
                    }
                    arrayOrHashOrString = arrayOrHashOrString.ArrayValue [idx];
                } else {
                    api.RaiseShovelError ("Getting an array element requires an integer index.");
                }
            } else if (arrayOrHashOrString.Kind == Value.Kinds.Hash) {
                if (index.Kind == Value.Kinds.String) {
                    if (!arrayOrHashOrString.HashValue.ContainsKey (index)) {
                        if (arrayOrHashOrString.HashValue.IndirectGet.Kind == Value.Kinds.Callable) { 
                            return false;
                        }
                        else
                        {
                            api.RaiseShovelError(String.Format("Key '{0}' not found.", index.StringValue));
                        }
                    } 
                    arrayOrHashOrString = arrayOrHashOrString.HashValue [index];
                } else {
                    api.RaiseShovelError ("Getting a hash table value requires a key that is a string.");
                }
            } else if (arrayOrHashOrString.Kind == Value.Kinds.String) {
                if (index.Kind == Value.Kinds.Integer) {
                    var idx = (int)index.IntegerValue;
                    if (idx < 0 || idx >= arrayOrHashOrString.StringValue.Length)
                    {
                        api.RaiseShovelError("Index out of range.");
                    }
                    arrayOrHashOrString = Value.Make (
                        arrayOrHashOrString.StringValue [idx].ToString ());
                } else {
                    api.RaiseShovelError ("Getting an string element requires an integer index.");
                }
            } else {
                api.RaiseShovelError ("First argument must be a hash or an array or a string.");
            }
            return true;
        }

        static int FindLocationInStruct (VmApi api, Struct ztruct, string key)
        {
            for (int i = 0; i < ztruct.Fields.Length; i ++) {
                if (key == ztruct.Fields [i]) {
                    return i;
                }
            }
            api.RaiseShovelError ("Key not found in struct.");
            return -1;
        }

        internal static bool HashOrStructGetDot (Vm vm, VmApi api, ref Value obj, ref Value index)
        {
            if (index.Kind != Value.Kinds.String) {
                api.RaiseShovelError ("Second argument must be a string.");
            }
            if (obj.Kind == Value.Kinds.StructInstance) {
                var cache = vm.GetCurrentCache ();
                var structInstance = obj.StructInstanceValue;
                var ztruct = structInstance.Struct;
                if (cache != null) {
                    var info = (Tuple<Struct, int>)cache;
                    if (info.Item1 == ztruct) {
                        obj = structInstance.Values [info.Item2];
                        return true;
                    }
                }
                int location = FindLocationInStruct (api, ztruct, index.StringValue);
                obj = structInstance.Values [location];
                vm.SetCurrentCache (Tuple.Create (ztruct, location));
            } else if (obj.Kind == Value.Kinds.Hash) {
                if (!obj.HashValue.ContainsKey (index)) {
                    if (obj.HashValue.IndirectGet.Kind == Value.Kinds.Callable) { 
                        return false;
                    }
                    else {
                        api.RaiseShovelError("Key not found in hash table.");
                    }
                }
                obj = obj.HashValue [index];
            } else {
                api.RaiseShovelError ("First argument must be a struct instance or a hash table.");
            }
            return true;
        }

        internal static bool HashOrStructDotSet (
            Vm vm, VmApi api, ref Value obj, ref Value index, ref Value value)
        {
            if (obj.Kind == Value.Kinds.StructInstance) {
                var cache = vm.GetCurrentCache ();
                var structInstance = obj.StructInstanceValue;
                var ztruct = structInstance.Struct;
                if (cache != null) {
                    var info = (Tuple<Struct, int>)cache;
                    if (info.Item1 == ztruct) {
                        structInstance.Values [info.Item2] = value;
                        return true;
                    }
                }
                int location = FindLocationInStruct (api, ztruct, index.StringValue);
                structInstance.Values [location] = value;
                vm.SetCurrentCache (Tuple.Create (ztruct, location));
            } else if (obj.Kind == Value.Kinds.Hash) {
                return HashSet(api, ref obj, ref index, ref value);
            } else {
                api.RaiseShovelError ("First argument must be a hash or a struct instance.");
            }
            return true;
        }

        private static bool HashSet(VmApi api, ref Value obj, ref Value index, ref Value value)
        {
            if (index.Kind == Value.Kinds.String)
            {
                if (!obj.HashValue.ContainsKey(index))
                {
                    var hasSetter = obj.HashValue.IndirectSet.Kind == Value.Kinds.Callable;
                    if (hasSetter)
                    {
                        return false;
                    }
                }
                obj.HashValue[index] = value;
                obj = value;
                return true;
            }
            else
            {
                api.RaiseShovelError("Setting a hash table value requires a key that is a string.");
                throw new Exception();
            }            
        }

        internal static bool ArrayOrHashSet (
            VmApi api, ref Value obj, ref Value index, ref Value value)
        {
            if (obj.Kind == Value.Kinds.Array) {
                if (index.Kind == Value.Kinds.Integer) {
                    var idx = (int)index.IntegerValue;
                    if (idx < 0 || idx >= obj.ArrayValue.Count)
                    {
                        if (obj.ArrayValue.IndirectSet.Kind == Value.Kinds.Callable)
                        {
                            return false;
                        }
                        else { 
                            api.RaiseShovelError("Index out of range.");
                        }
                    }
                    obj.ArrayValue [idx] = value;
                    obj = value;
                } else {
                    api.RaiseShovelError ("Setting an array element requires an integer index.");
                }
            }
            else if (obj.Kind == Value.Kinds.Hash)
            {
                return HashSet(api, ref obj, ref index, ref value);
            } else {
                api.RaiseShovelError ("First argument must be an array or a hash.");
            }
            return true;
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

        static Value SetHandlers(VmApi api, Value arrayOrHash, Value getter, Value setter)
        {
            if (getter.Kind != Value.Kinds.Callable || getter.CallableValue.Arity != 2)
            {
                api.RaiseShovelError("The second parameter should be a callable with 2 parameters.");
                throw new InvalidOperationException();
            }
            if (setter.Kind != Value.Kinds.Callable || setter.CallableValue.Arity != 3)
            {
                api.RaiseShovelError("The second parameter should be a callable with 3 parameters.");
                throw new InvalidOperationException();
            }
            if (arrayOrHash.Kind == Value.Kinds.Array)
            {
                var array = arrayOrHash.ArrayValue;
                array.IndirectGet = getter;
                array.IndirectSet = setter;
            }
            else if (arrayOrHash.Kind == Value.Kinds.Hash)
            {
                var hash = arrayOrHash.HashValue;
                hash.IndirectGet = getter;
                hash.IndirectSet = setter;
            }
            else
            {
                api.RaiseShovelError("The first parameter should be an array or hash.");
                throw new InvalidOperationException();
            }
            return Value.Make();
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
                api.CellsIncrementer (realEnd - realStart);
                return Value.Make (
                    arrayOrString.ArrayValue.GetRange2 (realStart, realEnd - realStart));
            } else if (arrayOrString.Kind == Value.Kinds.String) {
                var length = arrayOrString.StringValue.Length;
                var realStart = (int)start.IntegerValue;
                var realEnd = (int)end.IntegerValue;
                AdjustRealStartEnd (api, ref realStart, ref realEnd, length);
                api.CellsIncrementer (realEnd - realStart);
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
            api.CellsIncrementer (str.StringValue.Length);
            return Value.Make (str.StringValue.ToUpper ());
        }

        static Value StringLower (VmApi api, Value str)
        {
            CheckString (api, str);
            api.CellsIncrementer (str.StringValue.Length);
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
            var result = new HashInstance ();
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
            api.CellsIncrementer (8);
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

        internal static void IsStruct (VmApi api, ref Value obj)
        {
            obj.BoolValue = obj.Kind == Value.Kinds.Struct;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsStructInstance (VmApi api, ref Value obj, ref Value str)
        {
            if (str.Kind != Value.Kinds.Struct) {
                api.RaiseShovelError ("Second argument must be a struct.");
            }
            var result = obj.Kind == Value.Kinds.StructInstance && obj.StructInstanceValue.Struct == str.StructValue;
            obj.BoolValue = result;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static Value ShovelString (VmApi api, Value obj)
        {
            var result = ShovelStringImpl (api, obj);
            api.CellsIncrementer (result.Length);
            return Value.Make (result);
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
            } else if (obj.Kind == Value.Kinds.Struct) {
                return "[...struct...]";
            } else if (obj.Kind == Value.Kinds.StructInstance) {
                return "[...struct instance...]";
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

        private static string StructAsString (Struct ztruct)
        {
            var sb = new StringBuilder ();
            sb.Append ("defstruct(array(");
            var pieces = new List<string> ();
            foreach (var str in ztruct.Fields) {
                pieces.Add (String.Format ("'{0}'", str));
            }
            sb.Append (String.Join (", ", pieces));
            sb.Append ("))");
            return sb.ToString ();
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
            } else if (obj.Kind == Value.Kinds.Struct) {
                return StructAsString (obj.StructValue);
            } else if (obj.Kind == Value.Kinds.StructInstance) {
                visited.Add (obj);
                var sb = new StringBuilder ();
                sb.Append ("make(");
                var pieces = new List<string> ();
                var structInstance = obj.StructInstanceValue;
                var ztruct = structInstance.Struct;
                pieces.Add (StructAsString (ztruct));
                for (var i = 0; i < structInstance.Values.Length; i++) {
                    pieces.Add (ShovelStringRepresentationImpl (api, structInstance.Values [i], visited));
                }
                sb.Append (String.Join (", ", pieces));
                sb.Append (")");
                return sb.ToString ();
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

