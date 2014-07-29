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

            // Format
            AddPrim0 (result, Callable.MakePrim0 ("format", Format, null));

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
            result.values = new Value[ztruct.Fields.Length];
            var hash = args [start + 1].hashValue;
            var sizeIncrease = 1 + ztruct.Fields.Length;
            api.CellsIncrementHerald (sizeIncrease);
            for (int i = 0; i < ztruct.Fields.Length; i++) {
                var svKey = Value.Make (ztruct.Fields [i]);
                if (hash.ContainsKey (svKey)) {
                    result.values [i] = hash [svKey];
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
            var structInstance = args [start].structInstanceValue;
            var ztruct = structInstance.Struct;
            var sizeIncrease = 1 + 2 * ztruct.Fields.Length;
            api.CellsIncrementHerald (sizeIncrease);
            for (int i = 0; i < ztruct.Fields.Length; i++) {
                result [Value.Make (ztruct.Fields [i])] = structInstance.values [i];
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
            result.values = new Value[ztruct.Fields.Length];
            for (int i = 1; i < length; i++) {
                result.values [i - 1] = args [start + i];
            }
            api.CellsIncrementer (sizeIncrease);
            return Value.Make (result);
        }

        static Value Defstruct (VmApi api, Value[] args, int start, int length)
        {
            if (args [start].Kind != Value.Kinds.Array) {
                api.RaiseShovelError ("Argument must be an array of strings.");
            }
            var fieldNames = args [start].arrayValue;
            var sizeIncrease = 1 + length;
            api.CellsIncrementHerald (sizeIncrease);
            Struct newStruct = new Struct ();
            newStruct.Fields = new string[fieldNames.Count];
            for (int i = 0; i < newStruct.Fields.Length; i++) {
                if (fieldNames [i].Kind != Value.Kinds.String) {
                    api.RaiseShovelError ("Argument must be an array of strings.");
                }
                newStruct.Fields [i] = fieldNames [i].stringValue;
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
                    t1.integerValue = t1.integerValue + t2.integerValue;
                    break;
                case Value.Kinds.Double:
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.integerValue + t2.doubleValue;
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
                    t1.doubleValue = t1.doubleValue + t2.integerValue;
                    break;
                case Value.Kinds.Double:
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.doubleValue + t2.doubleValue;
                    break;
                default:
                    AddError (api);
                    break;
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    t1.Kind = Value.Kinds.String;
                    t1.stringValue = t1.stringValue + t2.stringValue;
                } else {
                    AddError (api);
                }
                break;
            case Value.Kinds.Array:
                if (t2.Kind == Value.Kinds.Array) {
                    var result = new ArrayInstance ();
                    result.AddRange (t1.arrayValue);
                    result.AddRange (t2.arrayValue);
                    t1.Kind = Value.Kinds.Array;
                    t1.arrayValue = result;
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
                    t1.integerValue = t1.integerValue - t2.integerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.integerValue - t2.doubleValue;
                } else {
                    BothNumbersError (api);
                }
            } else if (t1.Kind == Value.Kinds.Double) {
                if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.doubleValue - t2.doubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.doubleValue - t2.integerValue;
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
                t1.integerValue = -t1.integerValue;
            } else if (t1.Kind == Value.Kinds.Double) {
                t1.doubleValue = -t1.doubleValue;
            } else {
                api.RaiseShovelError ("Argument must be number.");
            }
        }

        internal static void Multiply (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer) {
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Integer;
                    t1.integerValue = t1.integerValue * t2.integerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.integerValue * t2.doubleValue;
                } else {
                    BothNumbersError (api);
                }
            } else if (t1.Kind == Value.Kinds.Double) {
                if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.doubleValue * t2.doubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.doubleValue * t2.integerValue;
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
                    t1.integerValue = t1.integerValue / t2.integerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.integerValue / t2.doubleValue;
                } else {
                    BothNumbersError (api);
                }
            } else if (t1.Kind == Value.Kinds.Double) {
                if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.doubleValue / t2.doubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = t1.doubleValue / t2.integerValue;
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
                t1.integerValue = t1.integerValue << (int)t2.integerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        internal static void ShiftRight (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.integerValue = t1.integerValue >> (int)t2.integerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        internal static void Modulo (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.integerValue = t1.integerValue % t2.integerValue;
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
                    t1.integerValue = Expt (t1.integerValue, t2.integerValue);
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = Math.Pow (t1.integerValue, t2.doubleValue);
                } else {
                    BothNumbersError (api);
                }
            } else if (t1.Kind == Value.Kinds.Double) {
                if (t2.Kind == Value.Kinds.Double) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = Math.Pow (t1.doubleValue, t2.doubleValue);
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.Kind = Value.Kinds.Double;
                    t1.doubleValue = Math.Pow (t1.doubleValue, t2.integerValue);
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
                t1.integerValue = (long)Math.Floor (t1.doubleValue);
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
                t1.hashValue.Remove (t2);
            }
        }

        internal static void LessThan (VmApi api, ref Value t1, ref Value t2)
        {
            switch (t1.Kind) {
            case Value.Kinds.Integer:
                if (t2.Kind == Value.Kinds.Integer) {
                    t1.boolValue = t1.integerValue < t2.integerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.boolValue = t1.integerValue < t2.doubleValue;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    t1.boolValue = CompareStrings (t1.stringValue, t2.stringValue) == -1;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.boolValue = t1.doubleValue < t2.doubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.boolValue = t1.doubleValue < t2.integerValue;
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
                    t1.boolValue = t1.integerValue <= t2.integerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.boolValue = t1.integerValue <= t2.doubleValue;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    var comparison = CompareStrings (t1.stringValue, t2.stringValue);
                    t1.boolValue = comparison == -1 || comparison == 0;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.boolValue = t1.doubleValue <= t2.doubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.boolValue = t1.doubleValue <= t2.integerValue;
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
                    t1.boolValue = t1.integerValue > t2.integerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.boolValue = t1.integerValue > t2.doubleValue;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    var comparison = CompareStrings (t1.stringValue, t2.stringValue);
                    t1.boolValue = comparison == 1;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.boolValue = t1.doubleValue > t2.doubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.boolValue = t1.doubleValue > t2.integerValue;
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
                    t1.boolValue = t1.integerValue >= t2.integerValue;
                } else if (t2.Kind == Value.Kinds.Double) {
                    t1.boolValue = t1.integerValue >= t2.doubleValue;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    var comparison = CompareStrings (t1.stringValue, t2.stringValue);
                    t1.boolValue = comparison == 1 || comparison == 0;
                } else {
                    RelationalError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.boolValue = t1.doubleValue >= t2.doubleValue;
                } else if (t2.Kind == Value.Kinds.Integer) {
                    t1.boolValue = t1.doubleValue >= t2.integerValue;
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
                    t1.boolValue = t1.integerValue == t2.integerValue;
                } else if (t2.Kind == Value.Kinds.Null) {
                    t1.boolValue = false;
                } else {
                    AreEqualError (api);
                }
                break;
            case Value.Kinds.String:
                if (t2.Kind == Value.Kinds.String) {
                    int comparison = CompareStrings (t1.stringValue, t2.stringValue);
                    t1.boolValue = comparison == 0;
                } else if (t2.Kind == Value.Kinds.Null) {
                    t1.boolValue = false;
                } else {
                    AreEqualError (api);
                }
                break;
            case Value.Kinds.Double:
                if (t2.Kind == Value.Kinds.Double) {
                    t1.boolValue = t1.doubleValue == t2.doubleValue;
                } else if (t2.Kind == Value.Kinds.Null) {
                    t1.boolValue = false;
                } else {
                    AreEqualError (api);
                }
                break;
            case Value.Kinds.Bool:
                if (t2.Kind == Value.Kinds.Bool) {
                    t1.boolValue = t1.boolValue == t2.boolValue;
                } else if (t2.Kind == Value.Kinds.Null) {
                    t1.boolValue = false;
                } else {
                    AreEqualError (api);
                }
                break;
            default:
                if (t1.Kind == Value.Kinds.Null && t2.Kind == Value.Kinds.Null) {
                    t1.boolValue = true;
                } else if (t1.Kind == Value.Kinds.Null || t2.Kind == Value.Kinds.Null) {
                    t2.boolValue = false;
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
            t1.boolValue = !t1.boolValue;
        }

        internal static void LogicalNot (VmApi api, ref Value argument)
        {
            if (argument.Kind == Value.Kinds.Bool) {
                argument.boolValue = !argument.boolValue;
            } else {
                api.RaiseShovelError ("Argument must be boolean.");
            }
        }

        internal static void BitwiseAnd (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.integerValue = t1.integerValue & t2.integerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        internal static void BitwiseOr (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.integerValue = t1.integerValue | t2.integerValue;
            } else {
                api.RaiseShovelError ("Both arguments must be integers.");
            }
        }

        internal static void BitwiseXor (VmApi api, ref Value t1, ref Value t2)
        {
            if (t1.Kind == Value.Kinds.Integer && t2.Kind == Value.Kinds.Integer) {
                t1.integerValue = t1.integerValue ^ t2.integerValue;
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
            hash.boolValue = hash.hashValue.ContainsKey (key);
            hash.Kind = Value.Kinds.Bool;
        }

        internal static void Keys (VmApi api, ref Value hash)
        {
            if (hash.Kind != Value.Kinds.Hash) {
                api.RaiseShovelError ("First argument must be a hash table.");
            }
            var result = new ArrayInstance ();
            var sizeIncrease = 1 + hash.hashValue.Count;
            api.CellsIncrementHerald (sizeIncrease);
            result.AddRange (hash.hashValue.Keys);
            hash.arrayValue = result;
            hash.Kind = Value.Kinds.Array;
            api.CellsIncrementer (sizeIncrease);
        }

        static Value Format(VmApi api, Value[] args, int start, int length)
        {
            CheckString(api, args[start]);
            var formatArgs = new List<object>();
            for (int i = 1; i < length; i++) {
                var arg = args[i + start];
                switch(arg.Kind) {
                    case Value.Kinds.Array:
                        formatArgs.Add(ShovelStringRepresentation(api, arg));
                        break;
                    case Value.Kinds.Bool: 
                        formatArgs.Add(arg.boolValue); 
                        break;
                    case Value.Kinds.Callable:
                        api.RaiseShovelError("Cannot call 'format' on callables.");
                        break;
                    case Value.Kinds.Double:
                        formatArgs.Add(arg.doubleValue); 
                        break;
                    case Value.Kinds.Hash:
                        formatArgs.Add(ShovelStringRepresentation(api, arg));
                        break;
                    case Value.Kinds.Integer:
                        formatArgs.Add(arg.integerValue); 
                        break;
                    case Value.Kinds.NamedBlock:
                        api.RaiseShovelError("Cannot call 'format' on named blocks.");
                        break;
                    case Value.Kinds.Null:
                        formatArgs.Add(ShovelStringRepresentation(api, arg));
                        break;
                    case Value.Kinds.ReturnAddress:
                        api.RaiseShovelError("Cannot call 'format' on return addresses.");
                        break;
                    case Value.Kinds.String:
                        formatArgs.Add(arg.stringValue);
                        break;
                    case Value.Kinds.Struct:
                        formatArgs.Add(ShovelStringRepresentation(api, arg));
                        break;
                    case Value.Kinds.StructInstance:
                        formatArgs.Add(ShovelStringRepresentation(api, arg));
                        break;
                    default:
                        api.RaiseShovelError(String.Format(
                            "'format' called with unknown value ('{0}').", arg.Kind.ToString()));
                        break;
                }
            }
            api.CellsIncrementer(1);
            return Value.Make(String.Format(args[start].stringValue, formatArgs.ToArray()));
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
            var sizeIncrease = 1 + (int)size.integerValue;
            api.CellsIncrementHerald (sizeIncrease);
            for (var i = 0; i < size.integerValue; i++) {
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
            array.arrayValue.Add (value);
            array = value;
            api.CellsIncrementer (1);
        }

        internal static void ArrayPop (VmApi api, ref Value array)
        {
            CheckVector (api, ref array);
            var vector = array.arrayValue;
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
                    var idx = (int)index.integerValue;
                    if (idx < 0 || idx >= arrayOrHashOrString.arrayValue.Count)
                    {
                        if (arrayOrHashOrString.arrayValue.IndirectGet.Kind == Value.Kinds.Callable)
                        {
                            return false;
                        }
                        else { 
                            api.RaiseShovelError("Index out of range.");
                        }
                    }
                    arrayOrHashOrString = arrayOrHashOrString.arrayValue [idx];
                } else {
                    api.RaiseShovelError ("Getting an array element requires an integer index.");
                }
            } else if (arrayOrHashOrString.Kind == Value.Kinds.Hash) {
                if (index.Kind == Value.Kinds.String) {
                    if (!arrayOrHashOrString.hashValue.ContainsKey (index)) {
                        if (arrayOrHashOrString.hashValue.IndirectGet.Kind == Value.Kinds.Callable) { 
                            return false;
                        }
                        else
                        {
                            api.RaiseShovelError(String.Format("Key '{0}' not found.", index.stringValue));
                        }
                    } 
                    arrayOrHashOrString = arrayOrHashOrString.hashValue [index];
                } else {
                    api.RaiseShovelError ("Getting a hash table value requires a key that is a string.");
                }
            } else if (arrayOrHashOrString.Kind == Value.Kinds.String) {
                if (index.Kind == Value.Kinds.Integer) {
                    var idx = (int)index.integerValue;
                    if (idx < 0 || idx >= arrayOrHashOrString.stringValue.Length)
                    {
                        api.RaiseShovelError("Index out of range.");
                    }
                    arrayOrHashOrString = Value.Make (
                        arrayOrHashOrString.stringValue [idx].ToString ());
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
                var structInstance = obj.structInstanceValue;
                var ztruct = structInstance.Struct;
                if (cache != null) {
                    var info = (Tuple<Struct, int>)cache;
                    if (info.Item1 == ztruct) {
                        obj = structInstance.values [info.Item2];
                        return true;
                    }
                }
                int location = FindLocationInStruct (api, ztruct, index.stringValue);
                obj = structInstance.values [location];
                vm.SetCurrentCache (Tuple.Create (ztruct, location));
            } else if (obj.Kind == Value.Kinds.Hash) {
                if (!obj.hashValue.ContainsKey (index)) {
                    if (obj.hashValue.IndirectGet.Kind == Value.Kinds.Callable) { 
                        return false;
                    }
                    else {
                        api.RaiseShovelError("Key not found in hash table.");
                    }
                }
                obj = obj.hashValue [index];
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
                var structInstance = obj.structInstanceValue;
                var ztruct = structInstance.Struct;
                if (cache != null) {
                    var info = (Tuple<Struct, int>)cache;
                    if (info.Item1 == ztruct) {
                        structInstance.values [info.Item2] = value;
                        return true;
                    }
                }
                int location = FindLocationInStruct (api, ztruct, index.stringValue);
                structInstance.values [location] = value;
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
                if (!obj.hashValue.ContainsKey(index))
                {
                    var hasSetter = obj.hashValue.IndirectSet.Kind == Value.Kinds.Callable;
                    if (hasSetter)
                    {
                        return false;
                    }
                }
                obj.hashValue[index] = value;
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
                    var idx = (int)index.integerValue;
                    if (idx < 0 || idx >= obj.arrayValue.Count)
                    {
                        if (obj.arrayValue.IndirectSet.Kind == Value.Kinds.Callable)
                        {
                            return false;
                        }
                        else { 
                            api.RaiseShovelError("Index out of range.");
                        }
                    }
                    obj.arrayValue [idx] = value;
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
                arrayOrString.integerValue = arrayOrString.arrayValue.Count;
            } else if (arrayOrString.Kind == Value.Kinds.String) {
                arrayOrString.Kind = Value.Kinds.Integer;
                arrayOrString.integerValue = (long)arrayOrString.stringValue.Length;
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
            if (getter.Kind != Value.Kinds.Null && (getter.Kind != Value.Kinds.Callable || getter.callableValue.Arity != 2))
            {
                api.RaiseShovelError("The second parameter (getter) should be a callable with 2 parameters.");
                throw new InvalidOperationException();
            }
            if (setter.Kind != Value.Kinds.Null && (setter.Kind != Value.Kinds.Callable || setter.callableValue.Arity != 3))
            {
                api.RaiseShovelError("The third parameter (setter) should be a callable with 3 parameters.");
                throw new InvalidOperationException();
            }
            if (arrayOrHash.Kind == Value.Kinds.Array)
            {
                var array = arrayOrHash.arrayValue;
                array.IndirectGet = getter;
                array.IndirectSet = setter;
            }
            else if (arrayOrHash.Kind == Value.Kinds.Hash)
            {
                var hash = arrayOrHash.hashValue;
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
                var length = arrayOrString.arrayValue.Count;
                var realStart = (int)start.integerValue;
                var realEnd = (int)end.integerValue;
                AdjustRealStartEnd (api, ref realStart, ref realEnd, length);
                api.CellsIncrementer (realEnd - realStart);
                return Value.Make (
                    arrayOrString.arrayValue.GetRange2 (realStart, realEnd - realStart));
            } else if (arrayOrString.Kind == Value.Kinds.String) {
                var length = arrayOrString.stringValue.Length;
                var realStart = (int)start.integerValue;
                var realEnd = (int)end.integerValue;
                AdjustRealStartEnd (api, ref realStart, ref realEnd, length);
                api.CellsIncrementer (realEnd - realStart);
                return Value.Make (
                    arrayOrString.stringValue.Substring (realStart, realEnd - realStart));
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
            api.CellsIncrementer (str.stringValue.Length);
            api.CellsIncrementer (str.stringValue.Length);
            return Value.Make (str.stringValue.ToUpper ());
        }

        static Value StringLower (VmApi api, Value str)
        {
            CheckString (api, str);
            api.CellsIncrementer (str.stringValue.Length);
            api.CellsIncrementer (str.stringValue.Length);
            return Value.Make (str.stringValue.ToLower ());
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
            var epochTimeSpan = TimeSpan.FromSeconds (timeInSeconds.integerValue);
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
            if (!hash.hashValue.ContainsKey (shKey)) {
                api.RaiseShovelError (String.Format (errorMessageFormat, "NIL"));
            }
            var value = hash.hashValue [shKey];
            var isValid = (value.Kind == Value.Kinds.Integer) 
                && (value.integerValue >= min) 
                && (value.integerValue <= max);
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
                (int)hash.hashValue [Value.Make ("year")].integerValue, 
                (int)hash.hashValue [Value.Make ("month")].integerValue, 
                (int)hash.hashValue [Value.Make ("day")].integerValue, 
                (int)hash.hashValue [Value.Make ("hour")].integerValue, 
                (int)hash.hashValue [Value.Make ("minute")].integerValue, 
                (int)hash.hashValue [Value.Make ("second")].integerValue);
            return Value.MakeInt ((long)(date - unixEpoch).TotalSeconds);
        }

        internal static void IsString (VmApi api, ref Value obj)
        {
            obj.boolValue = obj.Kind == Value.Kinds.String;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsHash (VmApi api, ref Value obj)
        {
            obj.boolValue = obj.Kind == Value.Kinds.Hash;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsBool (VmApi api, ref Value obj)
        {
            obj.boolValue = obj.Kind == Value.Kinds.Bool;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsArray (VmApi api, ref Value obj)
        {
            obj.boolValue = obj.Kind == Value.Kinds.Array;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsNumber (VmApi api, ref Value obj)
        {
            obj.boolValue = 
                obj.Kind == Value.Kinds.Integer || obj.Kind == Value.Kinds.Double;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsInteger (VmApi api, ref Value obj)
        {
            obj.boolValue = obj.Kind == Value.Kinds.Integer;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsCallable (VmApi api, ref Value obj)
        {
            obj.boolValue = obj.Kind == Value.Kinds.Callable;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsStruct (VmApi api, ref Value obj)
        {
            obj.boolValue = obj.Kind == Value.Kinds.Struct;
            obj.Kind = Value.Kinds.Bool;
        }

        internal static void IsStructInstance (VmApi api, ref Value obj, ref Value str)
        {
            if (str.Kind != Value.Kinds.Struct) {
                api.RaiseShovelError ("Second argument must be a struct.");
            }
            var result = obj.Kind == Value.Kinds.StructInstance && obj.structInstanceValue.Struct == str.StructValue;
            obj.boolValue = result;
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
                return obj.stringValue;
            } else if (obj.Kind == Value.Kinds.Array) {
                return "[...array...]";
            } else if (obj.Kind == Value.Kinds.Integer) {
                return obj.integerValue.ToString (CultureInfo.InvariantCulture);
            } else if (obj.Kind == Value.Kinds.Double) {
                return obj.doubleValue.ToString (CultureInfo.InvariantCulture);
            } else if (obj.Kind == Value.Kinds.Hash) {
                return "[...hash...]";
            } else if (obj.Kind == Value.Kinds.Callable) {
                return "[...callable...]";
            } else if (obj.Kind == Value.Kinds.Struct) {
                return "[...struct...]";
            } else if (obj.Kind == Value.Kinds.StructInstance) {
                return "[...struct instance...]";
            } else if (obj.Kind == Value.Kinds.Bool) {
                return obj.boolValue.ToString ().ToLower ();
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
            api.CellsIncrementer (result.stringValue.Length);
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
                return String.Format ("\"{0}\"", obj.stringValue.Replace ("\"", "\\\""));
            } else if (obj.Kind == Value.Kinds.Array) {
                visited.Add (obj);
                var stringReps = obj.arrayValue
                    .Select (elem => ShovelStringRepresentationImpl (api, elem, visited));
                return String.Format ("array({0})", String.Join (", ", stringReps));
            } else if (obj.Kind == Value.Kinds.Hash) {
                visited.Add (obj);
                var stringReps = new List<string> ();
                foreach (var key in obj.hashValue.Keys) {
                    stringReps.Add ((string)ShovelStringRepresentationImpl (api, key, visited));
                    stringReps.Add ((string)ShovelStringRepresentationImpl (api, obj.hashValue [key], visited));
                }
                return String.Format ("hash({0})", String.Join (", ", stringReps));
            } else if (obj.Kind == Value.Kinds.Struct) {
                return StructAsString (obj.StructValue);
            } else if (obj.Kind == Value.Kinds.StructInstance) {
                visited.Add (obj);
                var sb = new StringBuilder ();
                sb.Append ("make(");
                var pieces = new List<string> ();
                var structInstance = obj.structInstanceValue;
                var ztruct = structInstance.Struct;
                pieces.Add (StructAsString (ztruct));
                for (var i = 0; i < structInstance.values.Length; i++) {
                    pieces.Add (ShovelStringRepresentationImpl (api, structInstance.values [i], visited));
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
            return Value.MakeInt (long.Parse (str.stringValue));
        }

        static Value ParseFloat (VmApi api, Value str)
        {
            CheckString (api, str);
            return Value.MakeFloat (double.Parse (str.stringValue));
        }

        static Value Panic (VmApi api, Value str)
        {
            CheckString (api, str);
            api.RaiseShovelError (str.stringValue);
            throw new InvalidOperationException ();
        }

    }
}

