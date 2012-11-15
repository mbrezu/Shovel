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

			// Arithmetic operators.
			AddPrim0 (result, Callable.MakePrim0 ("+", Callable.MakeHostCallable (Add)));
			AddPrim0 (result, Callable.MakePrim0 ("-", Callable.MakeHostCallable (Subtract)));
			AddPrim0 (result, Callable.MakePrim0 ("unary-minus", Callable.MakeHostCallable (UnaryMinus), 1));
			AddPrim0 (result, Callable.MakePrim0 ("*", Callable.MakeHostCallable (Multiply)));
			AddPrim0 (result, Callable.MakePrim0 ("/", Callable.MakeHostCallable (Divide)));
			AddPrim0 (result, Callable.MakePrim0 ("<<", Callable.MakeHostCallable (ShiftLeft)));
			AddPrim0 (result, Callable.MakePrim0 (">>", Callable.MakeHostCallable (ShiftRight)));
			AddPrim0 (result, Callable.MakePrim0 ("%", Callable.MakeHostCallable (Modulo)));
			AddPrim0 (result, Callable.MakePrim0 ("pow", Callable.MakeHostCallable (Pow)));
			AddPrim0 (result, Callable.MakePrim0 ("floor", Callable.MakeHostCallable (Floor), 1));

			// Relational operators.
			AddPrim0 (result, Callable.MakePrim0 ("<", Callable.MakeHostCallable (LessThan)));
			AddPrim0 (result, Callable.MakePrim0 ("<=", Callable.MakeHostCallable (LessThanOrEqual)));
			AddPrim0 (result, Callable.MakePrim0 (">", Callable.MakeHostCallable (GreaterThan)));
			AddPrim0 (result, Callable.MakePrim0 (">=", Callable.MakeHostCallable (GreaterThanOrEqual)));
			AddPrim0 (result, Callable.MakePrim0 ("==", Callable.MakeHostCallable (AreEqual)));
			AddPrim0 (result, Callable.MakePrim0 ("!=", Callable.MakeHostCallable (AreNotEqual)));

			// Logic operators.
			AddPrim0 (result, Callable.MakePrim0 ("!", Callable.MakeHostCallable (LogicalNot), 1));

			// Bitwise operators.
			AddPrim0 (result, Callable.MakePrim0 ("&", Callable.MakeHostCallable (BitwiseAnd)));
			AddPrim0 (result, Callable.MakePrim0 ("|", Callable.MakeHostCallable (BitwiseOr)));
			AddPrim0 (result, Callable.MakePrim0 ("^", Callable.MakeHostCallable (BitwiseXor)));

			// Hash constructor.
			AddPrim0 (result, Callable.MakePrim0 ("hash", HashConstructor, null));

			// Hash table has key?
			AddPrim0 (result, Callable.MakePrim0 ("hasKey", Callable.MakeHostCallable (HasKey)));

			// Keys for hash table.
			AddPrim0 (result, Callable.MakePrim0 ("keys", Callable.MakeHostCallable (Keys), 1));

			// Array constructors.
			AddPrim0 (result, Callable.MakePrim0 ("array", ArrayConstructor, null));
			AddPrim0 (result, Callable.MakePrim0 ("arrayN", Callable.MakeHostCallable (SizedArrayConstructor), 1));

			// Array push and pop.
			AddPrim0 (result, Callable.MakePrim0 ("push", Callable.MakeHostCallable (ArrayPush)));
			AddPrim0 (result, Callable.MakePrim0 ("pop", Callable.MakeHostCallable (ArrayPop), 1));

			// Array and hash set and get.
			AddPrim0 (result, Callable.MakePrim0 ("svm_gref", Callable.MakeHostCallable (ArrayOrHashGet)));
			AddPrim0 (result, Callable.MakePrim0 ("svm_gref_dot", Callable.MakeHostCallable (HashGetDot)));
			AddPrim0 (result, Callable.MakePrim0 ("svm_set_indexed", Callable.MakeHostCallable (ArrayOrHashSet), 3));

			// String or array length.
			AddPrim0 (result, Callable.MakePrim0 ("length", Callable.MakeHostCallable (GetLength), 1));

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

			// Object types.
			AddPrim0 (result, Callable.MakePrim0 ("isString", Callable.MakeHostCallable (IsString), 1));
			AddPrim0 (result, Callable.MakePrim0 ("isHash", Callable.MakeHostCallable (IsHash), 1));
			AddPrim0 (result, Callable.MakePrim0 ("isBool", Callable.MakeHostCallable (IsBool), 1));
			AddPrim0 (result, Callable.MakePrim0 ("isArray", Callable.MakeHostCallable (IsArray), 1));
			AddPrim0 (result, Callable.MakePrim0 ("isNumber", Callable.MakeHostCallable (IsNumber), 1));
			AddPrim0 (result, Callable.MakePrim0 ("isInteger", Callable.MakeHostCallable (IsInteger), 1));
			AddPrim0 (result, Callable.MakePrim0 ("isCallable", Callable.MakeHostCallable (IsCallable), 1));

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

		static object Add (VmApi api, object t1, object t2)
		{
			if (t1 is string && t2 is string) {
				return (string)t1 + (string)t2;
			} else if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 + (long)t2);
			} else if (t1 is double && t2 is double) {
				return (double)t1 + (double)t2;
			} else if (t1 is double && t2 is long) {
				return (double)t1 + (long)t2;
			} else if (t1 is long && t2 is Double) {
				return (long)t1 + (double)t2;
			} else if (t1 is List<object> && t2 is List<object>) {
				var result = new List<object> ();
				result.AddRange ((List<object>)t1);
				result.AddRange ((List<object>)t2);
				return result;
			} else {
				api.RaiseShovelError (
					"Arguments must have the same type (numbers or strings or arrays).");
			}
			return null;
		}

		const long limitMask = ((long)1 << 60) - 1;

		static object LimitResult (object obj)
		{
			if (obj is long) {
				return (long)obj & limitMask;
			} else {
				return obj;
			}
		}

		static object Subtract (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 - (long)t2);
			} else if (t1 is double && t2 is double) {
				return (double)t1 - (double)t2;
			} else if (t1 is double && t2 is long) {
				return (double)t1 - (long)t2;
			} else if (t1 is long && t2 is Double) {
				return (long)t1 - (double)t2;
			} else {
				api.RaiseShovelError ("Both arguments must be numbers.");
			}
			return null;
		}

		static object UnaryMinus (VmApi api, object t1)
		{
			if (t1 is long) {
				return -(long)t1;
			} else if (t1 is Double) {
				return -(double)t1;
			} else {
				api.RaiseShovelError ("Argument must be number.");
			}
			return null;
		}

		static object Multiply (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 * (long)t2);
			} else if (t1 is double && t2 is double) {
				return (double)t1 * (double)t2;
			} else if (t1 is double && t2 is long) {
				return (double)t1 * (long)t2;
			} else if (t1 is long && t2 is Double) {
				return (long)t1 * (double)t2;
			} else {
				api.RaiseShovelError ("Both arguments must be numbers.");
			}
			return null;
		}

		static object Divide (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 / (long)t2);
			} else if (t1 is double && t2 is double) {
				return (double)t1 / (double)t2;
			} else if (t1 is double && t2 is long) {
				return (double)t1 / (long)t2;
			} else if (t1 is long && t2 is Double) {
				return (long)t1 / (double)t2;
			} else {
				api.RaiseShovelError ("Both arguments must be numbers.");
			}
			return null;
		}

		static object ShiftLeft (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 << (int)(long)t2);
			} else {
				api.RaiseShovelError ("Both arguments must be integers.");
			}
			return null;
		}

		static object ShiftRight (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 >> (int)(long)t2);
			} else {
				api.RaiseShovelError ("Both arguments must be integers.");
			}
			return null;
		}

		static object Modulo (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 % (long)t2);
			} else {
				api.RaiseShovelError ("Both arguments must be integers.");
			}
			return null;
		}

		static long Expt (long b, long e)
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

		static object Pow (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult (Expt ((long)t1, (long)t2));
			} else if (t1 is double && t2 is double) {
				return Math.Pow ((double)t1, (double)t2);
			} else if (t1 is double && t2 is long) {
				return Math.Pow ((double)t1, (long)t2);
			} else if (t1 is long && t2 is Double) {
				return Math.Pow ((long)t1, (double)t2);
			} else {
				api.RaiseShovelError ("Both arguments must be numbers.");
			}
			return null;
		}

		static object Floor (VmApi api, object t1)
		{
			if (t1 is double) {
				return (long)Math.Floor ((double)t1);
			} else if (t1 is long) {
				return t1;
			} else {
				api.RaiseShovelError ("Argument must be number.");
			}
			return null;
		}

		static object LessThan (VmApi api, object t1, object t2)
		{
			if (t1 is string && t2 is string) {
				return ((string)t1).CompareTo ((string)t2) == -1;
			} else if (t1 is long && t2 is long) {
				return (long)t1 < (long)t2;
			} else if (t1 is double && t2 is double) {
				return (double)t1 < (double)t2;
			} else if (t1 is double && t2 is long) {
				return (double)t1 < (long)t2;
			} else if (t1 is long && t2 is Double) {
				return (long)t1 < (double)t2;
			} else {
				api.RaiseShovelError (
					"Arguments must have the same type (numbers or strings).");
			}
			return null;
		}

		static object LessThanOrEqual (VmApi api, object t1, object t2)
		{
			if (t1 is string && t2 is string) {
				var comparison = ((string)t1).CompareTo ((string)t2);
				return comparison == -1 || comparison == 0;
			} else if (t1 is long && t2 is long) {
				return (long)t1 <= (long)t2;
			} else if (t1 is double && t2 is double) {
				return (double)t1 <= (double)t2;
			} else if (t1 is double && t2 is long) {
				return (double)t1 <= (long)t2;
			} else if (t1 is long && t2 is Double) {
				return (long)t1 <= (double)t2;
			} else {
				api.RaiseShovelError (
					"Arguments must have the same type (numbers or strings).");
			}
			return null;
		}

		static object GreaterThan (VmApi api, object t1, object t2)
		{
			if (t1 is string && t2 is string) {
				return ((string)t1).CompareTo ((string)t2) == 1;
			} else if (t1 is long && t2 is long) {
				return (long)t1 > (long)t2;
			} else if (t1 is double && t2 is double) {
				return (double)t1 > (double)t2;
			} else if (t1 is double && t2 is long) {
				return (double)t1 > (long)t2;
			} else if (t1 is long && t2 is Double) {
				return (long)t1 > (double)t2;
			} else {
				api.RaiseShovelError (
					"Arguments must have the same type (numbers or strings).");
			}
			return null;
		}

		static object GreaterThanOrEqual (VmApi api, object t1, object t2)
		{
			if (t1 is string && t2 is string) {
				var comparison = ((string)t1).CompareTo ((string)t2);
				return comparison == 1 || comparison == 0;
			} else if (t1 is long && t2 is long) {
				return (long)t1 >= (long)t2;
			} else if (t1 is double && t2 is double) {
				return (double)t1 >= (double)t2;
			} else if (t1 is double && t2 is long) {
				return (double)t1 >= (long)t2;
			} else if (t1 is long && t2 is Double) {
				return (long)t1 >= (double)t2;
			} else {
				api.RaiseShovelError (
					"Arguments must have the same type (numbers or strings).");
			}
			return null;
		}

		static object AreEqual (VmApi api, object t1, object t2)
		{
			if (t1 is string && t2 is string) {
				var comparison = ((string)t1).CompareTo ((string)t2);
				return comparison == 0;
			} else if (t1 is long && t2 is long) {
				return (long)t1 == (long)t2;
			} else if (t1 is double && t2 is double) {
				return (double)t1 == (double)t2;
			} else if (t1 is double && t2 is long) {
				return (double)t1 == (long)t2;
			} else if (t1 is long && t2 is Double) {
				return (long)t1 == (double)t2;
			} else {
				api.RaiseShovelError (
					"Arguments must have the same type (numbers or strings).");
			}
			return null;
		}

		static object AreNotEqual (VmApi api, object t1, object t2)
		{
			return !(bool)AreEqual (api, t1, t2);
		}

		internal static object LogicalNot (VmApi api, object argument)
		{
			if (argument is bool) {
				return !(bool)argument;
			} else {
				api.RaiseShovelError ("Argument must be boolean.");
				return false;
			}
		}

		static object BitwiseAnd (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 & (long)t2);
			} else {
				api.RaiseShovelError ("Both arguments must be integers.");
			}
			return null;
		}

		static object BitwiseOr (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 | (long)t2);
			} else {
				api.RaiseShovelError ("Both arguments must be integers.");
			}
			return null;
		}

		static object BitwiseXor (VmApi api, object t1, object t2)
		{
			if (t1 is long && t2 is long) {
				return Prim0.LimitResult ((long)t1 ^ (long)t2);
			} else {
				api.RaiseShovelError ("Both arguments must be integers.");
			}
			return null;
		}

		static object HashConstructor (VmApi api, object[] args)
		{
			if (args.Length % 2 != 0) {
				api.RaiseShovelError ("Must provide an even number of arguments.");
			}
			var sizeIncrease = 1 + 2 * args.Length;
			api.CellsIncrementHerald (sizeIncrease);
			var result = new Dictionary<string, object> ();
			for (var i = 0; i < args.Length; i += 2) {
				if (args [i] is String) {
					result [(string)args [i]] = args [i + 1];
				} else {
					api.RaiseShovelError ("Keys must be strings");
				}
			}
			api.CellsIncrementer (sizeIncrease);
			return result;
		}

		static object HasKey (VmApi api, object hash, object key)
		{
			if (!(hash is Dictionary<string, object>)) {
				api.RaiseShovelError ("First argument must be a hash table.");
			}
			if (!(key is string)) {
				api.RaiseShovelError ("Second argument must be a string.");
			}
			return ((Dictionary<string, object>)hash).ContainsKey ((string)key);
		}

		static object Keys (VmApi api, object hash)
		{
			if (!(hash is Dictionary<string, object>)) {
				api.RaiseShovelError ("First argument must be a hash table.");
			}
			var result = new List<object> ();
			result.AddRange (((Dictionary<string, object>)hash).Keys);
			return result;
		}

		static object ArrayConstructor (VmApi api, object[] args)
		{
			var result = new List<object> ();
			result.AddRange (args);
			return result;
		}

		static object SizedArrayConstructor (VmApi api, object size)
		{
			if (!(size is long)) {
				api.RaiseShovelError ("Argument must be an integer.");
			}
			var result = new List<object> ();
			for (var i = 0; i < (long)size; i++) {
				result.Add (null);
			}
			return result;
		}

		static void CheckVector (VmApi api, object vector)
		{
			if (!(vector is List<object>)) {
				api.RaiseShovelError ("First argument must be a vector.");
			}
		}

		static object ArrayPush (VmApi api, object array, object value)
		{
			CheckVector (api, array);
			((List<object>)array).Add (value);
			return value;
		}

		static object ArrayPop (VmApi api, object array)
		{
			CheckVector (api, array);
			var vector = (List<object>)array;
			if (vector.Count == 0) {
				api.RaiseShovelError ("Can't pop from an empty array.");
			}
			var result = vector [vector.Count - 1];
			vector.RemoveAt (vector.Count - 1);
			return result;
		}

		static object ArrayOrHashGet (VmApi api, object arrayOrHashOrString, object index)
		{
			if (arrayOrHashOrString is List<object>) {
				if (index is long) {
					var array = (List<object>)arrayOrHashOrString;
					var idx = (int)(long)index;
					return array [idx];
				} else {
					api.RaiseShovelError ("Getting an array element requires an integer index.");
					return null;
				}
			} else if (arrayOrHashOrString is String) {
				if (index is long) {
					var array = (string)arrayOrHashOrString;
					var idx = (int)(long)index;
					return (array [idx]).ToString ();
				} else {
					api.RaiseShovelError ("Getting an string element requires an integer index.");
					return null;
				}
			} else if (arrayOrHashOrString is Dictionary<string, object>) {
				if (index is string) {
					var hash = (Dictionary<string, object>)arrayOrHashOrString;
					var key = (string)index;
					return hash [key];
				} else {
					api.RaiseShovelError ("Getting a hash table value requires a key that is a string.");
					return null;
				}
			} else {
				api.RaiseShovelError ("First argument must be a hash or an array or a string.");
				return null;
			}
		}

		static object HashGetDot (VmApi api, object hash, object index)
		{
			if (!(hash is Dictionary<string, object>)) {
				api.RaiseShovelError ("First argument must be a hash table.");
			}
			if (!(index is string)) {
				api.RaiseShovelError ("Second argument must be a string.");
			}
			var h = (Dictionary<string, object>)hash;
			var k = (string)index;
			if (!h.ContainsKey (k)) {
				api.RaiseShovelError ("Key not found in hash table.");
			}
			return h [k];
		}

		static object ArrayOrHashSet (VmApi api, object arrayOrHashOrString, object index, object value)
		{
			if (arrayOrHashOrString is List<object>) {
				if (index is long) {
					var array = (List<object>)arrayOrHashOrString;
					var idx = (int)(long)index;
					array [idx] = value;
					return value;
				} else {
					api.RaiseShovelError ("Setting an array element requires an integer index.");
					return null;
				}
			} else if (arrayOrHashOrString is Dictionary<string, object>) {
				if (index is string) {
					var hash = (Dictionary<string, object>)arrayOrHashOrString;
					var key = (string)index;
					hash [key] = value;
					return value;
				} else {
					api.RaiseShovelError ("Setting a hash table value requires a key that is a string.");
					return null;
				}
			} else {
				api.RaiseShovelError ("First argument must be a hash or an array.");
				return null;
			}
		}

		static object GetLength (VmApi api, object arrayOrString)
		{
			if (arrayOrString is List<object>) {
				return (long)((List<object>)arrayOrString).Count;
			} else if (arrayOrString is string) {
				return (long)((string)arrayOrString).Length;
			} else {
				api.RaiseShovelError ("Argument must be a string or an array.");
				return null;
			}
		}

		static void AdjustRealStartEnd (VmApi api, ref int realStart, ref int realEnd, int length)
		{
			if (realStart < 0) {
				realStart += length;
			}
			if (realEnd < 0) {
				realEnd += length;
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
			if (realEnd >= length) {
				api.RaiseShovelError (String.Format (
						"End index (~d) is equal to or larger than the length of the sequence (~d).",
						realStart, length)
				);
			}
		}

		static object GetSlice (VmApi api, object arrayOrString, object start, object end)
		{
			if (!(start is long)) {
				api.RaiseShovelError ("The start index must be an integer.");
				return null;
			}
			if (!(end is long)) {
				api.RaiseShovelError ("The end index must be an integer.");
				return null;
			}
			if (arrayOrString is List<object>) {
				var array = (List<object>)arrayOrString;
				var length = array.Count;
				var realStart = (int)(long)start;
				var realEnd = (int)(long)end;
				AdjustRealStartEnd (api, ref realStart, ref realEnd, length);
				return array.GetRange (realStart, realEnd - realStart + 1);
			} else if (arrayOrString is string) {
				var str = (string)arrayOrString;
				var length = str.Length;
				var realStart = (int)(long)start;
				var realEnd = (int)(long)end;
				AdjustRealStartEnd (api, ref realStart, ref realEnd, length);
				return str.Substring (realStart, realEnd - realStart + 1);
			} else {
				api.RaiseShovelError ("Argument must be a string or an array.");
				return null;
			}
		}

		static void CheckString (VmApi api, object str)
		{
			if (!(str is string)) {
				api.RaiseShovelError ("Argument must be a string.");
			}
		}

		static object StringUpper (VmApi api, object str)
		{
			CheckString (api, str);
			api.CellsIncrementer (((string)str).Length);
			return ((string)str).ToUpper ();
		}

		static object StringLower (VmApi api, object str)
		{
			CheckString (api, str);
			api.CellsIncrementer (((string)str).Length);
			return ((string)str).ToLower ();
		}

		static DateTime unixEpoch = new DateTime (1970, 1, 1);

		static object UtcSecondsSinceUnixEpoch (VmApi api)
		{
			return (long)(DateTime.UtcNow - unixEpoch).TotalSeconds;
		}

		static object DecodeTime (VmApi api, object timeInSeconds)
		{
			if (!(timeInSeconds is long)) {
				api.RaiseShovelError ("Argument must be an integer.");
			}
			var epochTimeSpan = TimeSpan.FromSeconds ((long)timeInSeconds);
			var date = Prim0.unixEpoch + epochTimeSpan;
			var result = new Dictionary<string, object> ();
			result ["year"] = (long)date.Year;
			result ["month"] = (long)date.Month;
			result ["day"] = (long)date.Day;
			switch (date.DayOfWeek) {
			case DayOfWeek.Monday:
				result ["dayOfWeek"] = (long)1;
				break;
			case DayOfWeek.Tuesday:
				result ["dayOfWeek"] = (long)2;
				break;
			case DayOfWeek.Wednesday:
				result ["dayOfWeek"] = (long)3;
				break;
			case DayOfWeek.Thursday:
				result ["dayOfWeek"] = (long)4;
				break;
			case DayOfWeek.Friday:
				result ["dayOfWeek"] = (long)5;
				break;
			case DayOfWeek.Saturday:
				result ["dayOfWeek"] = (long)6;
				break;
			case DayOfWeek.Sunday:
				result ["dayOfWeek"] = (long)7;
				break;
			}
			result ["hour"] = (long)date.Hour;
			result ["minute"] = (long)date.Minute;
			result ["second"] = (long)date.Second;
			return result;
		}

		static void CheckBoundedInteger (
			VmApi api, 
			Dictionary<string, object> hash, string key, 
			long min, long max, string errorMessageFormat)
		{
			if (!hash.ContainsKey (key)) {
				api.RaiseShovelError (String.Format (errorMessageFormat, "NIL"));
			}
			var value = hash [key];
			var isValid = (value is long) && ((long)value >= min) && ((long)value <= max);
			if (!isValid) {
				api.RaiseShovelError (String.Format (errorMessageFormat, value));
			}
		}

		static object EncodeTime (VmApi api, object timeHash)
		{
			if (!(timeHash is Dictionary<string, object>)) {
				api.RaiseShovelError ("Argument must be a valid time hash (as returned by 'decodeTime').");
			}
			var hash = (Dictionary<string, object>)timeHash;
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
				(int)(long)hash ["year"], (int)(long)hash ["month"], (int)(long)hash ["day"], 
				(int)(long)hash ["hour"], (int)(long)hash ["minute"], (int)(long)hash ["second"]);
			return (long)(date - unixEpoch).TotalSeconds;
		}

		static object IsString (VmApi api, object obj)
		{
			return obj is string;
		}

		static object IsHash (VmApi api, object obj)
		{
			return obj is Dictionary<string, object>;
		}

		static object IsBool (VmApi api, object obj)
		{
			return obj is bool;
		}

		static object IsArray (VmApi api, object obj)
		{
			return obj is List<object>;
		}

		static object IsNumber (VmApi api, object obj)
		{
			return (obj is long) || (obj is double);
		}

		static object IsInteger (VmApi api, object obj)
		{
			return obj is long;
		}

		static object IsCallable (VmApi api, object obj)
		{
			return obj is Callable;
		}

		static object ShovelString (VmApi api, object obj)
		{
			if (obj is String) {
				return obj;
			} else if (obj is List<object>) {
				return "[...array...]";
			} else if (obj is long) {
				return ((long)obj).ToString (CultureInfo.InvariantCulture);
			} else if (obj is double) {
				return ((double)obj).ToString (CultureInfo.InvariantCulture);
			} else if (obj is Dictionary<string, object>) {
				return "[...hash...]";
			} else if (obj is Callable) {
				return "[...callable...]";
			} else if (obj is bool) {
				return obj.ToString ().ToLower ();
			} else if (obj == null) {
				return "null";
			} else {
				UnknownTypeError (api);
			}
			return null;
		}

		static void UnknownTypeError (VmApi api)
		{
			api.RaiseShovelError ("Object of unknown type.");
		}

		internal static object ShovelStringRepresentation (VmApi api, object obj)
		{
			var result = ShovelStringRepresentationImpl (api, obj, new HashSet<object> ());
			api.CellsIncrementer (((string)result).Length);
			return result;
		}

		private static object ShovelStringRepresentationImpl (
			VmApi api, object obj, HashSet<object> visited)
		{
			if (visited.Contains (obj)) {
				return "[...loop...]";
			} else if (obj is String) {
				return String.Format ("\"{0}\"", ((string)obj).Replace ("\"", "\\\""));
			} else if (obj is List<object>) {
				visited.Add (obj);
				var stringReps = ((List<object>)obj)
					.Select (elem => ShovelStringRepresentationImpl (api, elem, visited));
				return String.Format ("array({0})", String.Join (", ", stringReps));
			} else if (obj is Dictionary<string, object>) {
				visited.Add (obj);
				var stringReps = new List<string> ();
				var hash = (Dictionary<string, object>)obj;
				foreach (var key in hash.Keys) {
					stringReps.Add ((string)ShovelStringRepresentationImpl (api, key, visited));
					stringReps.Add ((string)ShovelStringRepresentationImpl (api, hash [key], visited));
				}
				return String.Format ("hash({0})", String.Join (", ", stringReps));
			} else if (obj == null || (obj is long) || (obj is double) || (obj is bool) || (obj is Callable)) {
				return ShovelString (api, obj);
			} else {
				UnknownTypeError(api);
			}
			return null;
		}

		static object ParseInt (VmApi api, object str)
		{
			CheckString (api, str);
			return long.Parse ((string)str);
		}

		static object ParseFloat (VmApi api, object str)
		{
			CheckString (api, str);
			return double.Parse ((string)str);
		}

		static object Panic (VmApi api, object str)
		{
			CheckString (api, str);
			api.RaiseShovelError ((string)str);
			return null;
		}

	}
}

