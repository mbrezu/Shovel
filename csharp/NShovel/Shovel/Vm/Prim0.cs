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
			AddPrim0 (result, Callable.MakePrim0 ("&&", Callable.MakeHostCallable (LogicalAnd)));
			AddPrim0 (result, Callable.MakePrim0 ("||", Callable.MakeHostCallable (LogicalOr)));

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
			AddPrim0 (result, Callable.MakePrim0 ("isCallable", Callable.MakeHostCallable (IsCallable), 1));

			// Stringification.
			AddPrim0 (result, Callable.MakePrim0 ("string", Callable.MakeHostCallable (ShovelString), 1));
			AddPrim0 (result, Callable.MakePrim0 (
				"stringRepresentation", Callable.MakeHostCallable (ShovelStringRepresentation), 1));

			// Parsing numbers.
			AddPrim0 (result, Callable.MakePrim0 ("parseInt", Callable.MakeHostCallable (ParseInt), 1));
			AddPrim0 (result, Callable.MakePrim0 ("parseFloat", Callable.MakeHostCallable (ParseFloat), 1));

			// Exception throwing.
			AddPrim0 (result, Callable.MakePrim0 ("panic", Callable.MakeHostCallable (Panic), 1));

			return result;
		}

		static object Add (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object Subtract (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object UnaryMinus (VmApi api, object t1)
		{
			throw new NotImplementedException ();
		}

		static object Multiply (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object Divide (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object ShiftLeft (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object ShiftRight (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object Modulo (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object Pow (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object Floor (VmApi api, object t1)
		{
			throw new NotImplementedException ();
		}

		static object LessThan (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object LessThanOrEqual (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object GreaterThan (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object GreaterThanOrEqual (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object AreEqual (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object AreNotEqual (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
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

		static object LogicalAnd (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object LogicalOr (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object BitwiseAnd (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object BitwiseOr (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object BitwiseXor (VmApi api, object t1, object t2)
		{
			throw new NotImplementedException ();
		}

		static object HashConstructor (VmApi api, List<object> args)
		{
			throw new NotImplementedException ();
		}

		static object HasKey (VmApi api, object hash, object key)
		{
			throw new NotImplementedException ();
		}

		static object Keys (VmApi api, object hash)
		{
			throw new NotImplementedException ();
		}

		static object ArrayConstructor (VmApi api, List<object> args)
		{
			throw new NotImplementedException ();
		}

		static object SizedArrayConstructor (VmApi api, object size)
		{
			throw new NotImplementedException ();
		}

		static object ArrayPush (VmApi api, object array, object value)
		{
			throw new NotImplementedException ();
		}

		static object ArrayPop (VmApi api, object array)
		{
			throw new NotImplementedException ();
		}

		static object ArrayOrHashGet (VmApi api, object arrayOrHash, object index)
		{
			throw new NotImplementedException ();
		}

		static object HashGetDot (VmApi api, object hash, object index)
		{
			throw new NotImplementedException ();
		}

		static object ArrayOrHashSet (VmApi api, object arrayOrHash, object index, object value)
		{
			throw new NotImplementedException ();
		}

		static object GetLength (VmApi api, object arrayOrString)
		{
			throw new NotImplementedException ();
		}

		static object GetSlice (VmApi api, object arrayOrString, object start, object end)
		{
			throw new NotImplementedException ();
		}

		static object StringUpper (VmApi api, object str)
		{
			throw new NotImplementedException ();
		}

		static object StringLower (VmApi api, object str)
		{
			throw new NotImplementedException ();
		}

		static object UtcSecondsSinceUnixEpoch (VmApi api)
		{
			throw new NotImplementedException ();
		}

		static object DecodeTime (VmApi api, object timeInSeconds)
		{
			throw new NotImplementedException ();
		}

		static object EncodeTime (VmApi api, object timeHash)
		{
			throw new NotImplementedException ();
		}

		static object IsString (VmApi api, object obj)
		{
			throw new NotImplementedException ();
		}

		static object IsHash (VmApi api, object obj)
		{
			throw new NotImplementedException ();
		}

		static object IsBool (VmApi api, object obj)
		{
			throw new NotImplementedException ();
		}

		static object IsArray (VmApi api, object obj)
		{
			throw new NotImplementedException ();
		}

		static object IsNumber (VmApi api, object obj)
		{
			throw new NotImplementedException ();
		}

		static object IsCallable (VmApi api, object obj)
		{
			throw new NotImplementedException ();
		}

		static object ShovelString (VmApi api, object obj)
		{
			throw new NotImplementedException ();
		}

		static object ShovelStringRepresentation (VmApi api, object obj)
		{
			throw new NotImplementedException ();
		}

		static object ParseInt (VmApi api, object str)
		{
			throw new NotImplementedException ();
		}

		static object ParseFloat (VmApi api, object str)
		{
			throw new NotImplementedException ();
		}

		static object Panic (VmApi api, object str)
		{
			throw new NotImplementedException ();
		}

	}
}

