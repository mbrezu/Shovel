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
using NUnit.Framework;
using System.Collections.Generic;

namespace ShovelTests
{
	[TestFixture]
	public class RequiredPrimitivesTests
	{
		[Test]
		public void NumericAddition ()
		{
			Utils.TestValue ("10 + 2.5", Shovel.ShovelValue.Kinds.Double, 12.5);
			Utils.TestValue ("10 + 2", Shovel.ShovelValue.Kinds.Integer, (long)12);
			Utils.TestValue ("10.5 + 2", Shovel.ShovelValue.Kinds.Double, 12.5);
			Utils.TestValue ("10.5 + 2.5", Shovel.ShovelValue.Kinds.Double, (double)13);
		}

		[Test]
		public void StringAddition ()
		{
			Utils.TestValue ("'the ' + 'string'", Shovel.ShovelValue.Kinds.String, "the string");
		}

		// FIXME: add tests for broken additions (invalid arguments).
		// FIXME: add tests for array additions.

		[Test]
		public void Subtractions ()
		{
			Utils.TestValue ("12.5 - 8", Shovel.ShovelValue.Kinds.Double, 4.5);
			Utils.TestValue ("12 - 8", Shovel.ShovelValue.Kinds.Integer, (long)4);
		}
		// FIXME: add tests for broken subtractions.

		[Test]
		public void Negations ()
		{
			Utils.TestValue ("- 8", Shovel.ShovelValue.Kinds.Integer, (long)-8);
			Utils.TestValue ("- 8.6", Shovel.ShovelValue.Kinds.Double, -8.6);
		}
		// FIXME: add tests for broken negations.

		[Test]
		public void Multiplications ()
		{
			Utils.TestValue ("11 * 11.1", Shovel.ShovelValue.Kinds.Double, 122.1);
			Utils.TestValue ("11 * 11", Shovel.ShovelValue.Kinds.Integer, (long)121);
		}
		// FIXME: add tests for broken multiplications.

		[Test]
		public void Divisions ()
		{
			Utils.TestValue ("10 / 7", Shovel.ShovelValue.Kinds.Integer, (long)1);
			Utils.TestValue ("22 / 7.0", Shovel.ShovelValue.Kinds.Double, 3.1428571428571428);
		}
		// FIXME: add tests for broken divisions.

		[Test]
		public void ShiftLeft ()
		{
			Utils.TestValue ("10 << 2", Shovel.ShovelValue.Kinds.Integer, (long)40);
		}

		[Test]
		public void ShiftRight ()
		{
			Utils.TestValue ("10 >> 2", Shovel.ShovelValue.Kinds.Integer, (long)2);
		}
		// FIXME: add tests for broken shl/shr.

		[Test]
		public void Modulo ()
		{
			Utils.TestValue ("10 % 3", Shovel.ShovelValue.Kinds.Integer, (long)1);
		}
		// FIXME: add tests for broken modulo.

		[Test]
		public void Power ()
		{
			Utils.TestValue ("pow(10, 3.0)", Shovel.ShovelValue.Kinds.Double, 1000.0);
			Utils.TestValue ("pow(10, 4)", Shovel.ShovelValue.Kinds.Integer, (long)10000);
			Utils.TestValue ("pow(10, 3)", Shovel.ShovelValue.Kinds.Integer, (long)1000);
			Utils.TestValue ("pow(10, 2)", Shovel.ShovelValue.Kinds.Integer, (long)100);
			Utils.TestValue ("pow(10, 1)", Shovel.ShovelValue.Kinds.Integer, (long)10);
			Utils.TestValue ("pow(10, 0)", Shovel.ShovelValue.Kinds.Integer, (long)1);
		}
		// FIXME: add tests for broken exponentiation.

		[Test]
		public void Floor ()
		{
			Utils.TestValue ("floor(10.34)", Shovel.ShovelValue.Kinds.Integer, (long)10);
			Utils.TestValue ("floor(10)", Shovel.ShovelValue.Kinds.Integer, (long)10);
		}
		// FIXME: add tests for broken truncation.

		[Test]
		public void LessThan ()
		{
			Utils.TestValue ("10 < 20", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("10 < 10", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("20 < 10", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("'a' < 'b'", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("'a' < 'a'", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("'b' < 'a'", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void LessThanOrEqual ()
		{
			Utils.TestValue ("10 <= 20", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("10 <= 10", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("12 <= 10", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void GreaterThan ()
		{
			Utils.TestValue ("20 > 10", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("20 > 20", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("10 > 20", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("'b' > 'a'", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("'b' > 'b'", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("'a' > 'b'", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void GreaterThanOrEqual ()
		{
			Utils.TestValue ("20 >= 10", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("20 >= 20", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("20 >= 30", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void AreEqual ()
		{
			Utils.TestValue ("20 == 10", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("10 == 10", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("'a' == 'a'", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("'b' == 'a'", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void AreNotEqual ()
		{
			Utils.TestValue ("20 != 10", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("10 != 10", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("'a' != 'a'", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("'b' != 'a'", Shovel.ShovelValue.Kinds.Bool, true);
		}
		// FIXME: add tests for broken comparisons.

		[Test]
		public void LogicalNot ()
		{
			Utils.TestValue ("!(20 == 10)", Shovel.ShovelValue.Kinds.Bool, true);
		}

		// These aren't implemented as primitives (they are rewritten as ifs by the compiler), 
		// but this looks like a good place to test them.
		[Test]
		public void LogicalOrAnd ()
		{
			Utils.TestValue ("2 == 3 && 2 == 2", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("2 == 3 || 2 == 2", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("2 == 2 || 2", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("2 == 3 && 2", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void BitwiseAnd ()
		{
			Utils.TestValue ("15 & 3", Shovel.ShovelValue.Kinds.Integer, (long)3);
			Utils.TestValue ("255 & 37", Shovel.ShovelValue.Kinds.Integer, (long)37);
		}

		[Test]
		public void BitwiseOr ()
		{
			Utils.TestValue ("15 | 3", Shovel.ShovelValue.Kinds.Integer, (long)15);
			Utils.TestValue ("15 | 3", Shovel.ShovelValue.Kinds.Integer, (long)15);
			Utils.TestValue ("255 | 37", Shovel.ShovelValue.Kinds.Integer, (long)255);
			Utils.TestValue ("12 | 10", Shovel.ShovelValue.Kinds.Integer, (long)14);
		}

		[Test]
		public void BitwiseXor ()
		{
			Utils.TestValue ("2 ^ 2", Shovel.ShovelValue.Kinds.Integer, (long)0);
			Utils.TestValue ("10 ^ 2", Shovel.ShovelValue.Kinds.Integer, (long)8);
			Utils.TestValue ("8 ^ 2", Shovel.ShovelValue.Kinds.Integer, (long)10);
		}

		// FIXME: add tests for broken bitwise operations.

		// FIXME: negative tests for primitives tested below (broken parameters).
		[Test]
		public void HashConstructor ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "hash('a', 1, 'b', 2)");
			var result = Shovel.Api.NakedRunVm (sources);
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Hash);
			Assert.AreEqual (2, result.HashValue.Keys.Count);
			Assert.AreEqual (1, result.HashValue [Shovel.ShovelValue.Make ("a")].IntegerValue);
			Assert.AreEqual (2, result.HashValue [Shovel.ShovelValue.Make ("b")].IntegerValue);
		}

		[Test]
		public void HasKey ()
		{
			Utils.TestValue ("hasKey(hash('a', 1, 'b', 2), 'a')", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("hasKey(hash('a', 1, 'b', 2), 'b')", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("hasKey(hash('a', 1, 'b', 2), 'c')", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void Keys ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "keys(hash('a', 1, 'b', 2))");
			var result = Shovel.Api.NakedRunVm (sources);
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Array);
			Assert.AreEqual (2, result.ArrayValue.Count);
			Assert.IsTrue (result.ArrayValue.Contains (Shovel.ShovelValue.Make ("a")));
			Assert.IsTrue (result.ArrayValue.Contains (Shovel.ShovelValue.Make ("b")));
			Assert.IsFalse (result.ArrayValue.Contains (Shovel.ShovelValue.Make ("c")));
		}

		[Test]
		public void ArrayConstructor ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "array(1, 2, 3)");
			var result = Shovel.Api.NakedRunVm (sources);
			Is123 (result);
		}

		void Is123 (Shovel.ShovelValue result)
		{
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Array);
			Assert.AreEqual (3, result.ArrayValue.Count);
			Assert.AreEqual (1, result.ArrayValue [0].IntegerValue);
			Assert.AreEqual (2, result.ArrayValue [1].IntegerValue);
			Assert.AreEqual (3, result.ArrayValue [2].IntegerValue);
		}

		[Test]
		public void SizedArrayConstructor ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "arrayN(3)");
			var result = Shovel.Api.NakedRunVm (sources);
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Array);
			Assert.AreEqual (3, result.ArrayValue.Count);
			Assert.IsTrue (result.ArrayValue [0].Kind == Shovel.ShovelValue.Kinds.Null);
			Assert.IsTrue (result.ArrayValue [1].Kind == Shovel.ShovelValue.Kinds.Null);
			Assert.IsTrue (result.ArrayValue [2].Kind == Shovel.ShovelValue.Kinds.Null);
		}

		[Test]
		public void VectorPush ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = array(1, 2) push(a, 3) a");
			var result = Shovel.Api.NakedRunVm (sources);
			Is123 (result);
		}

		[Test]
		public void VectorPop ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = array(1, 2, 3, 4) pop(a) a");
			var result = Shovel.Api.NakedRunVm (sources);
			Is123 (result);

			Utils.TestValue ("var a = array(1, 2, 3, 4) pop(a)", Shovel.ShovelValue.Kinds.Integer, (long)4);
		}

		[Test]
		public void ArrayGet ()
		{
			Utils.TestValue ("var a = array(1, 2, 3, 4) a[2]", Shovel.ShovelValue.Kinds.Integer, (long)3);
			Utils.TestValue ("var a = 'test' a[2]", Shovel.ShovelValue.Kinds.String, "s");
		}

		[Test]
		public void HashGet ()
		{
			Utils.TestValue ("var h = hash('a', 1, 'b', 2) h['b']", Shovel.ShovelValue.Kinds.Integer, (long)2);
		}

		[Test]
		public void HashDotGet ()
		{
			Utils.TestValue ("var h = hash('a', 1, 'b', 2) h.b", Shovel.ShovelValue.Kinds.Integer, (long)2);
		}

		[Test]
		public void ArraySet ()
		{
			Utils.TestValue ("var a = array(1, 2, 3, 4) a[2] = 'b' a[2]", Shovel.ShovelValue.Kinds.String, "b");
		}

		[Test]
		public void HashSet ()
		{
			Utils.TestValue (
				"var h = hash('a', 1, 'b', 2) h['b'] = 3 h['b']", 
				Shovel.ShovelValue.Kinds.Integer, (long)3);
		}

		[Test]
		public void GetLength ()
		{
			Utils.TestValue ("var a = array('a', 1, 'b', 2) length(a)", Shovel.ShovelValue.Kinds.Integer, (long)4);
			Utils.TestValue ("length('test')", Shovel.ShovelValue.Kinds.Integer, (long)4);
		}

		[Test]
		public void GetSlice ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = array('a', 1, 'b', 2) slice(a, -1, -1)");
			var result = Shovel.Api.NakedRunVm (sources);
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Array);
			Assert.AreEqual (1, result.ArrayValue.Count);
			Assert.IsTrue (result.ArrayValue [0].Kind == Shovel.ShovelValue.Kinds.Integer);
			Assert.AreEqual (2, result.ArrayValue [0].IntegerValue);

			Utils.TestValue (
				"var a = 'Test' slice(a, 1, -2)", Shovel.ShovelValue.Kinds.String, "es");
		}

		[Test]
		public void StringUpper ()
		{
			Utils.TestValue (
				"var a = 'Test' upper(a)", Shovel.ShovelValue.Kinds.String, "TEST");
		}

		[Test]
		public void StringLower ()
		{
			Utils.TestValue (
				"var a = 'Test' lower(a)", Shovel.ShovelValue.Kinds.String, "test");
		}

		[Test]
		public void DecodeTime ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "decodeTime(utcSecondsSinceUnixEpoch())");
			var result = Shovel.Api.NakedRunVm (sources);
			var aNow = DateTime.UtcNow;
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Hash);
			Assert.IsTrue (
				result.HashValue [Shovel.ShovelValue.Make ("year")].Kind == Shovel.ShovelValue.Kinds.Integer);
			Assert.IsTrue (
				result.HashValue [Shovel.ShovelValue.Make ("month")].Kind == Shovel.ShovelValue.Kinds.Integer);
			Assert.IsTrue (
				result.HashValue [Shovel.ShovelValue.Make ("day")].Kind == Shovel.ShovelValue.Kinds.Integer);
			Assert.IsTrue (
				result.HashValue [Shovel.ShovelValue.Make ("hour")].Kind == Shovel.ShovelValue.Kinds.Integer);
			Assert.IsTrue (
				result.HashValue [Shovel.ShovelValue.Make ("minute")].Kind == Shovel.ShovelValue.Kinds.Integer);
			Assert.IsTrue (
				result.HashValue [Shovel.ShovelValue.Make ("second")].Kind == Shovel.ShovelValue.Kinds.Integer);
			Assert.IsTrue (
				result.HashValue [Shovel.ShovelValue.Make ("dayOfWeek")].Kind == Shovel.ShovelValue.Kinds.Integer);
			var aDate = new DateTime (
				(int)result.HashValue [Shovel.ShovelValue.Make ("year")].IntegerValue,
				(int)result.HashValue [Shovel.ShovelValue.Make ("month")].IntegerValue,
				(int)result.HashValue [Shovel.ShovelValue.Make ("day")].IntegerValue,
				(int)result.HashValue [Shovel.ShovelValue.Make ("hour")].IntegerValue,
				(int)result.HashValue [Shovel.ShovelValue.Make ("minute")].IntegerValue,
				(int)result.HashValue [Shovel.ShovelValue.Make ("second")].IntegerValue);
			var diff = aNow - aDate;
			Assert.IsTrue (diff.TotalSeconds < 1);
		}

		[Test]
		public void EncodeTime ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", @"
var time = hash('year', 2012, 'month', 11, 'day', 15,
                'hour', 12, 'minute', 18, 'second', 37)
encodeTime(time)"
			);
			var result = Shovel.Api.NakedRunVm (sources);
			Assert.IsTrue (result.Kind == Shovel.ShovelValue.Kinds.Integer);
			var decodedDate = new DateTime (1970, 1, 1) + TimeSpan.FromSeconds (result.IntegerValue);
			Assert.AreEqual (decodedDate, new DateTime (2012, 11, 15, 12, 18, 37));
		}

		[Test]
		public void IsString ()
		{
			Utils.TestValue ("isString('test')", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("isString(12)", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void IsHash ()
		{
			Utils.TestValue ("isHash(hash('a', 1))", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("isHash(12)", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void IsBool ()
		{
			Utils.TestValue ("isBool(true)", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("isBool(12)", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void IsArray ()
		{
			Utils.TestValue ("isArray(array(1))", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("isArray(1)", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void IsNumber ()
		{
			Utils.TestValue ("isNumber(1)", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("isNumber(1.5)", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("isNumber(true)", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void IsInteger ()
		{
			Utils.TestValue ("isInteger(1)", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("isInteger(1.5)", Shovel.ShovelValue.Kinds.Bool, false);
			Utils.TestValue ("isInteger(true)", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void IsCallable ()
		{
			Utils.TestValue ("isCallable(fn() 1)", Shovel.ShovelValue.Kinds.Bool, true);
			Utils.TestValue ("isCallable(true)", Shovel.ShovelValue.Kinds.Bool, false);
		}

		[Test]
		public void Panic ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "panic('test')");
			Utils.ExpectException<Shovel.ShovelException> (() => {
				Shovel.Api.NakedRunVm (sources);
			},
			(ex) => {
				Assert.AreEqual ("test", ex.Message);
			}
			);
		}

		[Test]
		public void ParseInt ()
		{
			Utils.TestValue ("parseInt('10')", Shovel.ShovelValue.Kinds.Integer, (long)10);
		}

		[Test]
		public void ParseFloat ()
		{
			Utils.TestValue ("parseFloat('10.5')", Shovel.ShovelValue.Kinds.Double, 10.5);
		}

		[Test]
		public void ShovelString ()
		{
			Utils.TestValue("string('test')", Shovel.ShovelValue.Kinds.String, "test");
			Utils.TestValue("string(array())", Shovel.ShovelValue.Kinds.String, "[...array...]");
			Utils.TestValue("string(10)", Shovel.ShovelValue.Kinds.String, "10");
			Utils.TestValue("string(10.5)", Shovel.ShovelValue.Kinds.String, "10.5");
			Utils.TestValue("string(hash())", Shovel.ShovelValue.Kinds.String, "[...hash...]");
			Utils.TestValue("string(fn() 1)", Shovel.ShovelValue.Kinds.String, "[...callable...]");
			Utils.TestValue("string(true)", Shovel.ShovelValue.Kinds.String, "true");
			Utils.TestValue("string(null)", Shovel.ShovelValue.Kinds.String, "null");
		}

		[Test]
		public void ShovelStringRepresentation ()
		{
          		Utils.TestValue(                       
                            "stringRepresentation('test')", Shovel.ShovelValue.Kinds.String, "\"test\"");
          		Utils.TestValue(                       
                            "stringRepresentation('te\"st')", Shovel.ShovelValue.Kinds.String, "\"te\\\"st\"");
          		Utils.TestValue(                       
                            "stringRepresentation(array(1, 2))", Shovel.ShovelValue.Kinds.String, "array(1, 2)");
          		Utils.TestValue(                       
                            "var a = array(1, 2, 3) a[2] = a stringRepresentation(a)",
                            Shovel.ShovelValue.Kinds.String, "array(1, 2, [...loop...])");
          		Utils.TestValue(                       
                            "var a = hash('a', 1, 'b', 2) stringRepresentation(a)",
                            Shovel.ShovelValue.Kinds.String, "hash(\"a\", 1, \"b\", 2)");
          		Utils.TestValue(                       
                            "var a = hash('a', 1, 'b', 2) a['c'] = a stringRepresentation(a)",
                            Shovel.ShovelValue.Kinds.String, "hash(\"a\", 1, \"b\", 2, \"c\", [...loop...])");
          		Utils.TestValue(                       
                            @"
var a = hash('a', 1, 'b', 2, 'c', array(1, 2, hash('d', 4)))
stringRepresentation(a)",
                            Shovel.ShovelValue.Kinds.String,                            
                            "hash(\"a\", 1, \"b\", 2, \"c\", array(1, 2, hash(\"d\", 4)))");
		}
	}
}

