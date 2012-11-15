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
			var sources = Shovel.Api.MakeSources ("test.sho", "10 + 2.5");
			Assert.AreEqual (12.5, (double)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "10 + 2");
			Assert.AreEqual (12, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "10.5 + 2");
			Assert.AreEqual (12.5, (double)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "10.5 + 2.5");
			Assert.AreEqual (13, (double)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void StringAddition ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "'the ' + 'string'");
			Assert.AreEqual ("the string", (string)Shovel.Api.NakedRunVm (sources));
		}

		// FIXME: add tests for broken additions (invalid arguments).
		// FIXME: add tests for array additions.

		[Test]
		public void Subtractions ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "12.5 - 8");
			Assert.AreEqual (4.5, (double)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "12 - 8");
			Assert.AreEqual (4, (long)Shovel.Api.NakedRunVm (sources));
		}
		// FIXME: add tests for broken subtractions.

		[Test]
		public void Negations ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "- 8");
			Assert.AreEqual (-8, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "-8.6");
			Assert.AreEqual (-8.6, (double)Shovel.Api.NakedRunVm (sources));
		}
		// FIXME: add tests for broken negations.

		[Test]
		public void Multiplications ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "11 * 11.1");
			Assert.AreEqual (122.1, (double)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "11 * 11");
			Assert.AreEqual (121, (long)Shovel.Api.NakedRunVm (sources));
		}
		// FIXME: add tests for broken multiplications.

		[Test]
		public void Divisions ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "10 / 7");
			Assert.AreEqual (1, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "22 / 7.0");
			Assert.AreEqual (3.1428571428571428, (double)Shovel.Api.NakedRunVm (sources));
		}
		// FIXME: add tests for broken divisions.

		[Test]
		public void ShiftLeft ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "10 << 2");
			Assert.AreEqual (40, (long)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void ShiftRight ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "10 >> 2");
			Assert.AreEqual (2, (long)Shovel.Api.NakedRunVm (sources));
		}
		// FIXME: add tests for broken shl/shr.

		[Test]
		public void Modulo ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "10 % 3");
			Assert.AreEqual (1, (long)Shovel.Api.NakedRunVm (sources));
		}
		// FIXME: add tests for broken modulo.

		[Test]
		public void Power ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "pow(10, 3.0)");
			Assert.AreEqual (1000.0, (double)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "pow(10, 3)");
			Assert.AreEqual (1000, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "pow(10, 2)");
			Assert.AreEqual (100, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "pow(10, 1)");
			Assert.AreEqual (10, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "pow(10, 0)");
			Assert.AreEqual (1, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "pow(10, 4)");
			Assert.AreEqual (10000, (long)Shovel.Api.NakedRunVm (sources));
		}
		// FIXME: add tests for broken exponentiation.

		[Test]
		public void Floor ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "floor(10.34)");
			Assert.AreEqual (10, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "floor(10)");
			Assert.AreEqual (10, (long)Shovel.Api.NakedRunVm (sources));
		}
		// FIXME: add tests for broken truncation.

		[Test]
		public void LessThan ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "10 < 20");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "10 < 10");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "20 < 10");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'a' < 'b'");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'a' < 'a'");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'b' < 'a'");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void LessThanOrEqual ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "10 <= 20");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "10 <= 10");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void GreaterThan ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "20 > 10");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "20 > 20");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "10 > 20");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'b' > 'a'");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'b' > 'b'");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'a' > 'b'");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void GreaterThanOrEqual ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "20 >= 10");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "20 >= 20");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void AreEqual ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "20 == 10");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "10 == 10");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'a' == 'a'");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'b' == 'a'");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void AreNotEqual ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "20 != 10");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "10 != 10");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'a' != 'a'");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "'b' != 'a'");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
		}
		// FIXME: add tests for broken comparisons.

		[Test]
		public void LogicalNot ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "!(20 == 10)");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
		}

		// These aren't implemented as primitives (they are rewritten as ifs by the compiler), 
		// but this looks like a good place to test them.
		[Test]
		public void LogicalOrAnd ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "2 == 3 && 2 == 2");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "2 == 3 || 2 == 2");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			// Short circuits.
			sources = Shovel.Api.MakeSources ("test.sho", "2 == 2 || 2");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "2 == 3 && 2");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void BitwiseAnd ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "15 & 3");
			Assert.AreEqual (3, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "255 & 37");
			Assert.AreEqual (37, (long)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void BitwiseOr ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "15 | 3");
			Assert.AreEqual (15, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "255 | 37");
			Assert.AreEqual (255, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "12 | 10");
			Assert.AreEqual (14, (long)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void BitwiseXor ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "2 ^ 2");
			Assert.AreEqual (0, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "10 ^ 2");
			Assert.AreEqual (8, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "8 ^ 2");
			Assert.AreEqual (10, (long)Shovel.Api.NakedRunVm (sources));
		}

		// FIXME: add tests for broken bitwise operations.

		// FIXME: negative tests for primitives tested below (broken parameters).
		[Test]
		public void HashConstructor ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "hash('a', 1, 'b', 2)");
			var result = (Dictionary<string, object>)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual (2, result.Keys.Count);
			Assert.AreEqual (1, result ["a"]);
			Assert.AreEqual (2, result ["b"]);
		}

		[Test]
		public void HasKey ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "hasKey(hash('a', 1, 'b', 2), 'a')");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "hasKey(hash('a', 1, 'b', 2), 'b')");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "hasKey(hash('a', 1, 'b', 2), 'c')");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void Keys ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "keys(hash('a', 1, 'b', 2))");
			var result = (List<object>)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual (2, result.Count);
			Assert.IsTrue (result.Contains ("a"));
			Assert.IsTrue (result.Contains ("b"));
			Assert.IsFalse (result.Contains ("c"));
		}

		[Test]
		public void ArrayConstructor ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "array(1, 2, 3)");
			var result = (List<object>)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual (3, result.Count);
			Assert.AreEqual (1, (long)result [0]);
			Assert.AreEqual (2, (long)result [1]);
			Assert.AreEqual (3, (long)result [2]);
		}

		[Test]
		public void SizedArrayConstructor ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "arrayN(3)");
			var result = (List<object>)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual (3, result.Count);
			Assert.AreEqual (null, result [0]);
			Assert.AreEqual (null, result [1]);
			Assert.AreEqual (null, result [2]);
		}

		[Test]
		public void VectorPush ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = array(1, 2) push(a, 3) a");
			var result = (List<object>)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual (3, result.Count);
			Assert.AreEqual (1, (long)result [0]);
			Assert.AreEqual (2, (long)result [1]);
			Assert.AreEqual (3, (long)result [2]);
		}

		[Test]
		public void VectorPop ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = array(1, 2, 3, 4) pop(a) a");
			var result = (List<object>)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual (3, result.Count);
			Assert.AreEqual (1, (long)result [0]);
			Assert.AreEqual (2, (long)result [1]);
			Assert.AreEqual (3, (long)result [2]);
		}

		[Test]
		public void ArrayGet ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = array(1, 2, 3, 4) a[2]");
			Assert.AreEqual (3, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "var a = 'test' a[2]");
			Assert.AreEqual ("s", (string)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void HashGet ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var h = hash('a', 1, 'b', 2) h['b']");
			Assert.AreEqual (2, (long)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void HashDotGet ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var h = hash('a', 1, 'b', 2) h.b");
			Assert.AreEqual (2, (long)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void ArraySet ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = array(1, 2, 3, 4) a[2] = 'b' a[2]");
			Assert.AreEqual ("b", (string)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void HashSet ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var h = hash('a', 1, 'b', 2) h['b'] = 3 h['b']");
			Assert.AreEqual (3, (long)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void GetLength ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = array('a', 1, 'b', 2) length(a)");
			Assert.AreEqual (4, (long)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "length('test')");
			Assert.AreEqual (4, (long)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void GetSlice ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = array('a', 1, 'b', 2) slice(a, -1, -1)");
			var result = (List<object>)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual (1, result.Count);
			Assert.AreEqual (2, (long)result [0]);
			sources = Shovel.Api.MakeSources ("test.sho", "var a = 'Test' slice(a, 1, -2)");
			var result2 = (string)Shovel.Api.NakedRunVm (sources);
			Assert.AreEqual ("es", result2);
		}

		[Test]
		public void StringUpper ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = 'Test' upper(a)");
			Assert.AreEqual ("TEST", (string)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void StringLower ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "var a = 'Test' lower(a)");
			Assert.AreEqual ("test", (string)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void DecodeTime ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "decodeTime(utcSecondsSinceUnixEpoch())");
			var result = (Dictionary<string, object>)Shovel.Api.NakedRunVm (sources);
			var aNow = DateTime.UtcNow;
			Assert.IsInstanceOf (typeof(long), result ["year"]);
			Assert.IsInstanceOf (typeof(long), result ["month"]);
			Assert.IsInstanceOf (typeof(long), result ["day"]);
			Assert.IsInstanceOf (typeof(long), result ["hour"]);
			Assert.IsInstanceOf (typeof(long), result ["second"]);
			Assert.IsInstanceOf (typeof(long), result ["dayOfWeek"]);
			var aDate = new DateTime (
				(int)(long)result ["year"], (int)(long)result ["month"], (int)(long)result ["day"],
				(int)(long)result ["hour"], (int)(long)result ["minute"], (int)(long)result ["second"]);
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
			var result = (long)Shovel.Api.NakedRunVm (sources);
			Assert.IsInstanceOf (typeof(long), result);
			var decodedDate = new DateTime (1970, 1, 1) + TimeSpan.FromSeconds ((long)result);
			Assert.AreEqual (decodedDate, new DateTime (2012, 11, 15, 12, 18, 37));
		}

		[Test]
		public void IsString ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "isString('test')");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isString(12)");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void IsHash ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "isHash(hash('a', 1))");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isHash(12)");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void IsBool ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "isBool(true)");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isBool(12)");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void IsArray ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "isArray(array(1))");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isArray(1)");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void IsNumber ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "isNumber(1)");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isNumber(1.5)");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isNumber(true)");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void IsInteger ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "isInteger(1)");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isInteger(1.5)");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isNumber(true)");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void IsCallable ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "isCallable(fn() 1)");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isCallable(isCallable)");
			Assert.AreEqual (true, (bool)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "isCallable(true)");
			Assert.AreEqual (false, (bool)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void Panic ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "panic('test')");
			Utils.ExpectException<InvalidOperationException> (() => {
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
			var sources = Shovel.Api.MakeSources ("test.sho", "parseInt('10')");
			Assert.AreEqual (10, (long)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void ParseFloat ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "parseFloat('10.5')");
			Assert.AreEqual (10.5, (double)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void ShovelString ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "string('test')");
			Assert.AreEqual ("test", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "string(array())");
			Assert.AreEqual ("[...array...]", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "string(10)");
			Assert.AreEqual ("10", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "string(10.5)");
			Assert.AreEqual ("10.5", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "string(hash())");
			Assert.AreEqual ("[...hash...]", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "string(fn() 1)");
			Assert.AreEqual ("[...callable...]", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "string(true)");
			Assert.AreEqual ("true", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "string(null)");
			Assert.AreEqual ("null", (string)Shovel.Api.NakedRunVm (sources));
		}

		[Test]
		public void ShovelStringRepresentation ()
		{
			var sources = Shovel.Api.MakeSources ("test.sho", "stringRepresentation('test')");
			Assert.AreEqual ("\"test\"", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "stringRepresentation('te\"st')");
			Assert.AreEqual ("\"te\\\"st\"", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "stringRepresentation(array(1, 2))");
			Assert.AreEqual ("array(1, 2)", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "var a = array(1, 2, 3) a[2] = a stringRepresentation(a)");
			Assert.AreEqual ("array(1, 2, [...loop...])", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", "var a = hash('a', 1, 'b', 2) stringRepresentation(a)");
			Assert.AreEqual ("hash(\"a\", 1, \"b\", 2)", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources (
				"test.sho", "var a = hash('a', 1, 'b', 2) a['c'] = a stringRepresentation(a)");
			Assert.AreEqual ("hash(\"a\", 1, \"b\", 2, \"c\", [...loop...])", (string)Shovel.Api.NakedRunVm (sources));
			sources = Shovel.Api.MakeSources ("test.sho", @"
var a = hash('a', 1, 'b', 2, 'c', array(1, 2, hash('d', 4)))
stringRepresentation(a)"
			);
			Assert.AreEqual ("hash(\"a\", 1, \"b\", 2, \"c\", array(1, 2, hash(\"d\", 4)))", 
			                 (string)Shovel.Api.NakedRunVm (sources));
		}

	}
}

