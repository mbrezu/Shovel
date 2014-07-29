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
using System.Linq;

namespace ShovelTests
{
	[TestFixture]
	public class BytecodeSerializerTests
	{
		[Test]
		public void ConstInt ()
		{
			TestBytecodeSerialization (
				"1", 
				obj => obj.Kind == Shovel.Value.Kinds.Integer && obj.Integer.Value == 1);
		}

		[Test]
		public void ConstBool ()
		{
			TestBytecodeSerialization (
				"true || false", 
				obj => obj.Kind == Shovel.Value.Kinds.Bool && obj.Bool.Value);
		}

		[Test]
		public void ConstDouble ()
		{
			TestBytecodeSerialization (
				"1.4", obj => obj.Kind == Shovel.Value.Kinds.Double && 1.4 == obj.Double.Value);
		}

		[Test]
		public void ConstString ()
		{
			TestBytecodeSerialization (
				"'test'", 
				obj => obj.Kind == Shovel.Value.Kinds.String && "test" == obj.String.Value);
		}

		[Test]
		public void ConstNull ()
		{
			TestBytecodeSerialization (
				"null", 
				obj => obj.Kind == Shovel.Value.Kinds.Null);
		}

		[Test]
		public void Factorial ()
		{
			TestBytecodeSerialization (
				Utils.FactorialOfTenProgram (), 
				obj => obj.Kind == Shovel.Value.Kinds.Integer && (long)3628800 == obj.Integer.Value);
		}

		[Test]
		public void Fibonacci ()
		{
			TestBytecodeSerialization (
				Utils.FibonacciOfTenProgram (), 
				obj => obj.Kind == Shovel.Value.Kinds.Integer && (long)89 == obj.Integer.Value);
		}

		void TestBytecodeSerialization (
			string program, Func<Shovel.Value, bool> resultChecker)
		{
			var fileName = "test.sho";
			var sources = Shovel.Api.MakeSources (fileName, program);
			Console.WriteLine (Shovel.Api.PrintRawBytecode (sources));
			var bytecode = Shovel.Api.GetBytecode (sources);
			var ms = Shovel.Api.SerializeBytecode (bytecode);
			var bytecode2 = Shovel.Api.DeserializeBytecode (ms);
			var bytes1 = ms.ToArray ();
			var bytes2 = Shovel.Api.SerializeBytecode (bytecode2).ToArray ();
			Assert.IsTrue (bytes1.SequenceEqual (bytes2));
			var result = Shovel.Api.TestRunVm (bytecode2, sources);
			Assert.IsTrue (resultChecker (result));
		}

	}
}

