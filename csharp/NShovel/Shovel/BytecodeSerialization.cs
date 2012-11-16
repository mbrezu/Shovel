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
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Linq;

namespace Shovel
{
	public static class BytecodeSerialization
	{
		static byte[] minusOne = BitConverter.GetBytes ((int)-1);

		static void WriteBytes (Stream s, byte[] bytes)
		{
			s.Write (bytes, 0, bytes.Length);
		}

		static byte[] sixteenZeroes = new byte[16];

		static private MemoryStream SerializeWithMd5CheckSum (Action<Stream> body)
		{
			var ms = new MemoryStream ();
			WriteBytes (ms, sixteenZeroes);
			ms.WriteByte (Utils.Endianess ());
			WriteBytes (ms, BitConverter.GetBytes ((int)Shovel.Api.Version));
			body (ms);
			using (var md5 = MD5.Create()) {
				ms.Seek (0, SeekOrigin.Begin);
				var md5Bytes = md5.ComputeHash (ms);
				ms.Seek (0, SeekOrigin.Begin);
				WriteBytes (ms, md5Bytes);
			}
			return ms;
		}

		static internal Instruction[] DeserializeBytecode (MemoryStream ms)
		{
			return (Instruction[])DeserializeWithMd5CheckSum (ms, str => {
				return DeserializeBytecodeImpl (str);
			}
			);
		}

		static private object DeserializeWithMd5CheckSum (MemoryStream ms, Func<Stream, object> body)
		{
			// Check MD5 checksum.
			ms.Seek (0, SeekOrigin.Begin);
			byte[] expectedMd5 = new byte[16];
			ms.Read (expectedMd5, 0, expectedMd5.Length);
			ms.Seek (0, SeekOrigin.Begin);
			WriteBytes (ms, sixteenZeroes);
			using (var md5 = MD5.Create()) {
				ms.Seek (0, SeekOrigin.Begin);
				var actualMd5 = md5.ComputeHash (ms);
				if (!expectedMd5.SequenceEqual (actualMd5)) {
					throw new BrokenDataException ();
				}
			}	
			ms.Seek (0, SeekOrigin.Begin);
			WriteBytes (ms, expectedMd5);
			// Check endianess.
			if (ms.ReadByte () != Utils.Endianess ()) {
				throw new EndianessMismatchException ();
			}
			// Check version.
			if (ReadInt (ms) > Shovel.Api.Version) {
				throw new VersionNotSupportedException ();
			}
			return body (ms);
		}

		static private object DeserializeBytecodeImpl (Stream s)
		{
			// Read the number of instructions.
			int instructionsCount = ReadInt (s);
			// Allocate the array of instructions.
			var result = new Instruction[instructionsCount];
			// Fill in instructions.
			for (var i = 0; i < instructionsCount; i++) {
				result [i] = ReadInstruction (s);
			}
			return result;
		}

		static int ReadInt (Stream ms)
		{
			var bytes = new byte[4];
			ms.Read (bytes, 0, 4);
			return BitConverter.ToInt32 (bytes, 0);
		}

		static internal MemoryStream SerializeBytecode (Instruction[] bytecode)
		{
			return SerializeWithMd5CheckSum (str => {
				SerializeBytecodeImplementation (str, bytecode);
			}
			);
		}

		static private void SerializeBytecodeImplementation (Stream ms, Instruction[] bytecode)
		{
			WriteBytes (ms, BitConverter.GetBytes ((int)bytecode.Length));
			foreach (var instruction in bytecode) {
				WriteBytes (ms, BitConverter.GetBytes ((int)instruction.NumericOpcode));
				SerializeNullableInt (ms, instruction.StartPos);
				SerializeNullableInt (ms, instruction.EndPos);
				WriteBytes (ms, BitConverter.GetBytes ((int)instruction.Opcode));
				WriteArguments (ms, instruction);
			}
		}

		static Instruction ReadInstruction (Stream s)
		{
			var instruction = new Instruction ();
			instruction.NumericOpcode = ReadInt (s);
			instruction.StartPos = DeserializeNullableInt (s);
			instruction.EndPos = DeserializeNullableInt (s);
			var opcodeInt = ReadInt (s);
			instruction.Opcode = (Instruction.Opcodes)opcodeInt;
			ReadArguments (s, instruction);
			return instruction;
		}

		static void WriteString (Stream stream, string str)
		{
			byte[] bytes = Encoding.UTF8.GetBytes (str);
			if (bytes.Length < 256) {
				stream.WriteByte (1);
				stream.WriteByte ((byte)bytes.Length);
				WriteBytes (stream, bytes);
			} else {
				stream.WriteByte (4);
				WriteBytes (stream, BitConverter.GetBytes ((int)bytes.Length));
				WriteBytes (stream, bytes);
			}
		}

		static string ReadString (Stream s)
		{
			var disc = s.ReadByte ();
			if (disc == 1) {
				var size = s.ReadByte ();
				var bytes = new byte[size];
				s.Read (bytes, 0, size);
				return Encoding.UTF8.GetString (bytes);
			} else if (disc == 4) {
				var size = ReadInt (s);
				var bytes = new byte[size];
				s.Read (bytes, 0, size);
				return Encoding.UTF8.GetString (bytes);
			} else {
				Utils.Panic ();
				return null;
			}
		}

		static void WriteConst (Stream stream, object obj)
		{
			if (obj is string) {
				stream.WriteByte (1);
				WriteString (stream, (string)obj);
			} else if (obj is bool) {
				stream.WriteByte (2);
				stream.WriteByte ((byte)((bool)obj ? 1 : 0));
			} else if (obj is double) {
				stream.WriteByte (3);
				WriteBytes (stream, BitConverter.GetBytes ((double)obj));
			} else if (obj is long) {
				stream.WriteByte (4);
				WriteBytes (stream, BitConverter.GetBytes ((long)obj));
			} else {
				Utils.Panic ();
			}
		}

		static object ReadConst (Stream stream)
		{
			var disc = stream.ReadByte ();
			if (disc == 1) {
				return ReadString (stream);
			} else if (disc == 2) {
				var byt = stream.ReadByte ();
				return byt == 1;
			} else if (disc == 3) {
				var bytes = new byte[8];
				stream.Read (bytes, 0, 8);
				return BitConverter.ToDouble (bytes, 0);
			} else if (disc == 4) {
				var bytes = new byte[8];
				stream.Read (bytes, 0, 8);
				return BitConverter.ToInt64 (bytes, 0);
			} else {
				Utils.Panic ();
				return null;
			}
		}

		static void ReadArguments (Stream s, Instruction instruction)
		{
			switch (instruction.Opcode) {
			case Instruction.Opcodes.VmVersion:
				instruction.Arguments = ReadInt (s);
				break;
			case Instruction.Opcodes.VmSourcesMd5:
				instruction.Arguments = ReadString (s);
				break;
			case Instruction.Opcodes.VmBytecodeMd5:
				instruction.Arguments = ReadString (s);
				break;
			case Instruction.Opcodes.FileName:
				instruction.Arguments = ReadString (s);
				break;
			case Instruction.Opcodes.Prim0:
				instruction.Arguments = ReadString (s);
				break;
			case Instruction.Opcodes.Return:
				break;
			case Instruction.Opcodes.Pop:
				break;
			case Instruction.Opcodes.Prim:
				instruction.Arguments = ReadString (s);
				break;
			case Instruction.Opcodes.Const:
				instruction.Arguments = ReadConst (s);
				break;
			case Instruction.Opcodes.Context:
				break;
			case Instruction.Opcodes.BlockReturn:
				break;
			case Instruction.Opcodes.Block:
				instruction.Arguments = ReadInt (s);
				break;
			case Instruction.Opcodes.Label:
				Utils.Panic ();
				break;
			case Instruction.Opcodes.PopBlock:
				break;
			case Instruction.Opcodes.Call:
				instruction.Arguments = ReadInt (s);
				break;
			case Instruction.Opcodes.CallJ:
				instruction.Arguments = ReadInt (s);
				break;
			case Instruction.Opcodes.Lget:
				instruction.Arguments = ReadArrayOfTwoInts (s);
				break;
			case Instruction.Opcodes.Fjump:
				instruction.Arguments = ReadInt (s);
				break;
			case Instruction.Opcodes.Jump:
				instruction.Arguments = ReadInt (s);
				break;
			case Instruction.Opcodes.Lset:
				instruction.Arguments = ReadArrayOfTwoInts (s);
				break;
			case Instruction.Opcodes.Fn:
				instruction.Arguments = ReadArrayOfTwoInts (s);
				break;
			case Instruction.Opcodes.NewFrame:
				instruction.Arguments = ReadArrayOfStrings (s);
				break;
			case Instruction.Opcodes.Args:
				instruction.Arguments = ReadInt (s);
				break;
			case Instruction.Opcodes.DropFrame:
				break;
			case Instruction.Opcodes.Tjump:
				instruction.Arguments = ReadInt (s);
				break;
			default:
				Utils.Panic ();
				break;
			}
		}

		static void WriteArguments (Stream stream, Instruction instruction)
		{
			switch (instruction.Opcode) {
			case Instruction.Opcodes.VmVersion:
				WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
				break;
			case Instruction.Opcodes.VmSourcesMd5:
				WriteString (stream, (string)instruction.Arguments);
				break;
			case Instruction.Opcodes.VmBytecodeMd5:
				WriteString (stream, (string)instruction.Arguments);
				break;
			case Instruction.Opcodes.FileName:
				WriteString (stream, (string)instruction.Arguments);
				break;
			case Instruction.Opcodes.Prim0:
				WriteString (stream, (string)instruction.Arguments);
				break;
			case Instruction.Opcodes.Return:
				break;
			case Instruction.Opcodes.Pop:
				break;
			case Instruction.Opcodes.Prim:
				WriteString (stream, (string)instruction.Arguments);
				break;
			case Instruction.Opcodes.Const:
				WriteConst (stream, instruction.Arguments);
				break;
			case Instruction.Opcodes.Context:
				break;
			case Instruction.Opcodes.BlockReturn:
				break;
			case Instruction.Opcodes.Block:
				WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
				break;
			case Instruction.Opcodes.Label:
				Utils.Panic ();
				break;
			case Instruction.Opcodes.PopBlock:
				break;
			case Instruction.Opcodes.Call:
				WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
				break;
			case Instruction.Opcodes.CallJ:
				WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
				break;
			case Instruction.Opcodes.Lget:
				WriteArrayOfTwoInts (stream, instruction.Arguments);
				break;
			case Instruction.Opcodes.Fjump:
				WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
				break;
			case Instruction.Opcodes.Jump:
				WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
				break;
			case Instruction.Opcodes.Lset:
				WriteArrayOfTwoInts (stream, instruction.Arguments);
				break;
			case Instruction.Opcodes.Fn:
				WriteArrayOfTwoInts (stream, instruction.Arguments);
				break;
			case Instruction.Opcodes.NewFrame:
				WriteArrayOfStrings (stream, (string[])instruction.Arguments);
				break;
			case Instruction.Opcodes.Args:
				WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
				break;
			case Instruction.Opcodes.DropFrame:
				break;
			case Instruction.Opcodes.Tjump:
				WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
				break;
			default:
				Utils.Panic ();
				break;
			}
		}

		static void WriteArrayOfStrings (Stream stream, string[] strings)
		{
			WriteBytes (stream, BitConverter.GetBytes ((int)strings.Length));
			foreach (var str in strings) {
				WriteString (stream, str);
			}
		}

		static string[] ReadArrayOfStrings (Stream stream)
		{
			var count = ReadInt (stream);
			var result = new string[count];
			for (var i = 0; i < count; i++) {
				result [i] = ReadString (stream);
			}
			return result;
		}

		static void WriteArrayOfTwoInts (Stream stream, object arguments)
		{
			var array = (int[])arguments;
			WriteBytes (stream, BitConverter.GetBytes (array [0]));
			WriteBytes (stream, BitConverter.GetBytes (array [1]));
		}

		static int[] ReadArrayOfTwoInts (Stream stream)
		{
			var array = new int[2];
			array [0] = ReadInt (stream);
			array [1] = ReadInt (stream);
			return array;
		}

		static void SerializeNullableInt (Stream stream, int? nullableInt)
		{
			if (nullableInt.HasValue) {
				WriteBytes (stream, BitConverter.GetBytes (nullableInt.Value));
			} else {
				WriteBytes (stream, minusOne);
			}
		}

		static int? DeserializeNullableInt (Stream s)
		{
			var candidate = ReadInt (s);
			if (candidate == -1) {
				return null;
			} else {
				return candidate;
			}
		}

	}
}

