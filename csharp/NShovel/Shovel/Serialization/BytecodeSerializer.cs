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
using Shovel.Exceptions;

namespace Shovel.Serialization
{
    public static class BytecodeSerializer
    {
        static byte[] minusOne = BitConverter.GetBytes ((int)-1);

        static internal Instruction[] DeserializeBytecode (MemoryStream ms)
        {
            return (Instruction[])Utils.DeserializeWithMd5CheckSum (ms, str => {
                return DeserializeBytecodeImpl (str);
            }
            );
        }

        static private object DeserializeBytecodeImpl (Stream s)
        {
            // Read the version and throw if it's not supported.
            int version = Utils.ReadInt(s);
            if (version > Shovel.Api.Version) {
                throw new Exceptions.VersionNotSupportedException();
            }
            // Read the number of instructions.
            int instructionsCount = Utils.ReadInt (s);
            // Allocate the array of instructions.
            var result = new Instruction[instructionsCount];
            // Fill in instructions.
            for (var i = 0; i < instructionsCount; i++) {
                result [i] = ReadInstruction (s);
            }
            return result;
        }

        static internal MemoryStream SerializeBytecode (Instruction[] bytecode)
        {
            return Utils.SerializeWithMd5CheckSum (str => {
                SerializeBytecodeImplementation (str, bytecode);
            }
            );
        }

        static private void SerializeBytecodeImplementation (Stream ms, Instruction[] bytecode)
        {
            Utils.WriteBytes (ms, BitConverter.GetBytes (Shovel.Api.Version));
            Utils.WriteBytes (ms, BitConverter.GetBytes ((int)bytecode.Length));
            foreach (var instruction in bytecode) {
                Utils.WriteBytes (ms, BitConverter.GetBytes ((int)instruction.NumericOpcode));
                SerializeNullableInt (ms, instruction.StartPos);
                SerializeNullableInt (ms, instruction.EndPos);
                Utils.WriteBytes (ms, BitConverter.GetBytes ((int)instruction.Opcode));
                WriteArguments (ms, instruction);
            }
        }

        static Instruction ReadInstruction (Stream s)
        {
            var instruction = new Instruction ();
            instruction.NumericOpcode = Utils.ReadInt (s);
            instruction.StartPos = DeserializeNullableInt (s);
            instruction.EndPos = DeserializeNullableInt (s);
            var opcodeInt = Utils.ReadInt (s);
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
                Utils.WriteBytes (stream, bytes);
            } else {
                stream.WriteByte (4);
                Utils.WriteBytes (stream, BitConverter.GetBytes ((int)bytes.Length));
                Utils.WriteBytes (stream, bytes);
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
                var size = Utils.ReadInt (s);
                var bytes = new byte[size];
                s.Read (bytes, 0, size);
                return Encoding.UTF8.GetString (bytes);
            } else {
                Shovel.Utils.Panic ();
                return null;
            }
        }

        static void WriteConst (Stream stream, Value sv)
        {
            stream.WriteByte ((byte)sv.Kind);
            if (sv.Kind == Value.Kinds.Integer) {
                Utils.WriteBytes (stream, BitConverter.GetBytes (sv.IntegerValue));
            } else if (sv.Kind == Value.Kinds.Double) {
                Utils.WriteBytes (stream, BitConverter.GetBytes (sv.IntegerValue));
            } else if (sv.Kind == Value.Kinds.String) {
                WriteString (stream, sv.StringValue);
            } else if (sv.Kind == Value.Kinds.Bool) {
                stream.WriteByte (sv.BoolValue ? (byte)1 : (byte)0);
            } else if (sv.Kind == Value.Kinds.Null) {
                // Do nothing.
            } else {
                Shovel.Utils.Panic ();
            }
        }

        static Value ReadConst (Stream stream)
        {
            var result = Value.Make ();
            result.Kind = (Value.Kinds)stream.ReadByte ();
            if (result.Kind == Value.Kinds.Integer) {
                result.IntegerValue = Utils.ReadLong (stream);
            } else if (result.Kind == Value.Kinds.Double) {
                // FIXME: these allocate a lot of byte[] objects.
                // Should find a way to avoid this (have the caller pass the byte[]?).
                var bytes = new byte[8];
                stream.Read (bytes, 0, 8);
                result.DoubleValue = BitConverter.ToDouble (bytes, 0);
            } else if (result.Kind == Value.Kinds.String) {
                result.StringValue = ReadString(stream);
            } else if (result.Kind == Value.Kinds.Bool) {
                result.BoolValue = stream.ReadByte() == 1;
            } else if (result.Kind == Value.Kinds.Null) {
                // Do nothing.
            } else {
                Shovel.Utils.Panic ();
            }
            return result;
        }

        static void ReadArguments (Stream s, Instruction instruction)
        {
            switch (instruction.Opcode) {
            case Instruction.Opcodes.VmVersion:
                instruction.Arguments = Utils.ReadInt (s);
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
            case Instruction.Opcodes.Prim:
                instruction.Arguments = ReadString (s);
                break;
            case Instruction.Opcodes.Const:
                instruction.Arguments = ReadConst (s);
                break;
            case Instruction.Opcodes.Block:
                instruction.Arguments = Utils.ReadInt (s);
                break;
            case Instruction.Opcodes.Label:
                Shovel.Utils.Panic ();
                break;
            case Instruction.Opcodes.Call:
                instruction.Arguments = Utils.ReadInt (s);
                break;
            case Instruction.Opcodes.CallJ:
                instruction.Arguments = Utils.ReadInt (s);
                break;
            case Instruction.Opcodes.Lget:
                instruction.Arguments = ReadArrayOfTwoInts (s);
                break;
            case Instruction.Opcodes.Fjump:
                instruction.Arguments = Utils.ReadInt (s);
                break;
            case Instruction.Opcodes.Jump:
                instruction.Arguments = Utils.ReadInt (s);
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
                instruction.Arguments = Utils.ReadInt (s);
                break;
            case Instruction.Opcodes.Tjump:
                instruction.Arguments = Utils.ReadInt (s);
                break;
            }
        }

        static void WriteArguments (Stream stream, Instruction instruction)
        {
            switch (instruction.Opcode) {
            case Instruction.Opcodes.VmVersion:
                Utils.WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
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
            case Instruction.Opcodes.Prim:
                WriteString (stream, (string)instruction.Arguments);
                break;
            case Instruction.Opcodes.Const:
                WriteConst (stream, (Value)instruction.Arguments);
                break;
            case Instruction.Opcodes.Block:
                Utils.WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
                break;
            case Instruction.Opcodes.Label:
                Shovel.Utils.Panic ();
                break;
            case Instruction.Opcodes.Call:
                Utils.WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
                break;
            case Instruction.Opcodes.CallJ:
                Utils.WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
                break;
            case Instruction.Opcodes.Lget:
                WriteArrayOfTwoInts (stream, instruction.Arguments);
                break;
            case Instruction.Opcodes.Fjump:
                Utils.WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
                break;
            case Instruction.Opcodes.Jump:
                Utils.WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
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
                Utils.WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
                break;
            case Instruction.Opcodes.Tjump:
                Utils.WriteBytes (stream, BitConverter.GetBytes ((int)instruction.Arguments));
                break;
            }
        }

        static void WriteArrayOfStrings (Stream stream, string[] strings)
        {
            Utils.WriteBytes (stream, BitConverter.GetBytes ((int)strings.Length));
            foreach (var str in strings) {
                WriteString (stream, str);
            }
        }

        static string[] ReadArrayOfStrings (Stream stream)
        {
            var count = Utils.ReadInt (stream);
            var result = new string[count];
            for (var i = 0; i < count; i++) {
                result [i] = ReadString (stream);
            }
            return result;
        }

        static void WriteArrayOfTwoInts (Stream stream, object arguments)
        {
            var array = (int[])arguments;
            Utils.WriteBytes (stream, BitConverter.GetBytes (array [0]));
            Utils.WriteBytes (stream, BitConverter.GetBytes (array [1]));
        }

        static int[] ReadArrayOfTwoInts (Stream stream)
        {
            var array = new int[2];
            array [0] = Utils.ReadInt (stream);
            array [1] = Utils.ReadInt (stream);
            return array;
        }

        static void SerializeNullableInt (Stream stream, int? nullableInt)
        {
            if (nullableInt.HasValue) {
                Utils.WriteBytes (stream, BitConverter.GetBytes (nullableInt.Value));
            } else {
                Utils.WriteBytes (stream, minusOne);
            }
        }

        static int? DeserializeNullableInt (Stream s)
        {
            var candidate = Utils.ReadInt (s);
            if (candidate == -1) {
                return null;
            } else {
                return candidate;
            }
        }

    }
}

