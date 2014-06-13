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
using System.IO;
using Shovel.Vm.Types;
using System.Text;
using System.Linq;

namespace Shovel.Serialization
{
    internal class VmStateSerializer
    {
        #region Inner Types
        enum Types
        {
            ShortString,
            String,
            ShortInteger,
            Integer,
            LongInteger,
            Double,
            ShortComposite,
            Composite,
        }

        enum ObjectTypes
        {
            ShovelValueList,
            StringArray,
            ShovelValueArray,
            //List,
            Hash,
            Callable,
            ReturnAddress,
            NamedBlock,
            Environment,
            EnvFrame,
            Struct,
            StructInstance
        }

        class Composite
        {
            internal ObjectTypes Kind { get; set; }

            internal int[] Elements { get; set; }
        }
        #endregion

        #region Private Storage
        Dictionary<object, int> hash;
        List<object> array;
        #endregion

        #region Public API
        internal VmStateSerializer ()
        {
            this.hash = new Dictionary<object, int> ();
            this.array = new List<object> ();
        }

        internal void WriteToStream (Stream s)
        {
            var bs = new BinaryWriter (s);
            bs.Write ((int)this.array.Count);
            foreach (var obj in this.array) {
                if (obj is string) {
                    WriteString (bs, (string)obj);
                } else if (obj is long) {
                    WriteLong (bs, (long)obj);
                } else if (obj is int) {
                    WriteLong (bs, (int)obj);
                } else if (obj is Double) {
                    WriteDouble (bs, (double)obj);
                } else if (obj is Composite) {
                    WriteComposite (bs, (Composite)obj);
                } else {
                    Shovel.Utils.Panic ();
                }
            }
            bs.Flush ();
        }

        internal int Serialize (object obj)
        {
            if (obj == null) {
                return SerializeNull ();
            } else if (this.hash.ContainsKey (obj)) {
                return this.hash [obj];
            } else if (obj is Value) {
                return SerializeShovelValue ((Value)obj, obj);
            } else if (obj is string[]) {
                return SerializeStringArray ((string[])obj);
            } else if (obj is Value[]) {
                return SerializeShovelValueArray ((Value[])obj);
            } else if (obj is VmEnvironment) {
                return SerializeEnvironment ((VmEnvironment)obj, obj);
            } else if (obj is VmEnvFrame) {
                return SerializeEnvFrame ((VmEnvFrame)obj, obj);
            } else if (obj is Struct) {
                return SerializeStruct ((Struct)obj, obj);
            } else if (obj is StructInstance) {
                return SerializeStructInstance ((StructInstance)obj, obj);
            }
            Shovel.Utils.Panic ();
            throw new InvalidOperationException ();
        }

        internal static MemoryStream SerializeVmState (Vm.Vm vm)
        {
            return Utils.SerializeWithMd5CheckSum (str => {
                vm.SerializeState (str);
            });
        }

        internal void Deserialize (Stream s, int version, Action<Func<int, object>> action)
        {
            var length = Utils.ReadInt (s);
            var serArray = new object[length];
            var objects = new object[length];
            var br = new BinaryReader(s);
            for (var i = 0; i < length; i++) {
                serArray [i] = ReadValue (br);
            }
            Func<int, object> reader = null;
            reader = (index) => {
                if (index < 0) {
                    if (index == -1) {
                        return null;
                    } else if (index == -2) {
                        return true;
                    } else if (index == -3) {
                        return false;
                    } else {
                        Shovel.Utils.Panic ();
                        throw new InvalidOperationException ();
                    }
                }
                if (objects [index] == null) {
                    if (serArray [index] is string || serArray [index] is long || serArray [index] is double) {
                        objects [index] = serArray [index];
                    } else if (serArray [index] is Composite) {
                        RebuildFromComposite (objects, index, (Composite)serArray [index], reader, version);
                    } else {
                        Shovel.Utils.Panic ();
                        throw new NotImplementedException ();
                    }
                } 
                return objects [index];
            };
            action (reader);
        }
        #endregion

        #region Private Helper Functions - Deserializer

        object ReadShortString (BinaryReader br)
        {
            var length = br.ReadByte ();
            AdjustBigBuf(length);
            br.Read (bigBuf, 0, length);
            return Encoding.UTF8.GetString (bigBuf, 0, length);
        }

        object ReadString (BinaryReader br)
        {
            var length = ReadInteger (br);
            AdjustBigBuf(length);
            br.Read (bigBuf, 0, length);
            return Encoding.UTF8.GetString (bigBuf, 0, length);
        }

        byte[] bigBuf = new byte[1024];

        short ReadShortInteger (BinaryReader br)
        {
            return br.ReadInt16();
        }

        int ReadInteger (BinaryReader br)
        {
            return br.ReadInt32();
        }

        long ReadLongInteger (BinaryReader br)
        {
            return br.ReadInt64();
        }

        double ReadDouble (BinaryReader br)
        {
            return br.ReadDouble();
        }

        void AdjustBigBuf (int lengthInBytes)
        {
            if (bigBuf.Length < lengthInBytes) {
                bigBuf = new byte[lengthInBytes];
            }
        }

        Composite ReadCompositeImpl (BinaryReader br, int length)
        {
            var result = new Composite ();
            result.Kind = (ObjectTypes)br.ReadByte ();
            var lengthInBytes = 4 * length;
            AdjustBigBuf (lengthInBytes);
            br.Read (bigBuf, 0, lengthInBytes);
            result.Elements = new int[length];
            Buffer.BlockCopy (bigBuf, 0, result.Elements, 0, lengthInBytes);
            return result;
        }

        Composite ReadShortComposite (BinaryReader br)
        {
            var length = br.ReadByte ();
            return ReadCompositeImpl (br, length);
        }

        Composite ReadComposite (BinaryReader br)
        {
            var length = ReadInteger (br);
            return ReadCompositeImpl (br, length);
        }

        object ReadValue (BinaryReader br)
        {
            var type = (Types)br.ReadByte ();
            switch (type) {
            case Types.ShortString:
                return ReadShortString (br);
            case Types.String:
                return ReadString (br);
            case Types.ShortInteger:
                return (long)ReadShortInteger (br);
            case Types.Integer:
                return (long)ReadInteger (br);
            case Types.LongInteger:
                return (long)ReadLongInteger (br);
            case Types.Double:
                return ReadDouble (br);
            case Types.ShortComposite:
                return ReadShortComposite (br);
            case Types.Composite:
                return ReadComposite (br);
            default:
                Shovel.Utils.Panic ();
                throw new InvalidOperationException ();
            }
        }

        Value RebuildShovelValue (object par)
        {
            if (par is long) {
                return Value.MakeInt ((long)par);
            } else if (par is string) {
                return Value.Make ((string)par);
            } else if (par is double) {
                return Value.MakeFloat ((double)par);
            } else if (par == null) {
                return Value.Make ();
            } else if (par is bool) {
                return Value.Make ((bool)par);
            } else if (par is ArrayInstance) {
                return Value.Make ((ArrayInstance)par);
            } else if (par is HashInstance) {
                return Value.Make ((HashInstance)par);
            } else if (par is Callable) {
                return Value.Make ((Callable)par);
            } else if (par is ReturnAddress) {
                return Value.Make ((ReturnAddress)par);
            } else if (par is NamedBlock) {
                return Value.Make ((NamedBlock)par);
            } else if (par is Struct) {
                return Value.Make ((Struct)par);
            } else if (par is StructInstance) {
                return Value.Make ((StructInstance)par);
            } else {
                Shovel.Utils.Panic ();
                throw new InvalidOperationException ();
            }
        }

        ArrayInstance RebuildShovelValueList (
            object[] objects, int index, Composite composite, Func<int, object> reader, int version)
        {
            var result = new ArrayInstance ();
            objects [index] = result;

            var length = composite.Elements.Length;
            if (version > 4)
            {
                length -= 2;
            }

            for (var i = 0; i < length; i++) {
                result.Add (RebuildShovelValue (reader (composite.Elements [i])));
            }

            if (version > 4)
            {
                // The indirect get/set are stored as the last elements of the array.
                result.IndirectGet = RebuildShovelValue(reader(composite.Elements[length])).CallableValue;
                result.IndirectSet = RebuildShovelValue(reader(composite.Elements[length + 1])).CallableValue;
            }
            return result;
        }

        string[] RebuildStringArray (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new string[composite.Elements.Length];
            objects [index] = result;

            for (var i = 0; i < composite.Elements.Length; i++) {
                result [i] = (string)reader (composite.Elements [i]);
            }
            return result;
        }

        Value[] RebuildShovelValueArray (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new Value[composite.Elements.Length];
            objects [index] = result;

            for (var i = 0; i < composite.Elements.Length; i++) {
                result [i] = RebuildShovelValue (reader (composite.Elements [i]));
            }
            return result;
        }

        HashInstance RebuildHash (
            object[] objects, int index, Composite composite, Func<int, object> reader, int version)
        {
            var result = new HashInstance ();
            objects [index] = result;

            var length = composite.Elements.Length;
            if (version > 4)
            {
                length -= 2;
            }
            for (var i = 0; i < length; i+=2) {
                var key = RebuildShovelValue (reader (composite.Elements [i]));
                var value = RebuildShovelValue (reader (composite.Elements [i + 1]));
                result [key] = value;
            }
            if (version > 4)
            {
                // The indirect get/set are stored as the last elements of the hash.
                result.IndirectGet = RebuildShovelValue(reader(composite.Elements[length])).CallableValue;
                result.IndirectSet = RebuildShovelValue(reader(composite.Elements[length + 1])).CallableValue;
            }
            return result;
        }

        Callable RebuildCallable (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new Callable ();
            objects [index] = result;

            result.UdpName = (string)reader (composite.Elements [0]);
            result.Prim0Name = (string)reader (composite.Elements [1]);
            result.Arity = (int?)(long?)reader (composite.Elements [2]);
            result.ProgramCounter = (int?)(long?)reader (composite.Elements [3]);
            result.Environment = (VmEnvironment)reader (composite.Elements [4]);

            return result;
        }

        ReturnAddress RebuildReturnAddress (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new ReturnAddress ();
            objects [index] = result;

            result.Environment = (VmEnvironment)reader (composite.Elements [0]);
            result.ProgramCounter = (int)(long)reader (composite.Elements [1]);

            return result;
        }

        NamedBlock RebuildNamedBlock (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new NamedBlock ();
            objects [index] = result;

            result.Name = (string)reader (composite.Elements [0]);
            result.BlockEnd = (int)(long)reader (composite.Elements [1]);
            result.Environment = (VmEnvironment)reader (composite.Elements [2]);

            return result;
        }

        VmEnvironment RebuildEnvironment (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new VmEnvironment ();
            objects [index] = result;

            result.Frame = (VmEnvFrame)reader (composite.Elements [0]);
            result.Next = (VmEnvironment)reader (composite.Elements [1]);

            return result;
        }

        VmEnvFrame RebuildEnvFrame (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new VmEnvFrame ();
            objects [index] = result;

            result.VarNames = (string[])reader (composite.Elements [0]);
            result.Values = (Value[])reader (composite.Elements [1]);
            result.IntroducedAtProgramCounter = (int)(long)reader (composite.Elements [2]);

            return result;
        }

        Struct RebuildStruct (object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new Struct ();
            objects [index] = result;

            result.Fields = (string[])reader (composite.Elements [0]);

            return result;
        }

        StructInstance RebuildStructInstance (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new StructInstance ();
            objects [index] = result;

            result.Struct = (Struct)reader (composite.Elements [0]);
            result.Values = (Value[])reader (composite.Elements [1]);

            return result;
        }

        void RebuildFromComposite (
            object[] objects, int index, Composite composite, Func<int, object> reader, int version)
        {
            switch (composite.Kind) {
            case ObjectTypes.ShovelValueList:
                RebuildShovelValueList (objects, index, composite, reader, version);
                break;
            case ObjectTypes.StringArray:
                RebuildStringArray (objects, index, composite, reader);
                break;
            case ObjectTypes.ShovelValueArray:
                RebuildShovelValueArray (objects, index, composite, reader);
                break;
            case ObjectTypes.Hash:
                RebuildHash (objects, index, composite, reader, version);
                break;
            case ObjectTypes.Callable:
                RebuildCallable (objects, index, composite, reader);
                break;
            case ObjectTypes.ReturnAddress:
                RebuildReturnAddress (objects, index, composite, reader);
                break;
            case ObjectTypes.NamedBlock:
                RebuildNamedBlock (objects, index, composite, reader);
                break;
            case ObjectTypes.Environment:
                RebuildEnvironment (objects, index, composite, reader);
                break;
            case ObjectTypes.EnvFrame:
                RebuildEnvFrame (objects, index, composite, reader);
                break;
            case ObjectTypes.Struct:
                RebuildStruct (objects, index, composite, reader);
                break;
            case ObjectTypes.StructInstance:
                RebuildStructInstance (objects, index, composite, reader);
                break;
            default:
                Shovel.Utils.Panic ();
                break;
            }
        }
        #endregion


        #region Private Helper Functions - Serializer
        int SerializeNull ()
        {
            return -1;
        }

        int SerializeBool (bool b)
        {
            if (b) {
                return -2;
            } else {
                return -3;
            }
        }

        int SerializeOne (object obj)
        {
            if (obj == null) {
                return SerializeNull ();
            }
            this.array.Add (obj);
            return this.array.Count - 1;
        }

        int SerializeOneHashed (object obj, object storeAs = null)
        {
            if (storeAs == null) {
                storeAs = obj;
            }
            if (storeAs == null) {
                return SerializeNull ();
            }
            if (this.hash.ContainsKey (storeAs)) {
                return this.hash [storeAs];
            }
            var result = SerializeOne (obj);
            this.hash [storeAs] = result;
            return result;
        }

        int SerializeShovelValueArray (Value[] array)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.ShovelValueArray,
                Elements = new int[array.Length] 
            };
            var result = SerializeOneHashed (composite, array);
            for (var i = 0; i < array.Length; i++) {
                composite.Elements [i] = Serialize (array [i]);
            }
            return result;
        }

        int SerializeStringArray (string[] array)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.StringArray, 
                Elements = new int[array.Length] 
            };
            var result = SerializeOneHashed (composite, array);
            for (var i = 0; i < array.Length; i++) {
                composite.Elements [i] = SerializeOneHashed (array [i]);
            }
            return result;
        }

        int SerializeHash (HashInstance dict, object obj)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.Hash, 
                Elements = new int[dict.Count * 2 + 2] 
            };
            var result = SerializeOneHashed (composite, obj);
            var cursor = 0;
            foreach (var kv in dict) {
                composite.Elements [cursor] = Serialize (kv.Key);
                composite.Elements [cursor + 1] = Serialize (kv.Value);
                cursor += 2;
            }
            composite.Elements[cursor] = Serialize(dict.IndirectGet);
            cursor++;
            composite.Elements[cursor] = Serialize(dict.IndirectSet);
            cursor++;
            return result;
        }

        int SerializeList (ArrayInstance list, object obj)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.ShovelValueList, 
                Elements = new int[list.Count + 2] 
            };
            var result = SerializeOneHashed (composite, obj);
            var i = 0;
            for (i = 0; i < list.Count; i++) {
                composite.Elements [i] = Serialize (list [i]);
            }
            composite.Elements[i] = Serialize(list.IndirectGet);
            i++;
            composite.Elements[i] = Serialize(list.IndirectSet);
            i++;
            return result;
        }

        int SerializeCallable (Callable callable, object obj)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.Callable, 
                Elements = new int[5] 
            };
            var result = SerializeOneHashed (composite, obj);
            composite.Elements [0] = SerializeOneHashed (callable.UdpName);
            composite.Elements [1] = SerializeOneHashed (callable.Prim0Name);
            composite.Elements [2] = SerializeOne (callable.Arity);
            composite.Elements [3] = SerializeOne (callable.ProgramCounter);
            composite.Elements [4] = Serialize (callable.Environment);
            return result;
        }

        int SerializeReturnAddress (ReturnAddress returnAddress, object obj)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.ReturnAddress, 
                Elements = new int[2] 
            };
            var result = SerializeOneHashed (composite, obj);
            composite.Elements [0] = Serialize (returnAddress.Environment);
            composite.Elements [1] = SerializeOne (returnAddress.ProgramCounter);
            return result;
        }

        int SerializeNamedBlock (NamedBlock namedBlock, object obj)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.NamedBlock,
                Elements = new int[3] 
            };
            var result = SerializeOneHashed (composite, obj);
            composite.Elements [0] = SerializeOneHashed (namedBlock.Name);
            composite.Elements [1] = SerializeOne (namedBlock.BlockEnd);
            composite.Elements [2] = Serialize (namedBlock.Environment);
            return result;
        }

        int SerializeEnvironment (VmEnvironment env, object obj)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.Environment, 
                Elements = new int[2] 
            };
            var result = SerializeOneHashed (composite, obj);
            composite.Elements [0] = Serialize (env.Frame);
            composite.Elements [1] = Serialize (env.Next);
            return result;
        }

        int SerializeEnvFrame (VmEnvFrame frame, object obj)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.EnvFrame, 
                Elements = new int[3] 
            };
            var result = SerializeOneHashed (composite, obj);
            composite.Elements [0] = Serialize (frame.VarNames);
            composite.Elements [1] = Serialize (frame.Values);
            composite.Elements [2] = SerializeOne (frame.IntroducedAtProgramCounter);
            return result;
        }

        int SerializeStruct (Struct ztruct, object obj)
        {
            var composite = new Composite {
                Kind = ObjectTypes.Struct,
                Elements = new int[1]
            };
            var result = SerializeOneHashed (composite, obj);
            composite.Elements [0] = SerializeStringArray (ztruct.Fields);
            return result;
        }

        int SerializeStructInstance (StructInstance structInstance, object obj)
        {
            var composite = new Composite {
                Kind = ObjectTypes.StructInstance,
                Elements = new int[2]
            };
            var result = SerializeOneHashed (composite, obj);
            composite.Elements [0] = Serialize (structInstance.Struct);
            composite.Elements [1] = SerializeShovelValueArray (structInstance.Values);
            return result;
        }

        int SerializeShovelValue (Value sv, object obj)
        {
            switch (sv.Kind) {
            case Value.Kinds.Null:
                return SerializeNull ();
            case Value.Kinds.Bool:
                return SerializeBool (sv.BoolValue);
            case Value.Kinds.String:
                return SerializeOne (sv.StringValue);
            case Value.Kinds.Integer:
                return SerializeOne (sv.IntegerValue);
            case Value.Kinds.Double:
                return SerializeOne (sv.DoubleValue);
            case Value.Kinds.Array:
                return SerializeList (sv.ArrayValue, obj);
            case Value.Kinds.Hash:
                return SerializeHash (sv.HashValue, obj);
            case Value.Kinds.Callable:
                return SerializeCallable (sv.CallableValue, obj);
            case Value.Kinds.ReturnAddress:
                return SerializeReturnAddress (sv.ReturnAddressValue, obj);
            case Value.Kinds.NamedBlock:
                return SerializeNamedBlock (sv.NamedBlockValue, obj);
            case Value.Kinds.Struct:
                return SerializeStruct (sv.StructValue, obj);
            case Value.Kinds.StructInstance:
                return SerializeStructInstance (sv.StructInstanceValue, obj);
            default:
                Shovel.Utils.Panic ();
                throw new InvalidOperationException ();
            }
        }

        void WriteString (BinaryWriter bs, string str)
        {
            byte[] bytes = Encoding.UTF8.GetBytes (str);
            if (bytes.Length <= 255) {
                bs.Write ((byte)Types.ShortString);
                bs.Write ((byte)bytes.Length);
            } else {
                bs.Write ((byte)Types.String);
                bs.Write (bytes.Length);
            }
            bs.Write (bytes);
        }

        void WriteLong (BinaryWriter bs, long li)
        {
            if (li <= Int16.MaxValue && li >= Int16.MinValue) {
                bs.Write ((byte)Types.ShortInteger);
                bs.Write ((ushort)li);
            } else if (li <= Int32.MaxValue && li >= Int32.MinValue) {
                bs.Write ((byte)Types.Integer);
                bs.Write ((int)li);
            } else {
                bs.Write ((byte)Types.LongInteger);
                bs.Write (li);
            }
        }

        void WriteDouble (BinaryWriter bs, double d)
        {
            bs.Write ((byte)Types.Double);
            bs.Write (d);
        }

        void WriteComposite (BinaryWriter bs, Composite composite)
        {
            int length = composite.Elements.Length;
            if (length < 255) {
                bs.Write ((byte)Types.ShortComposite);
                bs.Write ((byte)length);
            } else {
                bs.Write ((byte)Types.Composite);
                bs.Write (length);
            }
            bs.Write ((byte)composite.Kind);
            var lengthInBytes = 4 * composite.Elements.Length;
            AdjustBigBuf (lengthInBytes);
            Buffer.BlockCopy (composite.Elements, 0, bigBuf, 0, lengthInBytes);
            bs.Write (bigBuf, 0, lengthInBytes);
        }
        #endregion

    }
}

