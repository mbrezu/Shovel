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
            EnvFrame
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
            Utils.WriteBytes (s, BitConverter.GetBytes ((int)this.array.Count));
            foreach (var obj in this.array) {
                if (obj is string) {
                    WriteString (s, (string)obj);
                } else if (obj is long) {
                    WriteLong (s, (long)obj);
                } else if (obj is int) {
                    WriteLong (s, (int)obj);
                } else if (obj is Double) {
                    WriteDouble (s, (double)obj);
                } else if (obj is Composite) {
                    WriteComposite (s, (Composite)obj);
                } else {
                    Shovel.Utils.Panic ();
                }
            }
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
                return SerializeStringArray (obj, (string[])obj);
            } else if (obj is Value[]) {
                return SerializeShovelValueArray (obj, (Value[])obj);
            } else if (obj is VmEnvironment) {
                return SerializeEnvironment ((VmEnvironment)obj, obj);
            } else if (obj is VmEnvFrame) {
                return SerializeEnvFrame ((VmEnvFrame)obj, obj);
            }
            Shovel.Utils.Panic ();
            throw new InvalidOperationException ();
        }

        internal static MemoryStream SerializeVmState (Vm.Vm vm)
        {
            return Utils.SerializeWithMd5CheckSum (str => {
                vm.SerializeState (str);
            }
            );
        }

        internal void Deserialize (Stream s, Action<Func<int, object>> action)
        {
            var length = Utils.ReadInt (s);
            var serArray = new object[length];
            var objects = new object[length];
            for (var i = 0; i < length; i++) {
                serArray [i] = ReadValue (s);
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
                        RebuildFromComposite (objects, index, (Composite)serArray [index], reader);
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

        object ReadShortString (Stream s)
        {
            var length = s.ReadByte ();
            var bytes = new byte[length];
            s.Read (bytes, 0, length);
            return Encoding.UTF8.GetString (bytes);
        }

        object ReadString (Stream s)
        {
            var length = ReadInteger (s);
            var bytes = new byte[length];
            s.Read (bytes, 0, length);
            return Encoding.UTF8.GetString (bytes);
        }

        byte[] minibuf2 = new byte[2];
        byte[] minibuf4 = new byte[4];
        byte[] minibuf8 = new byte[8];

        short ReadShortInteger (Stream s)
        {
            s.Read (minibuf2, 0, 2);
            return BitConverter.ToInt16 (minibuf2, 0);
        }

        int ReadInteger (Stream s)
        {
            s.Read (minibuf4, 0, 4);
            return BitConverter.ToInt32 (minibuf4, 0);
        }

        long ReadLongInteger (Stream s)
        {
            s.Read (minibuf8, 0, 8);
            return BitConverter.ToInt64 (minibuf8, 0);
        }

        double ReadDouble (Stream s)
        {
            s.Read (minibuf8, 0, 8);
            return BitConverter.ToDouble (minibuf8, 0);
        }

        Composite ReadCompositeImpl (Stream s, int length)
        {
            var result = new Composite ();
            result.Kind = (ObjectTypes)s.ReadByte ();
            // SLOW: I should change this to read directly into an
            // array of bytes and copy it over into an array of ints.
            // Not sure that is possible :-)
            result.Elements = new int[length];
            for (var i = 0; i < length; i++) {
                result.Elements [i] = ReadInteger (s);
            }
            return result;
        }

        Composite ReadShortComposite (Stream s)
        {
            var length = s.ReadByte ();
            return ReadCompositeImpl (s, length);
        }

        Composite ReadComposite (Stream s)
        {
            var length = ReadInteger (s);
            return ReadCompositeImpl (s, length);
        }

        object ReadValue (Stream s)
        {
            var type = (Types)s.ReadByte ();
            switch (type) {
            case Types.ShortString:
                return ReadShortString (s);
            case Types.String:
                return ReadString (s);
            case Types.ShortInteger:
                return (long)ReadShortInteger (s);
            case Types.Integer:
                return (long)ReadInteger (s);
            case Types.LongInteger:
                return (long)ReadLongInteger (s);
            case Types.Double:
                return ReadDouble (s);
            case Types.ShortComposite:
                return ReadShortComposite (s);
            case Types.Composite:
                return ReadComposite (s);
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
            } else if (par is List<Value>) {
                return Value.Make ((List<Value>)par);
            } else if (par is Dictionary<Value, Value>) {
                return Value.Make ((Dictionary<Value, Value>)par);
            } else if (par is Callable) {
                return Value.Make ((Callable)par);
            } else if (par is ReturnAddress) {
                return Value.Make ((ReturnAddress)par);
            } else if (par is NamedBlock) {
                return Value.Make ((NamedBlock)par);
            } else {
                Shovel.Utils.Panic ();
                throw new InvalidOperationException ();
            }
        }

        List<Value> RebuildShovelValueList (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new List<Value> ();
            objects [index] = result;

            for (var i = 0; i < composite.Elements.Length; i++) {
                result.Add (RebuildShovelValue (reader (composite.Elements [i])));
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

        Dictionary<Value, Value> RebuildHash (
            object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            var result = new Dictionary<Value, Value> ();
            objects [index] = result;

            for (var i = 0; i < composite.Elements.Length; i+=2) {
                var key = RebuildShovelValue (reader (composite.Elements [i]));
                var value = RebuildShovelValue (reader (composite.Elements [i + 1]));
                result [key] = value;
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
            result.Uses = (int)(long)reader (composite.Elements [2]);

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

        void RebuildFromComposite (object[] objects, int index, Composite composite, Func<int, object> reader)
        {
            switch (composite.Kind) {
            case ObjectTypes.ShovelValueList:
                RebuildShovelValueList (objects, index, composite, reader);
                break;
            case ObjectTypes.StringArray:
                RebuildStringArray (objects, index, composite, reader);
                break;
            case ObjectTypes.ShovelValueArray:
                RebuildShovelValueArray (objects, index, composite, reader);
                break;
            case ObjectTypes.Hash:
                RebuildHash (objects, index, composite, reader);
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

        int SerializeShovelValueArray (object obj, Value[] array)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.ShovelValueArray,
                Elements = new int[array.Length] 
            };
            var result = SerializeOneHashed (composite, obj);
            for (var i = 0; i < array.Length; i++) {
                composite.Elements [i] = Serialize (array [i]);
            }
            return result;
        }

        int SerializeStringArray (object obj, string[] array)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.StringArray, 
                Elements = new int[array.Length] 
            };
            var result = SerializeOneHashed (composite, obj);
            for (var i = 0; i < array.Length; i++) {
                composite.Elements [i] = SerializeOneHashed (array [i]);
            }
            return result;
        }

        int SerializeHash (Dictionary<Value, Value> dict, object obj)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.Hash, 
                Elements = new int[dict.Count * 2] 
            };
            var result = SerializeOneHashed (composite, obj);
            var cursor = 0;
            foreach (var kv in dict) {
                composite.Elements [cursor] = Serialize (kv.Key);
                composite.Elements [cursor + 1] = Serialize (kv.Value);
                cursor += 2;
            }
            return result;
        }

        int SerializeList (List<Value> list, object obj)
        {
            var composite = new Composite { 
                Kind = ObjectTypes.ShovelValueList, 
                Elements = new int[list.Count] 
            };
            var result = SerializeOneHashed (composite, obj);
            for (var i = 0; i < list.Count; i++) {
                composite.Elements [i] = Serialize (list [i]);
            }
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
                Elements = new int[3] 
            };
            var result = SerializeOneHashed (composite, obj);
            composite.Elements [0] = Serialize (env.Frame);
            composite.Elements [1] = Serialize (env.Next);
            composite.Elements [2] = SerializeOne (env.Uses);
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
            default:
                Shovel.Utils.Panic ();
                throw new InvalidOperationException ();
            }
        }

        void WriteString (Stream s, string str)
        {
            byte[] bytes = Encoding.UTF8.GetBytes (str);
            if (bytes.Length <= 255) {
                s.WriteByte ((byte)Types.ShortString);
                s.WriteByte ((byte)bytes.Length);
            } else {
                s.WriteByte ((byte)Types.String);
                Utils.WriteBytes (s, BitConverter.GetBytes (bytes.Length));
            }
            Utils.WriteBytes (s, bytes);
        }

        void WriteLong (Stream s, long li)
        {
            if (li <= Int16.MaxValue && li >= Int16.MinValue) {
                s.WriteByte ((byte)Types.ShortInteger);
                Utils.WriteBytes (s, BitConverter.GetBytes ((ushort)li));
            } else if (li <= Int32.MaxValue && li >= Int32.MinValue) {
                s.WriteByte ((byte)Types.Integer);
                Utils.WriteBytes (s, BitConverter.GetBytes ((int)li));
            } else {
                s.WriteByte ((byte)Types.LongInteger);
                Utils.WriteBytes (s, BitConverter.GetBytes (li));
            }
        }

        void WriteDouble (Stream s, double d)
        {
            s.WriteByte ((byte)Types.Double);
            Utils.WriteBytes (s, BitConverter.GetBytes (d));
        }

        void WriteComposite (Stream s, Composite composite)
        {
            int length = composite.Elements.Length;
            if (length < 255) {
                s.WriteByte ((byte)Types.ShortComposite);
                s.WriteByte ((byte)length);
            } else {
                s.WriteByte ((byte)Types.Composite);
                Utils.WriteBytes (s, BitConverter.GetBytes (length));
            }
            s.WriteByte ((byte)composite.Kind);
            for (var i = 0; i < composite.Elements.Length; i++) {
                Utils.WriteBytes (s, BitConverter.GetBytes (composite.Elements [i]));
            }
        }
        #endregion

    }
}

