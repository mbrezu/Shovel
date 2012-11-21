using System;
using System.Collections.Generic;
using System.IO;
using Shovel.Vm.Types;

namespace Shovel.Serialization
{
    public class VmStateSerializer
    {
        public enum Types {
            ShortString,
            String,
            ShortInteger,
            Integer,
            Double,
            ShortArray,
            Array,
        }

        public enum ObjectTypes {
            ShovelValueList,
            StringArray,
            ShovelValueArray,
            List,
            Hash,
            Callable,
            ReturnAddress,
            NamedBlock
        }

        Dictionary<object, int> hash;
        List<object> array;

        public VmStateSerializer ()
        {
            this.hash = new Dictionary<object, int> ();
            this.array = new List<object> ();
        }

        public int SerializeNull ()
        {
            return -1;
        }

        public int SerializeBool (bool b)
        {
            if (b) {
                return -2;
            } else {
                return -3;
            }
        }

        public int SerializeOne (object obj)
        {
            this.array.Add (obj);
            return this.array.Count - 1;
        }

        public int SerializeOneHashed (object obj, object storeAs = null)
        {
            if (storeAs == null) {
                storeAs = obj;
            }
            if (this.hash.ContainsKey (storeAs)) {
                return this.hash [storeAs];
            }
            var result = SerializeOne (obj);
            this.hash [storeAs] = result;
            return result;
        }

        int SerializeArray<T> (object obj, T[] array, ObjectTypes otype)
        {
            var resultArray = new object[array.Length + 1];
            resultArray [0] = otype;
            var result = SerializeOneHashed (resultArray, obj);
            for (var i = 0; i < array.Length; i++) {
                resultArray [i + 1] = Serialize (array [i]);
            }
            return result;
        }

        int SerializeHash (object obj)
        {
            var dict = (Dictionary<ShovelValue, ShovelValue>)obj;
            var resultArray = new object[dict.Count * 2 + 1];
            resultArray [0] = ObjectTypes.Hash;
            var result = SerializeOneHashed (resultArray, obj);
            var cursor = 0;
            foreach (var kv in dict) {
                resultArray [cursor] = Serialize (kv.Key);
                resultArray [cursor + 1] = Serialize (kv.Value);
                cursor += 2;
            }
            return result;
        }

        int SerializeList (object obj)
        {
            var list = (List<ShovelValue>)obj;
            var resultArray = new object[list.Count + 1];
            resultArray [0] = ObjectTypes.ShovelValueList;
            var result = SerializeOneHashed (resultArray, obj);
            for (var i = 0; i < list.Count; i++) {
                resultArray [i + 1] = Serialize (list [i]);
            }
            return result;
        }

        int SerializeCallable (Callable callable)
        {
            var resultArray = new object[6];
            var result = SerializeOneHashed(resultArray);
            resultArray[0] = ObjectTypes.Callable;
            resultArray[1] = Serialize (callable.UdpName);
            resultArray[2] = Serialize (callable.Prim0Name);
            resultArray[3] = Serialize (callable.Arity);
            resultArray[4] = Serialize (callable.ProgramCounter);
            resultArray[5] = Serialize (callable.Environment);
            return result;
        }

        int SerializeReturnAddress (ReturnAddress returnAddress)
        {
            var resultArray = new object[3];
            var result = SerializeOneHashed(resultArray);
            resultArray[0] = ObjectTypes.ReturnAddress;
            resultArray[1] = Serialize (returnAddress.Environment);
            resultArray[2] = Serialize (returnAddress.ProgramCounter);
            return result;
        }

        int SerializeNamedBlock (NamedBlock namedBlock)
        {
            var resultArray = new object[4];
            var result = SerializeOneHashed(resultArray);
            resultArray[0] = ObjectTypes.NamedBlock;
            resultArray[1] = Serialize (namedBlock.Name);
            resultArray[2] = Serialize (namedBlock.BlockEnd);
            resultArray[3] = Serialize (namedBlock.Environment);
            return result;
        }

        public int Serialize (object obj)
        {
            if (obj is ShovelValue) {
                var sv = (ShovelValue)obj;
                switch (sv.Kind) {
                case ShovelValue.Kinds.Null:
                    return SerializeNull ();
                case ShovelValue.Kinds.Bool:
                    return SerializeBool (sv.BoolValue);
                case ShovelValue.Kinds.String:
                    return SerializeOne (sv.StringValue);
                case ShovelValue.Kinds.Integer:
                    return SerializeOne (sv.IntegerValue);
                case ShovelValue.Kinds.Double:
                    return SerializeOne (sv.DoubleValue);
                case ShovelValue.Kinds.Array:
                    return SerializeList (sv.ArrayValue);
                case ShovelValue.Kinds.Hash:
                    return SerializeHash (sv.HashValue);                
                case ShovelValue.Kinds.Callable:
                    return SerializeCallable (sv.CallableValue);
                case ShovelValue.Kinds.ReturnAddress:
                    return SerializeReturnAddress (sv.ReturnAddressValue);
                case ShovelValue.Kinds.NamedBlock:
                    return SerializeNamedBlock (sv.NamedBlockValue);
                default:
                    Utils.Panic ();
                    break;
                }
            } else if (obj is Dictionary<ShovelValue, ShovelValue>) {
                return SerializeHash (obj);
            } else if (obj is List<ShovelValue>) {
                return SerializeList (obj);
            } else if (obj is string[]) {
                return SerializeArray (obj, (string[])obj, ObjectTypes.StringArray);
            } else if (obj is ShovelValue[]) {
                return SerializeArray (obj, (ShovelValue[])obj, ObjectTypes.ShovelValueArray);
            } else if (obj is VmEnvironment) {
                var env = (VmEnvironment)obj;
                var resultArray = new object[4];
                resultArray [0] = "env";
                var result = SerializeOneHashed (resultArray, obj);
                resultArray [1] = Serialize (env.Frame);
                resultArray [2] = Serialize (env.Next);
                resultArray [3] = Serialize (env.Uses);
                return result;
            } else if (obj is VmEnvFrame) {
                var frame = (VmEnvFrame)obj;
                var resultArray = new object[4];
                resultArray [0] = "frame";
                var result = SerializeOneHashed (resultArray, obj);
                resultArray [1] = Serialize (frame.VarNames);
                resultArray [2] = Serialize (frame.Values);
                resultArray [3] = Serialize (frame.IntroducedAtProgramCounter);
                return result;
            }
            Utils.Panic ();
            throw new InvalidOperationException ();
        }

        public void WriteToStream(Stream s)
        {
//            WriteB
            foreach (var obj in this.array) {
            }
        }
    }
}

