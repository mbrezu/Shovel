using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;

namespace Shovel
{
	[StructLayout(LayoutKind.Explicit)]
	public struct ShovelValue
	{
		public enum Kinds {
			Integer,
			String,
			Double,
			Bool,
			Array,
			Hash,
			Callable
		};

		[FieldOffset(0)]
		public Kinds Kind; 

		[FieldOffset(4)]
		public long IntegerValue;

		[FieldOffset(4)]
		public string StringValue;

		[FieldOffset(4)]
		public double DoubleValue;

		[FieldOffset(4)]
		public bool BoolValue;

		[FieldOffset(4)]
		public List<ShovelValue> ArrayValue;

		[FieldOffset(4)]
		public Dictionary<string, ShovelValue> HashValue;

		[FieldOffset(4)]
		public Callable CallableValue;

		static ShovelValue value;
		public static ShovelValue Make()
		{
			return value;
		}
	}
}

