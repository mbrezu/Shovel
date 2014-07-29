using System;
using System.Collections.Generic;

namespace Shovel
{
    public class StructInstance
    {
        internal Struct Struct;
        internal Value[] values;

        public IEnumerable<Value> Values { get { return values; } }
    }
}

