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
using System.Text;
using Shovel.Exceptions;

namespace ShovelTests
{
    [TestFixture]
    public class StructTests
    {
        [Test]
        public void CreateStruct ()
        {
            var program = "defstruct(array('x', 'y'))";
            var sources = Shovel.Api.MakeSources ("test.sho", program);
            var value = Shovel.Api.TestRunVm (sources);
            Assert.AreEqual (Shovel.Value.Kinds.Struct, value.Kind);

        }

        [Test]
        public void InstantiateStruct ()
        {
            var program = @"
var point = defstruct(array('x', 'y'))
make(point)
";
            var sources = Shovel.Api.MakeSources ("test.sho", program);
            var value = Shovel.Api.TestRunVm (sources);
            Assert.AreEqual (Shovel.Value.Kinds.StructInstance, value.Kind);
        }

        [Test]
        public void InstantiateStructComplex ()
        {
            var program = @"
var point = defstruct(array('x', 'y'))
make(point, 1, 2)
";
            var sources = Shovel.Api.MakeSources ("test.sho", program);
            var value = Shovel.Api.TestRunVm (sources);
            Assert.AreEqual (Shovel.Value.Kinds.StructInstance, value.Kind);
        }

        [Test]
        public void StructGetters ()
        {
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var p1 = make(point, 1, 2)
p1.x
", Shovel.Value.Kinds.Integer, (long)1);
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var p1 = make(point, 1, 2)
p1.y
", Shovel.Value.Kinds.Integer, (long)2);
            Utils.TestValue (@"
var point = defstruct(array('x', 'y', 'z'))
var p1 = make(point, 1, 2)
p1.z
", Shovel.Value.Kinds.Null, null);
        }

        [Test]
        public void StructSetters ()
        {
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var p1 = make(point, 1, 2)
p1.x = p1.x + 3
p1.x
", Shovel.Value.Kinds.Integer, (long)4);
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var p1 = make(point, 1, 2)
p1.x = p1.x + 3
p1.y
", Shovel.Value.Kinds.Integer, (long)2);
        }

        [Test]
        public void StructsToHashes ()
        {
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var h = structToHash(make(point, 1, 2))
h.x
", Shovel.Value.Kinds.Integer, (long)1);
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var h = structToHash(make(point, 1, 2))
h.y
", Shovel.Value.Kinds.Integer, (long)2);
        }

        [Test]
        public void HashesToStructs ()
        {
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var p1 = hashToStruct(point, hash('x', 10, 'y', 20, 'z', 30))
p1.x
", Shovel.Value.Kinds.Integer, (long)10);
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var p1 = hashToStruct(point, hash('x', 10, 'y', 20, 'z', 30))
p1.y
", Shovel.Value.Kinds.Integer, (long)20);
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var p1 = hashToStruct(point, hash('x', 10, 'z', 30))
p1.y
", Shovel.Value.Kinds.Null, null);
        }

        [Test]
        public void PrintStructs ()
        {
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
string(point)
", Shovel.Value.Kinds.String, "[...struct...]");
            Utils.TestValue(@"
var point = defstruct(array('x', 'y'))
stringRepresentation(point)
", Shovel.Value.Kinds.String, "defstruct(array('x', 'y'))");
        }

        [Test]
        public void PrintStructInstances()
        {
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
string(make(point))
", Shovel.Value.Kinds.String, "[...struct instance...]");
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
stringRepresentation(make(point, 1, 2))
", Shovel.Value.Kinds.String, "make(defstruct(array('x', 'y')), 1, 2)");
            Utils.TestValue (@"
var point = defstruct(array('x', 'y', 'loop'))
var test = make(point, 1, 2)
test.loop = test
stringRepresentation(test)
", Shovel.Value.Kinds.String, "make(defstruct(array('x', 'y', 'loop')), 1, 2, [...loop...])");
        }

        [Test]
        public void IsStruct()
        {
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
isStruct(point)
", Shovel.Value.Kinds.Bool, true);
            Utils.TestValue ("isStruct(10)", Shovel.Value.Kinds.Bool, false);
        }

        [Test]
        public void IsStructInstance()
        {
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
isStructInstance(make(point, 1, 2), point)
", Shovel.Value.Kinds.Bool, true);
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
isStructInstance(10, point)
", Shovel.Value.Kinds.Bool, false);
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var rectangle = defstruct(array('left', 'top', 'right', 'bottom'))
isStructInstance(make(point, 10, 20), rectangle)
", Shovel.Value.Kinds.Bool, false);
            Utils.TestValue (@"
var point = defstruct(array('x', 'y'))
var rectangle = defstruct(array('left', 'top', 'right', 'bottom'))
isStructInstance(make(point, 10, 20), point) && isStructInstance(make(rectangle), rectangle)
", Shovel.Value.Kinds.Bool, true);
        }

    }
}

