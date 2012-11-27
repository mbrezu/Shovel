<!-- -*- markdown -*- -->

# Shovel - C# API Documentation

This is work in progress. Look at the code examples mentioned in the
tutorials, they cover almost everything Shovel knows how to handle.

That said, here's a general description of the API.

Classes and namespaces:

 * `Shovel.Api` - a class that contains a truckload of static methods that try to
   package everything in short and sweet calls;
 * `Shovel.Exceptions` - this namespace contains all the exceptions defined by Shovel;
 * `Shovel.SourceFile` - represents a named program piece (a file abstraction,
   with name and content) - most of the time you don't care about it,
   `Shovel.Api.MakeSource*` builds a list of these;
 * `Shovel.UdpResult` - a class representing the result of an user defined
   primitive (UDP);
 * `Shovel.Value` - the 'union' holding a Shovel VM value;
 * `Shovel.VmApi` - useful if you need to report approximations of
   RAM/CPU usage from an UDP;
 * `Shovel.Callable` - you only need to care about Shovel.Callable.MakeUdp.
 
## `Shovel.Api`

### `Version`

The current version of Shovel. Versions are supposed to be backwards
compatible. They are stored in both serialized bytecode and serialized
state. Trying to restore a version `n` process on a computer running a
Shovel version less than `n` is not possible.

### `SerializeVmState`, `SerializeVmStateToStream`

The argument is a Shovel VM. The result is the serialized state,
either as a `byte[]` or a `MemoryStream`.

### `VmProgrammingError`, `VmUserDefinedPrimitiveError`

The argument is a Shovel VM. They return the programming error or the
UDP error associated with the VM, if any. The programming error is
what results from an illegal operation (such as comparing numbers with
strings). The UDP error is an error that was thrown out of an UDP.

A VM with either one of these errors cannot be serialized and cannot
be resumed.

### `PrintRawBytecode`

Compiles a list of `Shovel.SourceFile` (such as returned by the
`Shovel.Api.MakeSource*` functions) and returns a listing of the
resulting bytecode, before assembly.

### `PrintAssembledBytecode`

Similar to `PrintRawBytecode`, but assembles code before printing
it. Can take bytecode or source as arguments.

### `GetBytecode`

Transforms sources into assembled bytecode (runs the compiler and the
assembler). The result is ready to be fed to `RunVm` or `TestRunVm`.

### `SerializeBytecode` and `SerializeBytecodeToStream`

Will serialize assembled bytecode. Result as `byte[]` or
`MemoryStream`.

### `DeserializeBytecode`

The opposite of `SerializeBytecode` and `SerializeBytecodeToStream`.

### `MakeSources`

Takes a list of <name, content> tuples and assembles them into a list
of `Shovel.SourceFile` objects. Odd arguments are names, even
arguments are corresponding content.

### `MakeSourcesWithStdlib`

Like `MakeSources`, but prepends the 'standard library' as 'stdlib.sho'.

### `TestRunVm`

Useful for testing. Compiles and runs a bunch of sources, returns the
value on the top of the stack at program completion.

### `RunVm`

Runs a VM, built from bytecode, sources, state and UDPs. Restricts the
process with soft quotas for RAM and CPU. The sources, state and UPDs
are optional. The bytecode can be replaced by an existing VM object.

Returns the VM such as it is when the process completed or was
interrupted.

### `CheckStartTop`

Verifies that a VM has only one item on its stack and returns the item.

### `WakeUpVm`

Wakes up a VM which doesn't have any error (it is napping because it
exceeded the ticks-until-next-nap quota or an UDP asked for a nap).

### `VmIsLive`

Returns true if the VM has not completed and is not napping.

### `VmExecutionComplete`

Returns true if the Shovel process inside the VM completed.

### `VmUsedCells`

Returns the number of cells currently used by the VM.

### `VmExecutedTicks`

Returns the total number of ticks executed by the VM.

### `VmExecutedTicksSinceLastNap`

Returns the total number of ticks executed since the previous nap (or
since the beginning, if there is no previous nap).

## `Shovel.Exceptions`

### `BrokenDataExceptions`

An attempt has been made to deserialize data which doesn't match its
own MD5 hash.

### `BytecodeDoesntMatchState`

An attempt has been made to resume a VM with bytecode and state which
do not belong to the same process.

### `EndianessMismatchException`

The endianness on the serializing side doesn't match the endianness on
the deserializing side.

### `ShovelCellQuotaExceededException`

The cell quota has been exceeded by a Shovel process.

### `ShovelException`

A programming error (e.g. comparing numbers and strings)
occured. There are fields describing the error and its location in
source code.

### `ShovelTicksQuotaExceededException`

The total ticks quota has been exceeded by a Shovel process.

### `VersionNotSupportedException`

An attempt has been made to restart a `n+m` version process on a `n`
version VM (with both `n` and `m` positive). This is not supported.

## `Shovel.UdpResult`

Two fields: the `Result` which is a `Shovel.Value` (see below) and
`After`, which encodes what to with the Shovel process when the UDP
returns. See the [getting started guide](CsharpGettingStarted.md).

## `Shovel.Value`

An 'union' type holding the possible values of a Shovel
variable/memory location. See the
[getting started guide](CsharpGettingStarted.md).

## `Shovel.VmApi`

You can use the `CellIncrementer` and `TicksIncrementer` from UDPs to
model your CPU and RAM usage. See the
[VM spec](../../docs/ShovelVmSpec.md), section 'Quotas'.

## `Shovel.Callable`

Use `Shovel.Callable.MakeUdp` to create UDPs. See the
[getting started guide](CsharpGettingStarted.md).
