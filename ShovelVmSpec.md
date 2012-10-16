-*- markdown -*-

# Shovel VM Specification

## Value types

The VM is stack based. It has the following primitive scalar types:

 * string,
 * integer (modulo 2^60),
 * double precision floating point,
 * boolean and 
 * void (with only one value, `null`).
 
NOTE: the integers are limited to 60 bits because bounding the
CPU/memory usage is more difficult in the presence of unbounded
bignums.

The other types allowed are:

 * primitive function (defined as a name and a corresponding function
   in the host environment),
 * function (defined as an environment and a Shovel VM address),
 * arrays (elements can be any primitive type, including arrays) and
 * hashes (associative arrays, keys can be strings, values can have
   any Shovel VM type).

## The stack

The stack can hold the following kinds of elements:

 * a return address, made of an environment and a Shovel VM address
   (the 'program counter' to return to),
 * a value of one of the Shovel VM types or 
 * a 'named block record', consisting of the name of the block, the
   address (program counter) where the block ends, and the environment
   which was current when the block started executing.

## Environments

Variables are stored in environments (in the
[SICP](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-21.html#%_sec_3.2)
sense) this gives easy closures.

An environment is a stack of 'frames' (the environment stack). Frames
are arrays of (name, value) pairs. The names are only needed to offer
more helpful debugging information.

A variable is identified as a pair of integers (frame address,
variable index in frame). See [PAIP](http://www.norvig.com/paip.html),
Chapter 23. The 'frame address' is the distance of the frame from the
top of environment stack (i.e., frame address 0 means the frame on the
top of the environment stack, frame address 1 means the one below it
and so on). Once the frame is identified, the 'variable index' is the
index of the variable in the frame.

## State

The VM state is defined by:

 * the stack (and objects reachable from it),
 * the current environment (and objects reachable from it),
 * the 'program counter' (the next instruction to be executed),
 * the total-ticks counter (see section 'Quotas' below) and
 * the used cells counter (see section 'Quotas' below).

The Shovel implementation must provide means to serialize and
deserialize the state of the VM, and to stop and start the VM.

Since serialization of stopped Shovel processes involves serialization
of state and VM bytecode and the serialized VM bytecode is often
larger than the serialized state, it makes sense to store the two in
different locations. To help with matching the state with the correct
bytecode, the state is also serialized with three additional elements:

 * the VM version (state serialized from a VM version 10 won't run on
   VMs with versions less than 10),
 * the MD5 hash of the bytecode (the bytecode MD5 from the serialized
   state must match the bytecode MD5 of the VM) and
 * the MD5 hash of the source from which the bytecode was compiled
   (this is informational, no checks are made).
 
This way it is immediately obvious if an attempt is made to restart a
VM with the state of another VM.

### Serialization

The serialization stores circular data-structures correctly by keeping
a record of the pointers already 'visited'.

Right now the serialization details are in the Common Lisp
code. [MessagePack](http://msgpack.org/) is used to encode the data.

A description of the serialization algorithm and storage is [TBD]
(will be done probably after there are more than 1 implementations of
Shovel) - see the `vm.lisp` file, functions `serialize-vm-state` and
`deserialize-vm-state` for now.

## Sleeping

A VM loaded in memory can be active or sleeping. A sleeping VM can be
woken up (thus becoming active) or it can be serialized and stored.

A VM goes to sleep if it exceeds a ticks quota (see section 'Quotas'
below) or if a user-defined primitive (see section 'Primitives' below)
asks it to go to sleep.

## Opcodes

The VM uses the same bytecode in each host language - so a ShovelScript
program compiled on a Common Lisp platform will run without problems
in the JavaScript implementation of the Shovel VM.

The VM program is a list of opcodes. Since the VM is stack based, each
instruction has a 'stack effect' - it transforms the stack by popping
and pushing elements. The stack effect is described using
[stack effect diagrams](https://en.wikipedia.org/wiki/Stack-oriented_programming_language#Stack_effect_diagrams).

The next subsections describe the available opcodes.

### `CONST`

Arguments: `value`;

Stack effect: ( -- `value`);

Description: push a value on the stack; the value must belong to one
of the scalar types described above.

### `LGET`

Arguments: `frame`, `var`;

Stack effect: ( -- `variable-value`);

Description: get the value from frame `frame` and variable index `var`
(see section 'Environments' above) and push the value on the stack. An
error is thrown if no such variable is found.

### `LSET`

Arguments: `frame`, `var`;

Stack effect: ( `variable-value` -- `variable-value`);

Description: store the value on the top of the stack to the variable
identified by `frame` and `var` (see LGET above). An error is thrown
if no such variable is found.

### `POP`

Arguments: none;

Stack effect: ( `top` -- );

Description: throw away the top of the stack.

### `TJUMP`

Arguments: `address`;

Stack effect: ( `boolean-value` -- );

Description: jump to the address `address` if the top of the stack is
the boolean value `true`; pop the stack anyway. An error is thrown if
the top of the stack (`boolean-value`) is not a boolean.

### `FJUMP`

Arguments: `address`;

Stack effect: ( `boolean-value` -- );

Description: jump to the address `address` if the top of the stack is
the boolean value `false`; pop the stack anyway. An error is thrown if
the top of the stack (`boolean-value`) is not a boolean.

### `JUMP`

Arguments: `address`;

Stack effect: ( -- );

Description: unconditionally jump to address `address`.

### `RETURN`

Arguments: none;

Stack effect: ( `return-address` `return-value` -- `return-value` );

Description: Set the current environment and program counter to the
ones stored in `return-address`. See the section 'The Stack' above.

### `CALLJ`

Arguments: `argument-count`;

Stack effect: ( `callable` -- );

Description: call the callable object found on the top of the stack,
with the next `argument-count` elements from the stack as
arguments. If the callable is a primitive, then `argument-count`
elements are popped from the stack and used to call the primitive.

### `CALL`

Arguments: `argument-count`;

Stack effect:
( [ ... `arguments` ... ] `callable` --  `return-address` [ ... `arguments` ... ]);

Description: save a return address on the stack 'under' the arguments;
the return address is the pair (current environment, program counter
pointing to the instruction after the call); THEN do what CALLJ does.

### `ARGS`

Arguments: `argument-count`;

Stack effect: ( [ ... `arguments` ... ] -- );

Description: the instruction pops `argument-count` values from the
stack, and stores them in the topmost frame of the current environment
in the first slots (e.g. for `argument-count = 2` the top of the
stack will be stored at position 1 in the frame and the next-top-of-stack at
position 0).

### `FN`

Arguments: `address`, `number-of-arguments`;

Stack effect: ( -- `new-closure`);

Description: builds a closure (as a pair of the current environment
and `address`) and stores it on the stack; the closure can only be
called with `number-of-arguments` arguments.

### `PRIM`

Arguments: `primitive-name` (a string);

Stack effect: ( -- `callable primitive`);

Description: pushes a user-defined primitive on the stack. The
primitive can be called with CALLJ or stored in a data structure. See
the 'Required Primitives' section below for an explanation of the
differences between a required primitive and a user-defined primitive.

### `PRIM0`

Arguments: `primitive-name` (a string);

Stack effect: ( -- `callable primitive`);

Description: like `PRIM`, but for required primitives. See `Required
Primitives` below.

### `NEW-FRAME`

Arguments: `var-1`, `var-2`, ... `var-count`;

Stack effect: ( -- );

Description: extends the current environment by adding a new frame
containing `count` slots (the number of arguments). All the new
variables are initialized to `null`.

### `DROP-FRAME`

Arguments: none;

Stack effect: ( -- );

Description: removes a frame from the current environment (pops the
environment stack).

### `BLOCK`

Arguments: `address`;

Stack effect: ( `block-name` -- `named-block-record` );

Description: starts a named block (the name is `block-name`) that ends
at `address`; creates a 'named block' record (which contains
`block-name`, `address` and the current environment) in
`named-block-record`.  Non-local exits are implemented using `BLOCK`,
`BLOCK_RETURN` and `POP_BLOCK`. The instruction at `address` is
supposed to be `POP_BLOCK`.

### `BLOCK_RETURN`

Arguments: none;

Stack effect: 
( `named-block-record` .... `block-name` `return-value` -- `named-block-record` `return-value`);

Description: returns from block named `block-name` (a string). If
there is no named block record on the stack for `block-name`, an error
is thrown. `named-block-record` is the record for `block-name`
Execution is transferred to the end address from `named-block-record`,
and the environment is set to the environment of
`named-block-record`. All values on the stack between
`named-block-record` and `return-value` are thrown away.

### `POP_BLOCK`

Arguments: none;

Stack effect: ( `named-block-record` `return-value` -- `return-value` )

Description: ends a named block. An error is thrown if
`named-block-record` is not a named block record. The ShovelScript compiler
guarantees that `RETURN` isn't issued in places where it would cause
the VM to miss a `POP_BLOCK` instruction.

### `CONTEXT`

Arguments: none

Stack effect: ( -- `context-hash` )

Description: creates and pushes on the stack a hash table with two
entries:

 * 'stack' - the corresponding value is a string representation of the
   current stack trace and
 * 'environment' - the corresponding value is a string representation
   of the current environment.

## Errors

There is no exception handling mechanism in Shovel. The VM allows
non-local returns via `BLOCK`, `BLOCK_RETURN` and `POP_BLOCK`, which
can be used to implement exception handling as functions in ShovelScript.

The VM throws an exception in the host language in case of a
programming error (e.g. out of bounds array access). A VM that threw
an error stops and cannot be resumed and its state cannot be
serialized.

## Primitives

Primitives are functions written in the language hosting Shovel. They
receive arguments which are Shovel VM values and return Shovel VM
values.

There are 'required primitives' and 'user defined primitives'.

### Required Primitives

'Required primitives' are present in all Shovel VM implementations and
provide basic functionality for ShovelScript programs (e.g. arithmetic
operators, string functions etc.). They are considered 'safe' (they
cannot access state other than what is passed to them as arguments)
and they can only harm the environment by 'denial of service'
(e.g. allocating a huge array which occupies all the memory available
to Shovel or running infinite loops). This problem can be solved by
setting CPU and RAM usage quotas for a VM.

### User Defined Primitives

'User defined primitives' (UDPs) are similar functions, but they are
defined by the user and they can do whatever the user wants (e.g. read
and write files on disk, access a database etc.). A Shovel VM
process has no way of accessing the local disk etc. unless a user
defined primitive offers that capability.

UDPs are responsible for not running too long or allocating too much
memory (because they are explicitly provided to the VM by the
programmer setting up the VM environment, unlike the required
primitives which are always available). UDPs can model their CPU and
RAM usage by telling Shovel how much of these resources they are
using.

UDPs can also ask the VM to 'go to sleep'. They can do this by
returning two values instead of just one. The second value will
specify what to do next:

 * CONTINUE or no second value means that the VM is still active and
   still runs;
 * NAP-AND-RETRY-ON-WAKEUP means that the VM is rewinded to the state
   before the call to this UDP and then goes to sleep; when the VM is
   woken up again, it will retry calling the UDP that told it to go to
   sleep;
 * NAP means that the UDP returns normally, but instead of executing
   the next instruction, the VM goes to sleep; when woken up, the VM
   will continue from the instruction after the UDP call.

## Quotas

Shovel VMs can limit the amount of RAM and CPU available to the
programs running inside them. This is done by providing soft quotas
for 'ticks' (a tick is the CPU time required to execute a Shovel VM
instruction) and 'cells' (a cell is a basic unit of memory allocation,
typically a few bytes - e.g. a CONS cell in Common Lisp). The quotas
are 'soft' because the process is not guaranteed to stop immediatelly
after exceeding the quota and because the count of ticks and cells is
approximative (the definition for ticks and cells is also 'soft' -
i.e. if you want to build a real time system on top of Shovel VM tick
quotas or if you want a Shovel VM not to use more than 5 MB of memory
no matter what you mistook Shovel quotas for something they are not).

The point of Shovel quotas is to provide some basic isolation between
VMs, so that a runaway Shovel process won't bring down or take over
the entire OS process.

### RAM Quota

You can specify the total number of cells that are available to a
VM. The VM keeps a counter of used cells. It increments that counter
whenever it executes an operation that allocates memory.

UDPs can model their memory allocation by incrementing this counter by
a value of their choice (hopefully representative of the amount of
memory they allocated).

Before executing each instruction, the VM checks if this cell counter
went over the cell quota. If it did, it walks the active objects in
the VM (i.e. the objects reachable from the stack and the current
environment) and adds up their cell usage. The cell counter is then
set to this (more accurate) cell usage. If the cell counter is still
over the cell quota, the VM throws a cell-quota-exceeded exception.

The ever-incrementing cell counter is a means of avoiding to count all
the live objects before every instruction.

The cells quota is soft because:
 
 * the size of the cell is implementation dependent;
 * the cell count is approximative;
 * the UDPs are trusted to declare their approximative memory usage;
 * a more accurate cell usage is calculated only now and then (but it
   is still an approximation).
 
Nevertheless, setting a cell usage quota will prevent a misbehaving VM
process from using all the available memory.

### CPU Quota

There are two CPU-related quotas: a total ticks quota (how many Shovel
VM instructions is a particular VM allowed to execute) and a
ticks-until-next-nap quota (how many Shovel VM instructions will a
particular VM execute before it is interrupted and asked to go to
sleep).

There are two corresponding tick counters which are incremented by one
after executing any Shovel VM instruction. The user-defined primitives
can model their CPU cost by incrementing these counters by values
representative of their CPU usage.

Before each instruction the tick quotas are checked. If the total
ticks quota is exceeded, the VM throws a total-ticks-quota-exceeded
exception. If the ticks-until-next-nap quota is exceeded, the VM goes
to sleep.

These CPU quotas are soft because:

 * the CPU cost of a 'tick' is approximative and unequal (not all
   ticks have the same cost);
 * the UDPs are trusted to declare their approximative CPU usage.

Despite these limitations, CPU quotas can be used to stop VM processes
which enter infinite loops.

