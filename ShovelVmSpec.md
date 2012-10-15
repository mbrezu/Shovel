-*- markdown -*-

# Shovel VM Specification

## Value types

The VM is stack based. It has the following primitive scalar types:

 * string,
 * integer (modulo 2^60),
 * double precision floating point,
 * boolean and 
 * void (with only one value, `null`).

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
[SICP](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-21.html#%_sec_3.2);
this gives easy closures.

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
 * the current environment (and objects reachable from it) and
 * the 'program counter' (the next instruction to be executed).

The Shovel implementation must provide means to serialize and
deserialize the state of the VM, and to stop and start the VM.

Since serialization of stopped Shovel processes involves serialization
of state and VM bytecode and the serialized VM bytecode is often
larger than the serialized state, it makes sense to store the two in
different locations. To help with matching the state with the correct
bytecode, the state is also serialized with three additional elements:

 * the VM version (state serialized from a VM version 10 won't run on
   VMs with versions less than 10),
 * the MD5 hash of the bytecode and
 * the MD5 hash of the source from which the bytecode was compiled.
 
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
to Shovel). This will be fixed in the future by limiting the memory
available to Shovel.

### User Defined Primitives

'User defined primitives' are similar functions, but they are defined
by the user and they can do whatever the user wants (e.g. read and
write files on disk, access the database etc.). A Shovel VM process
has no way of accessing the local disk etc. unless a user defined
primitive offers that capability.


