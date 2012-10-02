-*- markdown -*-

# Shovel

## Introduction

Shovel is a bytecode virtual machine (VM) that allows programs to be:

 * sandboxed (Shovel programs are limited by the primitives loaded
   into the VM),
 * passivizable (Shovel processes can be stopped, serialized, stored
   and reactivated later),
 * and therefore transportable between hosts.

Why 'Shovel'? Because that's what you use to do heavy lifting in a
sandbox.

The Shovel VM doesn't try to be fast. If some operation needs to be
fast, you can implement it as a primitive in the host language.

A host language is a programming language for which there is a Shovel
VM implementation. The first host language will be Common Lisp. The
second JavaScript. The third... probably C#.

The point of having multiple implementations is to be able to write
Shovel programs that execute on a machine up to a certain point, then
are 'passivated' and moved to another machine, then reactivated and
restarted, possibly inside a program written in another host language.

For instance, when the Shovel VM implementation for JavaScript and
Common Lisp are ready, it will be possible to create a Shovel process
inside a browser (using JavaScript as a host language), send the
process to a server (using Common Lisp), let the program gather and
calculate some data, then return the program to the browser and
display the results. This should eliminate most scenarios when it's
necessary to make multiple calls to the server (and the actual
sequence of calls is not known in advance).

Shovel programs will be written in Shript (which is a JavaScript-like
language). Shovel implementation will include a Shript compiler.

The Shript compiler and the Shovel VM are inspired by chapter 23 of
'Paradigms of Artificial Intelligence Programming' by Peter Norvig.

## VM description

### Value types

The VM is stack based. It has the following primitive scalar types:

 * string,
 * integer (bignum),
 * double precision floating point and
 * boolean.
 
The other types allowed are:

 * primitive function (defined as a name and a corresponding function
   in the host environment),
 * function (defined as an environment and a Shovel VM address),
 * arrays (elements can be any primitive type, including arrays) and
 * hashes (associative arrays, keys can be strings, values can have
   any Shovel VM type).
   
### The stack
   
The stack can have two types of elements:

 * a return address, made of an environment and a Shovel VM address
   (the 'program counter' to return to) or
 * a value of one of the Shovel VM primitive types.
 
### Environments
 
Variables are stored in environments (in the SICP sense); this gives
easy closures.

An environment is a stack of 'frames'. Frames are arrays of values. A
variable is identified as a pair of (frame address, variable index in
frame). The 'frame address' is the distance of the frame from the top
of environment stack (i.e., frame address 0 means the frame on the top
of the environment stack, frame address 1 means the one below it and
so on). Once the frame is identified, the 'variable index' is the
index of the variable in the frame.

### State

The VM state is defined by:

 * the stack (and objects reachable from it),
 * the current environment (and objects reachable from it) and
 * the 'program counter' (the next instruction to be executed).
 
The Shovel implementation must provide means to serialize and
deserialize the state of the VM, and to stop and start the VM. More on
this when describing the implementation of the VM runtime.

### Opcodes

The VM uses the same bytecode in each host language - so a Shript
program compiled on a Common Lisp platform will run without problems
in the JavaScript implementation of the Shovel VM.

The VM program is a list of opcodes. 

The possible opcodes are:

 * CONST *value* - push a value on the stack; the value must be a
   scalar value (see section 'Value types' above);
 * LGET *frame*, *var* - get the value from frame *frame* and variable
   index *var* (see section 'Environments' above) and push the value
   on the stack;
 * LSET *frame*, *var* - store the value on the top of the stack to
   the variable identified by *frame* and *var* (see LGET above);
 * POP - throw away the top of the stack;
 * TJUMP *address* - jump to the address *address* if the top of the
   stack IS NOT the boolean value 'false'; pop the stack;
 * FJUMP *address* - jump to the address *address* if the top of the
   stack IS the boolean value 'false'; pop the stack;
 * JUMP *address* - unconditionally jump to address *address*;
 * SAVE *address* - save a return address on the stack; the return
   address is the pair (current environment, *address*);
 * RETURN - the top of the stack is the value to be returned by the
   current function; the 'next top' is a return address; pop the 'next
   top' and set the current environment and program counter to the
   ones from the return address;
 * CALLJ *argument-count* - call the function object found on the top
   of the stack, with the next *argument-count* elements from the
   stack as arguments;
 * ARGS *argument-count* - the code for every function must start with
   an ARGS instruction; the instruction pops *argument-count* from the
   stack, and extends the current environment with a frame containing
   the extracted values; the top of the stack before the execution of
   ARGS is the last value in the new frame (it has the highest
   'variable index');
 * FN *address* - builds a closure (as a pair of the current
   environment and *address*) and stores it on the stack.
   
### Required Primitives

Relational operators, arithmetic operators, array and hash access,
string access, string deconstruction and construction.


