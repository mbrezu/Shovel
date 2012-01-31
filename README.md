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

Shovel programs will be written in Shovelisp and
ShovelScript. Shovelisp is a Scheme-like language. ShovelScript is a
JavaScript-like language (so they are similar, only the syntax is
different). Both Shovelisp and ShovelScript will be compiled to Shovel
VM bytecode.

## VM description

The VM is stack based. It has the following primitive types:

 * string,
 * integer (bignum),
 * character,
 * double precision floating point,
 * date,
 * list,
 * function.

Variables are stored in environments (in the SICP sense); this gives
easy closures.

When calling a function or primitive, the arguments are found in a
list that's on top of the stack. When the function returns, the result
is the value on top of the stack.

The VM uses the same bytecode in each host language - so a Shovelisp
program compiled on a Common Lisp platform will run without problems
in the JavaScript implementation of the Shovel VM.

### VM opcodes

 * `load var-name` - will fetch the value of `var-name` from the
   environment and push it on the stack;
 * `load-indirect` - similar to `load`, but will pop the `var-name`
   from the stack;
 * `store var-name` - will pop the stack and store the value into
   `var-name` into the environment;
 * `store-indirect` - similar to `store`, but will pop the `var-name`
   first, then the value to store;
 * `push value` - pushes value on the stack; the value can be of any
   Shovel type;
 * `add` - pops two values from the stack and pushes back their sum;
   the values must be either integers or floats; if any of the values
   is a float, the result is also a float, otherwise it is an integer;
 * `subtract` - similar to add, but subtracts the first popped number
   from the second popped number;
 * `multiply` - multiplication, similar to `add`, but multiplies;
 * `divide` - floating point division, divides the second popped
   number by the first popped number; same conversions as for `add`
   apply; if both numbers are integers and the result is not an
   integer, the result will be a float;
 * `div` - integer division, both arguments must be integers, the
   quotient of the division is pushed back;
 * `mod` - integer modulo, both arguments must be integers, the
   remainder of the division is the result;
 * `concatenate` - will concatenate the first popped string or list to
   the second popped string or list; both values must be strings or
   both values must be strings;
 * `cons` - will cons the second popped value to the first popped
   value; the top of the stack must be a list;
 * `add-seconds`;
 * `subtract-seconds`;
 * `pop-list n`;
 * `pop-list-indirect`;
 * `call address`;
 * `call-indirect`;
 * `call-primitive primitive-name`;
 * `call-primitive-indirect`;
 * `extend-environment`;
 * `define-variable var-name`;
 * `return`;
 * `make-lambda address` - pairs the current environment with an
   address to make a closure object which is pushed on the stack;
 * `make-lambda-indirect`;
 * `seconds-to-date`;
 * `date-to-seconds`;
 * `length`;
 * `round decimal-places`;
 * `round-indirect`;
 * `get-elt n`;
 * `get-elt-indirect`;
 * `set-elt n`;
 * `set-elt-indirect`;
 
### Exception handling notes

The ("exception-name", "catch-label", "after-label",
current-environment) tuple should be added on the control stack when
executing "try" below. When executing "end-try", then the tuple is
popped from the stack and the program counter jumps to "after-label".

When an exception is encountered, exception handling tuples are popped
from the control stack until the stack is empty or an exception
handling tuple with an exception-name that matches the thrown
exception is encountered.

    "exception-name"
    "catch-label"
    "after-label"
    try
    ...instructions...
    end-try
    :catch-label
    ...exception handler...
    :after-label

## Shovel components

 1. Common Lisp Shovel VM;
 1. JavaScript Shovel VM;
 1. C# Shovel VM;
 1. Shovelisp compiler;
 1. ShovelScript compiler;


## Shovel Status

No code written yet. Shovel VM specification not written yet.
