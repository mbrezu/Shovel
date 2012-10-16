-*- markdown -*-

# Shovel

## WHAT?

Shovel is a bytecode virtual machine (VM) that allows programs written
for it to be:

 * embeddable - Shovel is implemented as a library, it can be loaded
   and used by your program;
 * secure/sandboxed - Shovel processes can only perform the operations
   you explicitly allow and the CPU/RAM usage of the process can be
   bound by soft limits;
 * and interruptible - a Shovel process can be stopped, saved to a
   database and resumed later, possibly on another machine.
   
## WHY?

Have you ever needed to make RPC calls, but didn't know in advance the
sequence of RPC calls? Maybe you wanted those RPC calls to be executed
transactionally (all or none), but you didn't want to (or couldn't)
use distributed transactions? Maybe you wanted to replace a long
sequence of 'tiny' RPC calls with just one 'large' call? Shovel is
great in this case.

Maybe you like continuation-based web frameworks, but would like to be
able to stop the server and restart it without losing the
continuations? Or maybe use load-balancing and run the continuation on
a different machine? Shovel can help you design such a web framework.

More generally, have you ever needed to associate a 'program' with a
'business object', but needed the 'process' generated from that
'program' to be actually running for only a small fraction of its
lifetime and to spend the rest of the time tucked away in a database
somewhere (thus freeing memory and threads for more urgent needs)?
Document management, bug tracking, task tracking etc. software may use
such an approach - and the 'programs' mentioned may become real
programs by using Shovel.

## HOW?

Shovel is actually more then just the VM: a VM specification, a
language specification (it's more fun to write code in a high level
language than to write assembly for a VM by hand), a compiler for that
language and a VM implementation (both written in Common Lisp now,
with implementations in C#, JavaScript and Java planned). The Shovel
language is named ShovelScript.

A Shovel quick start guide for Common Lisp:
[ClGettingStarted.md](ClGettingStarted.md).

A simple number guessing demo: [WebGuessNumber.md](WebGuessNumber.md).

If you want more information: The language specification:
[ShovelScriptSpec.md](ShovelScriptSpec.md).

If you want even more information: The VM specification:
[ShovelVmSpec.md](ShovelVmSpec.md).

Beyond these, the source code is the real source of information :-)

## THANKS!

The code generator and Shovel VM are modeled after the Scheme compiler
from Peter Norvig's *Paradigms in Artificial Intelligence Programming*
book (chapter 23). Thank you, Peter Norvig! (I hope I haven't broke
your code beyond recognition, any bugs in the Shovel code generator
and VM are obviously my bugs and my fault)

