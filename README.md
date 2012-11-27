<!-- -*- markdown -*- -->

# Shovel - a bytecode virtual machine that is embeddable, secure and interruptible

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

Have you ever needed to make RPC calls in an unspecified sequence and
wanted them to be executed transactionally (all or none), but you
didn't want to (or couldn't) use distributed transactions?  Maybe you
wanted to replace a long sequence of 'tiny' RPC calls with just one
'large' call? Shovel is great in this case.

Maybe you like continuation-based web frameworks, but would like to be
able to stop the server and restart it without losing the
continuations? Or maybe use load-balancing and run the continuation on
a different machine? Shovel can help you design such a web framework.

More generally, have you ever needed to associate a 'program' with a
'business object', but needed the 'process' generated from that
'program' to be actually running for only a small fraction of its
lifetime and to spend the rest of the time tucked away in a database
somewhere (thus freeing memory and threads for more urgent needs)?
Software for document management, bug tracking, task tracking
etc. needs such an approach - and the 'programs' mentioned may become
real programs by using Shovel (instead of being hopelessly
[complected](http://www.infoq.com/presentations/Simple-Made-Easy) with
the rest of the application).

## HOW?

Shovel is actually more then just the VM: a VM specification, a
language specification (it's more fun to write code in a high level
language than to write assembly for a VM by hand), a compiler for that
language and a 2 VM implementations (Common Lisp and C#, with
JavaScript and Java planned). The Shovel language is named
ShovelScript.

The [documentation trail for C#/.NET](shovel/blob/master/csharp/docs/index.md).

The
[documentation trail for Common Lisp](shovel/blob/master/common-lisp/docs/index.md).

The [specifications](shovel/blob/master/docs/specs.md).

Some [notes for advanced scenarios](shovel/blob/master/docs/advanced.md).

## License

Shovel implementations and the associated documentation are
distributed under the
[2-clause](https://en.wikipedia.org/wiki/BSD_licenses#2-clause_license_.28.22Simplified_BSD_License.22_or_.22FreeBSD_License.22.29)
BSD license (see the [license.txt](shovel/blob/master/license.txt)
file).

## THANKS!

The code generator and Shovel VM are modeled after the Scheme compiler
from Peter Norvig's *Paradigms of Artificial Intelligence Programming*
book (chapter 23). Thank you, Peter Norvig! (I hope I haven't broken
your code beyond recognition, any bugs in the Shovel code generator
and VM are obviously my bugs and my fault)


