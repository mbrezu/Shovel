<!-- -*- markdown -*- -->

# Shovel Getting Started for C#

## Installation

Clone Shovel from `http://github.com/mbrezu/shovel` (or download a ZIP
file from that page).

Inside the shovel folder you'll find a `csharp/NShovel` folder which
contains a `NShovel.sln` file. I'm using
[Mono](http://mono-project.com/Main_Page) and
[MonoDevelop](http://monodevelop.com/) to write C# code. The
`NShovel.sln` file can be opened with Microsoft Visual Studio 2010 and
2012. If using Visual Studio, you'll have to unload the `ShovelTests`
projects (or delete it from the solution if using the Express versions
of the Microsoft tools) because they reference the Mono versions of
NUnit (alternatively, you can install NUnit and change the
references).

Mark the `Demos\Playground` project as startup project. Run the
application without debugging. It should say 'hello, world' and wait
for a keypress from you.

## The First Programs

Open `Demos\Playground\Main.cs` in the text editor. In the `Main`
method you should see something like:

    Console.WriteLine (
      Shovel.Api.TestRunVm(
        Shovel.Api.MakeSources("test.sho", "'hello, world'")));
    
This runs the ShovelScript program `'hello, world'` which simply
returns the string. `Shovel.Api.MakeSources` pairs the source with a
file name (you can provide multiple 'files' and names are useful in
figuring out which piece of code is mentioned in error messages). In
this case, `"test.sho"` is the file name and `"'hello, world'"` is the
source. `MakeSources` can take any number of (file name, content)
pairs.

`Shovel.Api.TestRunVm` will compile and run the sources passed as
parameters.

We'll use the `Playground` project for more experiments.

Change the contents of the `Main` method to read:

    Console.WriteLine (
      Shovel.Api.TestRunVm(
        Shovel.Api.MakeSources("test.sho", @"
    var fact = fn n if n == 0 1 else n * fact(n - 1)
    fact(10)")));
    
Running the new program should print `3628800`.

ShovelScript is a variation on the 'infix Scheme' theme. It is simpler
than JavaScript, and a description can be found in
[ShovelScriptSpec.md](../../docs/ShovelScriptSpec.md). A reader of the present
document would be greatly helped by some Scheme or JavaScript
knowledge. The factorial script above is JavaScript-like, but:

 * `function` is shortened to `fn`;
 * if the function has only one argument, the parentheses are not
   required;
 * if the function body has only one statement, curly brackets/braces
   are not required;
 * statements have values;
 * the value of a function is the value of the last statement executed
   in the function;
 * blocks (`{}`) group multiple statements together; the value of a
   block is the value of the last statement in the block;
 * **`if`** can be used as an expression, there's no ternary (`?:`)
   operator.
 
Let's see an error message. Change the `Main` method:

    try {
        Console.WriteLine (
            Shovel.Api.TestRunVm(
            Shovel.Api.MakeSources("test.sho", @"
    var fact = fn n if n == 0 1 else n * fact(n - 1)
    fact('10')")));
    } catch (Shovel.Exceptions.ShovelException shex) {
        Console.WriteLine (shex.Message);
    }

(we're passing a string to `fact` instead of a number). When running this, we'll see:

    Arguments must have the same type (numbers or strings).

    Current stack trace:
    file 'test.sho' line 2:             var fact = fn n if n == 0 1 else n * fact(n - 1)
    file 'test.sho' line 2:                                  ^^
    file 'test.sho' line 3:             fact('10')
    file 'test.sho' line 3:             ^^^^^^^^^^

    Current environment:

    Frame starts at:
    file 'test.sho' line 2:             var fact = fn n if n == 0 1 else n * fact(n - 1)
    file 'test.sho' line 2:                           ^
    Frame variables are:
    n = "10"

    Frame starts at:
    file 'test.sho' line 2:             var fact = fn n if n == 0 1 else n * fact(n - 1)
    file 'test.sho' line 2:             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Frame variables are:
    fact = [...callable...]

Oops. ShovelScript is rather unforgiving. We passed it the string '10'
instead of an integer and it crashed when comparing the string with a
number. On the bright side, the crash report is quite enlightening -
it provides the stack trace at the moment of the crash and the
variables visible at that moment (with values!).

## 'Standard Library'

There is also a 'standard library', which can be used as follows
(change the contents of `Main` again):

    try {
        Console.WriteLine (
            Shovel.Api.TestRunVm(
            Shovel.Api.MakeSourcesWithStdlib("test.sho", @"
    var arr = array(1, 2, 3, 4, 5)
    stringRepresentation(stdlib.map(arr, fn x x * x))
    ")));
    } catch (Shovel.Exceptions.ShovelException shex) {
        Console.WriteLine (shex.Message);
    }

`MakeSources` changed to `MakeSourcesWithStdlib`. `array` is a
primitive which builds an array from its arguments. `stdlib.map` takes
two arguments: an array and a function. It builds a new array by
applying the function to each element in the input array; it doesn't
change the original array. `stringRepresentation` returns a printable
representation of the data structure passed to it.

You should see `array(1, 4, 9, 16, 25)` when running the program.

The standard library is defined in `Shovel\Utils.cs`, method
`ShovelStdLib`. ShovelScript doesn't have iteration constructs, it
uses recursion instead. The 'standard library' defines a `while`
function which works like the `while` control flow statement, with one
difference: the while-condition and the while-body have to be passed
as functions. An example (again, change the contents of `Main` in the
`Playground` project):

    try {
        Console.WriteLine (
            Shovel.Api.TestRunVm(
            Shovel.Api.MakeSourcesWithStdlib("test.sho", @"
    var i = 0
    var result = array()
    stdlib.while(fn () i < 10, fn () {
      i = i + 1
      push(result, i)
    })
    stringRepresentation(result)
    ")));
    } catch (Shovel.Exceptions.ShovelException shex) {
        Console.WriteLine (shex.Message);
    }

The printed result should be `array(1, 2, 3, 4, 5, 6, 7, 8, 9,
10)`. `push` is a primitive that adds the second argument at the end
of the first argument, which must be an array.

The `while` function is a bit more cumbersome to use than a regular
`while` statement, but hopefully it's not going to be needed very much
(most iterations can be formulated as reductions, maps or for-each
operations - all of which are found in the 'standard library').

## Defining Custom Functions (a.k.a. User Defined Primitives, UDPs)

Shovel processes can be provided with user defined primitives
(UDPs). For instance, the code above could print the number on each
iteration instead of accumulating a result. We need a `print`
UDP. First, let's see the code using the UPD (change the `Main` method
again):

    try {
        var sources = Shovel.Api.MakeSourcesWithStdlib("test.sho", @"
    var i = 0
    stdlib.while(fn () i < 10, fn () {
      i = i + 1
      @print(string(i))
    })");
        var bytecode = Shovel.Api.GetBytecode(sources);
        Shovel.Api.RunVm (bytecode, sources, Udps());
    } catch (Shovel.Exceptions.ShovelException shex) {
        Console.WriteLine (shex.Message);
    }

Don't try run this code yet (it won't compile, as we need to add
method `Udps` first)!

Some explanations about the new code. 

UDPs are prefixed with a `@` sign in ShovelScript code to make them
stand out. Our UPD will only accept strings and `i` is an integer, so
we use the `string` primitive to convert it to a string.

We also use new Shovel API methods. `Shovel.Api.GetBytecode` compiles
the sources into an array of bytecode instructions. `Shovel.Api.RunVm`
runs a Shovel process for which we provided the bytecode, the sources
(optional argument, we usually want to pass them to `RunVm` to get
better error messages) and an `IEnumerable` of UPDs to make available
to the ShovelScript code.

Let's add the `Udps` method to the `MainClass` in project
`Playground`:

    static System.Collections.Generic.IEnumerable<Shovel.Callable> Udps ()
    {
        Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) => {
            if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                Console.WriteLine (args [0].StringValue);
            }
        };
        return new Shovel.Callable[] {
            Shovel.Callable.MakeUdp ("print", print, 1),
        };
    }
    
We define an UDP as a `Action<Shovel.VmApi, Shovel.Value[],
Shovel.UdpResult>` instance, which is then wrappend as a
`Shovel.Callable` by calling `Shovel.Callable.MakeUdp`.

The arguments to `Shovel.Callable.MakeUdp` are:

 * the UDP name (this is how it will be used from ShovelScript code);
 * the UDP `Action` which is used to implement the UDP and
 * the number of arguments (`null` for a variable number of
   arguments).

About the `print` we defined: it takes as arguments a `Shovel.VmApi`
instance which we'll ignore for now, an array of `Shovel.Value`
objects (the arguments passed by ShovelScript code) and a
`Shovel.UdpResult` we can use to store the return value of our UDP.

Our `print` checks that it was called correctly (with one argument
which is a string). If called correctly, it extracts the C# string
from the argument and prints it. If called incorrectly, it doesn't do
anything.

Let's run this. We should get the numbers from 1 to 10, each printed
on a fresh line.

One more example, to show how to return a value. Let's define a new
function that checks if a string starts with a given prefix:

    static System.Collections.Generic.IEnumerable<Shovel.Callable> Udps ()
    {
        Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) => {
            if (args.Length > 0 && args [0].Kind == Shovel.Value.Kinds.String) {
                Console.WriteLine (args [0].StringValue);
            }
        };
        Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> startsWith = (api, args, result) => {
            if (args.Length == 2
                && args [0].Kind == Shovel.Value.Kinds.String
                && args [1].Kind == Shovel.Value.Kinds.String) {
                result.Result = Shovel.Value.Make (args [0].StringValue.StartsWith (args [1].StringValue));
            } else {
                result.Result = Shovel.Value.Make (false);
            }
        };
        return new Shovel.Callable[] {
            Shovel.Callable.MakeUdp ("print", print, 1),
            Shovel.Callable.MakeUdp ("startsWith", startsWith, 2),
        };
    }

In `startsWith`, we check that both arguments are strings and return
`false` if the function is called incorrectly. To return a value, we
set `result.Result` which is also a `Shovel.Value`. To build a
`Shovel.Value`, we use one of `Shovel.Value.Make` (several overloads),
`Shovel.Value.MakeInt` or `Shovel.Value.MakeFloat`.

Change the ShovelScript code in `Main` to:

    var str = 'It\'s nice outside.'
    var prefix = 'It\'s'
    @print(string(@startsWith(str, prefix)))

`@print` is a bit cumbersome to use because it wants a string as a
parameter. Let's change `print` to accept more kinds of
`Shovel.Value`:

    Action<Shovel.VmApi, Shovel.Value[], Shovel.UdpResult> print = (api, args, result) => {
        switch (args[0].Kind) {
        case Shovel.Value.Kinds.String:
            Console.WriteLine (args [0].StringValue);
            break;
        case Shovel.Value.Kinds.Integer:
            Console.WriteLine (args [0].IntegerValue);
            break;
        case Shovel.Value.Kinds.Double:
            Console.WriteLine (args [0].DoubleValue);
            break;
        case Shovel.Value.Kinds.Bool:
            Console.WriteLine (args [0].BoolValue);
            break;
        default:
            Console.WriteLine ("Don't know how to print the argument.");
            break;
        }
    };

Notice that we don't check the number of arguments. Shovel does that
for us anyway, because we said (when calling `Shovel.Value.MakeUdp`)
that `print` takes one argument.

`Kind` stores the type stored in the `Shovel.Value`. We handle the
basic types (numbers, strings, bools). `Shovel.Value` can also hold:

 * arrays (defined as `System.Collections.Generic.List<Shovel.Value>`
   and accessible via the `ArrayValue` field of `Shovel.Value`),
 * hashes (defined as
   `System.Collections.Generic.Dictionary<Shovel.Value, Shovel.Value>`
   and accessible via the `HashValue` field of `Shovel.Value`) and 
 * other values that you shouldn't be handling directly (such as
   return addresses, callables etc.)
   
Note about Shovel hashes: the keys must be strings wrapped as
`Shovel.Value`s.
   
We can now drop the `string` call from the ShovelScript code:

    var str = 'It\'s nice outside.'
    var prefix = 'It\'s'
    @print(@startsWith(str, prefix))

User-defined primitives should handle their own conditions/exceptions
and return values appropriate for the exceptional situation;
ShovelScript doesn't have exception handling by default but it
provides the tools to build exception handling mechanisms that look
like the usual 'try/catch/finally' syntax.

ShovelScript is not object oriented, but this does not make it object
disoriented. Closures are objects, but closures + hashes = convenient
objects. Try this ShovelScript code by replacing the source in method
`Main`:

    var Person = fn (name, age) {
      var result = hash('name', name,
                        'age', age)
      result.getDescription = fn () 'A Person: name = ' +
        result.name + ', age = ' + string(result.age)
      result
    }
    var john = Person('John', 40)
    @print(john.getDescription())
    @print(john.age)
    @print('representation for \'john\': ' + stringRepresentation(john))

The output should be:

    A Person: name = John, age = 40
    40
    representation for 'john': hash("name", "John", "age", 40, "getDescription", [...callable...])

Notes:

 * `result` is the last statement used in `Person` to make `Person`
   return it (the value of a sequence of statements enclosed in `{`
   and `}` is the value of the last statement);
 * `stringRepresentation` tries to provide as much information about
   its argument, with the exception of callable objects (callables are
   displayed as `[...callable...]`) and circular structures
   (back-pointers are displayed as `[...loop...`]). `string` is used
   in `Person` to convert `age` to a string - it is an error to `+`
   numbers and strings.

## Where were we? ... interruptible programs

The point of writing Shovel was getting an easy way to write
interruptible programs. To show off interruptible programs, we need
some context and more functions. Context first.

Suppose we need to expose a few functions via a RPC mechanism. We need
to make calls of several of these functions at once (the sequence is
not known in advance and it may branch depending on intermediate
results) and we want to group calls into transactions (they all
execute or none executes, the information they see is isolated from
other transactions etc.). One way would be to write an extra function
for every scenario, put a BeginTransaction/Commit in every function
and also expose these larger functions via RPC. This way doesn't scale
very well (every new use case requires changing the code on the
server), and with Shovel we can do better.

On the server we'll accept Shovel programs instead of plain RPC
calls. This way, to make a 'Shovel RPC call', the client would start a
Shovel VM, provide it with user-defined primitives (UDPs) to access
arguments for the calls on the client and with an UDP to 'transfer to
the server' (say `goToServer`). The server would also accept a
serialized Shovel VM, resume it and provide it with UDPs for the
functions we wanted to expose in the first place, plus `goToClient` to
go back to the client.

Then the generic scenario would go like this:

 * the client would compile a ShovelScript program and run it until
   `goToServer` is encountered;
 * after running `goToServer` the VM state and bytecode are serialized
   and sent to the server;
 * the server starts a transaction and resumes the VM received from
   the client;
 * once the ShovelScript finishes its job on the server, it calls
   `goToClient`;
 * the server commits the transaction, serializes the VM and state and
   sends them back to the client;
 * the client resumes the VM (providing UDPs to handle the results on the
   client);
 * the VM runs to completion.

All this makes you worry about security? Good for you!

On one hand, we were going to expose the functions as RPCs, so they
would need to be security-hardened anyway. The nature of this exposure
doesn't change because we use Shovel.

On the other hand, a client could pass a defective program to the
server (by mistake or with evil intent). Since our UDPs are secured
for public access, the ShovelScript can only harm the server by
entering an infinite loop or by allocating large amounts of memory (or
both). Shovel VMs can be assigned soft CPU and RAM quotas to prevent
this (see section 'Quotas' in [ShovelVmSpec.md](../../docs/ShovelVmSpec.md)). I
will show how to use these quotas in
[WebGuessNumber.md](CsharpWebGuessNumber.md).

We'll use updating bank accounts as an example of an operation
requiring transactions (lots of people use this example, it must be
good).

The original functions we want to transform into RPCs are:

 * `subtractFromAccount(accountNo, amount)` - which takes money from
   an account and
 * `addToAccount(accountNo, amount)` - which adds money to an account.

The UDPs on the client are:

 * `getSourceAccountNo()`,
 * `getDestinationAccountNo()`,
 * `getAmount()`,
 * `goToServer()` and
 * `setTransactionDeadline(transactionDeadline)`.

The UDPs on the server are:

 * `subtractFromAccount(accountNo, amount)`,
 * `addToAccount(accountNo, amount)`,
 * `getTransactionDeadline()` and
 * `goToClient()`.

Since we need to return something to the client and the transferred
amount isn't interesting enough (the client already knows it), we
assume that our transaction on the server simply places the transfer
orders in a bank transaction queue (which is then verified and
executed - maybe by humans), and we return to the client the date of
the bank transaction deadline (by that date the bank guarantees the
money actually 'changes hands').

This example is implemented by the projects `Demos\Server` and
`Demos\Client`. You need to start the server project, leave it
running, then start the client.

The code in both projects uses some time measuring code. The problem
is that if the client only runs once, it appears to be very slow, so
you may get the impression that Shovel is much slower than it actually
is. The client gets much faster if it runs twice or more times. This is
probably due to JIT compiling times.

The ShovelScript code:

    // Setting up the call on the client.
    var sourceAccountNo = @getSourceAccountNo()
    var destinationAccountNo = @getDestinationAccountNo()
    var amount = @getAmount()

    // Go to the server.
    @goToServer()

    // Run the code on the server (the code on the server will wrap
    // the code between 'goToServer' and 'goToClient' in a transaction) .

    @subtractFromAccount(sourceAccountNo, amount)
    @addToAccount(destinationAccountNo, amount) 
    var transactionDeadline = @getTransactionDeadline()

    // Go back to the client.
    @goToClient()

    // Handle the results on the client.
    @setTransactionDeadline(transactionDeadline)

In the client code the `Main` method just calls the `Client` method
several times.

`Client` contains the ShovelScript code running the entire process
(listed above). It compiles the source to bytecode and runs it by
using `GetBytecode` and `RunVm`. The client UDPs are defined in method
`Udps`.

The most interesting UDP in `Udps` is `goToServer` which sets
`result.After` to `Shovel.UdpResult.AfterCall.Nap`. This causes the VM
execution to stop (and the `RunVm` call in `Client` returns). The
process can be now serialized and sent to the server. `Nap` means
that, on wake-up, the execution will continue with the code after the
`goToServer` call. `Continue`, the default value, means that execution
is not interrupted. `NapAndRetryOnWakeUp` means that, on wake-up, the
execution will continue by retrying the UDP call (not any parameter
evaluation, just the call).

Back to `Client`. After `RunVm` returns, `Client` calls
`SerializeBytecode` and `SerializeVmState` to turn the Shovel process
into `byte` arrays. One byte array represents the bytecode, the other
the current state of the process. If the server already has the
bytecode, you can only send the state. In our case, the server doesn't
save the bytecode so we need to send both. We also send the source
(this is not required but error messages are better if the source is
available).

The protocol between `Client` and `Server` is a simple binary protocol
built over HTTP. The Client sends a block of bytes in the request,
with the following structure:

 * 4 bytes representing the length of the bytecode,
 * 4 bytes representing the length of the source,
 * 4 bytes representing the length of the VM state,
 * the bytecode bytes,
 * the source bytes and
 * the VM state bytes.

The server replies with a binary content made of 4 bytes representing
the length of the new state and then the bytes of the new state.

This being just an example, there are no endianness checks made.

After receiving the new state from the server, the client starts the
process again (with the new state). This is necessary because we
transfer the results out of the Shovel process by calling UDPs.

On the server: in the `Main` method of the `Demos\Server\MainClass`
class, we decode the request and rebuild the bytecode (by calling
`DeserializeBytecode`). We invoke `RunVm` with the serialized
state. `RunVm` deserializes the `byte` array and checks that the state
matches the bytecode, then continues to run the process. To run things
transactionally, we should wrap the `RunVm` call in a
transaction. This example doesn't do that because the server UDPs are
dummy functions anyway. By the way, they are defined in the `Udps`
method. `goToClient` is very similar to `goToServer`.

After `RunVm` returns on the server, the state is extracted from the
VM, serialized and sent back to the client. We don't use any RAM/CPU
quotas for the process because this is a simple example. In any
production deployment you should set RAM and CPU quotas. The
[number guessing tutorial](CsarpWebGuessNumber.md) shows how to set
quotas.

