-*- markdown -*-

# Shovel Getting Started for Common Lisp

## Installation

Clone Shovel from `http://github.com/mbrezu/shovel` into the
[`local-projects`](http://blog.quicklisp.org/2011/11/november-quicklisp-updates.html)
directory of your [Quicklisp](http://www.quicklisp.org/) installation.

At your REPL (preferably a SLIME REPL) you can now type `(ql:quickload
:shovel)` to load Shovel into your Lisp image. Run
`(shovel-tests:run-tests)` to check that things are in reasonably good
shape.

NOTE: I tested Shovel with SBCL 1.1.0 and CCL 1.8-r15286M (32bit). It
should work on other CL implementations, but these are the only
implementations actually tested.

## The first programs

Time for the your first ShovelScript program (ShovelScript is the
high-level language used to write Shovel programs):

    (shovel:run-code (list "'hello, world'"))

This should return the string 'hello, world'. Easy, right? Another
classic:

    (shovel:run-code (list "
    var fact = fn n if n == 0 1 else n * fact(n - 1)
    fact(10)"))

ShovelScript is a variation on the 'infix Scheme' theme. It is simpler
than JavaScript, and a description can be found in
[ShovelScriptSpec.md](ShovelScriptSpec.md). A reader of the present
document would be greatly served by some Scheme or JavaScript
knowledge. The factorial script above is JavaScript-like, but:

 * `function` is shortened to `fn`;
 * if the function has only one argument, the parentheses are not
   required;
 * if the function body has only one statement, curly brackets/braces
   are not required;
 * statements have values;

`shovel:run-code` takes a list of sources. A 'source' can simply be a
string, or an instance of `shovel:source-file`. The example above can
be rewritten as:

    (shovel:run-code (list (shovel:make-source-file
                            :name "test.sho"
                            :contents "
    var fact = fn n if n == 0 1 else n * fact(n - 1)
    fact(10)")))

'String' sources are converted to `shovel:source-file` by assigning
them a name. Explicitly providing a name can help with debugging:

    (shovel:run-code (list (shovel:make-source-file
                            :name "test.sho"
                            :contents "
    var fact = fn n if n == 0 1 else n * fact(n - 1)
    fact('10')")))
    Shovel error in file 'test.sho' at line 2, column 20:
    At least one of the arguments must be null or they must have
    the same type (numbers, strings, booleans, hashes or arrays).

    Current stack trace:
    file 'test.sho' line 2: var fact = fn n if n == 0 1 else n * fact(n - 1)
    file 'test.sho' line 2:                    ^^^^^^
    file 'test.sho' line 3: fact('10')
    file 'test.sho' line 3: ^^^^^^^^^^

    Current environment:

    Frame starts at:
    file 'test.sho' line 2: var fact = fn n if n == 0 1 else n * fact(n - 1)
    file 'test.sho' line 2:               ^
    Frame variables are:
    n = "10"

    Frame starts at:
    file 'test.sho' line 2: var fact = fn n if n == 0 1 else n * fact(n - 1)
    file 'test.sho' line 2: ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Frame variables are:
    fact = [...callable...]

Oops. ShovelScript is rather unforgiving. We passed it the string '10'
instead of an integer and it crashed when comparing the string with a
number. On the bright side, the crash report is quite enlightening -
it provides the stack trace at the moment of the crash and the
variables visible at that moment (with values!).

There is also a 'standard library', which can be used as follows:

    (shovel:run-code (list (shovel:stdlib)
                           (shovel:make-source-file
                            :name "test.sho"
                            :contents "
    var arr = array(1, 2, 3, 4, 5)
    stdlib.map(arr, fn x x * x)
    ")))
    #(1 4 9 16 25)

`array` builds an array from its arguments. `stdlib.map` applies a
given function to all the elements of an array and returns an array of
results.

There are other useful functions in this library - see the source of
`shovel.lisp`, function `stdlib`.

We can provide a Shovel VM with user-defined primitives. For instance,
suppose we want to print values:

    (shovel:run-code (list (shovel:stdlib)
                           (shovel:make-source-file
                            :name "test.sho"
                            :contents "
    var arr = array(1, 2, 3, 4, 5)
    stdlib.forEach(arr, fn x @print (x))
    "))
                     :user-primitives (list (list "print" #'print 1)))

    1
    2
    3
    4
    5 NULL

(the final `NULL` is the result from `forEach`, a user-defined
primitive is defined by providing a name, a CL function object and the
number of arguments - NIL if we want to use functions which accept a
variable number of arguments).

User-defined primitives should handle their own conditions/exceptions
and return values appropriate for the exceptional situation
- ShovelScript doesn't have exception handling.

A simple example with two user-defined primitives:

    (shovel:run-code (list (shovel:stdlib)
                           (shovel:make-source-file
                            :name "test.sho"
                            :contents "
    @print('Your name, please: ')
    var name = @readLine()
    @printLn('Hello, ' + name)
    "))
                     :user-primitives (list (list "print" #'princ 1)
                                            (list "printLn" (lambda (x)
                                                              (princ x)
                                                              (terpri)
                                                              :null) 1)
                                            (list "readLine" #'read-line 0)))
    Your name, please: John
    Hello, John
    NULL

The definition of `printLn` returns `:null` - this is the
representation of ShovelScript's `null` in Common Lisp. User-defined
primitives are supposed to return ShovelScript values and it is an
error if they don't. See the ShovelScript specification for the list
of types. `:true` and `:false` are the ShovelScript booleans
represented in CL, CL arrays and hashes (with comparison via
`#'equal`) are ShovelScript arrays and hashes and CL strings, integers
and double precision floats are ShovelScript's strings, integers and
floats.

Let's make our user-defined primitives easier to use:

    (defvar *udps* (list (list "print" #'princ 1)
                         (list "printLn" (lambda (x)
                                           (princ x)
                                           (terpri)
                                           :null) 1)
                         (list "readLine" #'read-line 0)))

ShovelScript is not object oriented, but this does not make it object
disoriented. Closures are objects, but closures + hashes = convenient
objects:

    (shovel:run-code (list "
    var Person = fn (name, age) {
      var result = hash('name', name,
                        'age', age)
      result.getDescription = fn () 'A Person: name = ' +
        result.name + ', age = ' + string(result.age)
      result
    }
    var john = Person('John', 40)
    @printLn(john.getDescription())
    @printLn(john.age)
    @printLn('representation for \"john\": ' + stringRepresentation(john))
    ")
                     :user-primitives *udps*)
    A Person: name = John, age = 40
    40
    representation for "john": hash("age", 40, "getDescription", [...callable...], "name", "John")
    NULL

Notes: `result` is an expression used as the the last statement in
`Person` to make it the result of `Person`. `stringRepresentation`
tries to provide as much information about its argument, with the
exception of callable objects (callables are displayed as
`[...callable...]`) and circular structures (back-pointers are
displayed as `[...loop...`]). `string` is used in `Person` to convert
`age` to a string - it is an error to `+` numbers and strings.

## Where were we? ... interruptible programs

The point of writing Shovel was getting an easy way to write
interruptible programs. The function `run-code` we used so far is a
convenient wrapper for testing ShovelScript programs. To show off
interruptible programs, we need some context and more
functions. Context first.

Suppose we need to expose a few functions via a RPC mechanism. We need
to make calls of several of these functions at once (the sequence is
not known in advance and it may branch depending on results) and we
want to group calls into transactions (they all execute or none
executes). One way would be to write an extra function for every
scenario, and also expose these larger functions via RPC. This way
doesn't scale very well (every new use case requires changing the code
on the server), and with Shovel we can do better.

On the server we'll accept Shovel programs instead of plain RPC
calls. This way, to make a 'Shovel RPC call', the client would start a Shovel
VM, provide it with user-defined primitives (UDPs) to access arguments
for the calls on the client and with an UDP to 'transfer to the
server' (say `goToServer`). The server would also accept a serialized
Shovel VM, resume it and provide it with UDPs for the functions we
wanted to expose in the first place, plus `goToClient`.

Then the generic scenario would go like this:

 * the client would compile a ShovelScript program and run it until
   `goToServer` is encountered;
 * after running `goToServer` the VM state and bytecode are serialized
   and sent to the server;
 * the server starts a transaction and resumes the VM received from
   the client;
 * once the VM finished its job, it calls `goToClient`;
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
this (see section 'Quotas' in [ShovelVmSpec.md](ShovelVmSpec.md)). I
will also show how to use these quotas.

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
orders in a bank transaction queue (which is then verified - maybe by
humans), and we return to the client the date of the bank transaction
deadline (by that date the bank guarantees the transfer is actually
executed).

A simplified simulated setup:

    (defvar *transfer-ready* nil)
    (defun get-source-account-no () 2)
    (defun get-destination-account-no () 4)
    (defun get-amount () 1)
    (defun go-to-server ()
      (setf *transfer-ready* t)
      (values :null :nap))

The four `get-*` functions provide some test data. `go-to-server`
marks the transfer as ready to go to server. By returning a second
value of `:nap`, it signals the Shovel VM to go to sleep. We need
`*transfer-ready*` (or another way to signal to the Shovel VM host
that `go-to-server` actually executed) because the VM may have gone to
sleep because of an exceeded CPU quota and we want to be sure that we
actually executed the ShovelScript program up to and including the
call to `go-to-server`.

We use global variables to simulate the communication between the
client and server:

    (defvar *sent-bytecode* nil)
    (defvar *sent-sources* nil)
    (defvar *sent-state* nil)

It is not mandatory to send the sources, but the error messages will
make more sense if the source is included.

The client to server call is simulated by setting these variables on
the 'client' side and reading them on the 'server' side. The server to
client reply is simulated by setting these variables on the 'server'
side and reading them on the 'client'.

The code to make the call from the client:

    (let* ((sources (list "
    // Setting up the call on the client.
    var sourceAccountNo = @getSourceAccountNo()
    var destinationAccountNo = @getDestinationAccountNo()
    var amount = @getAmount()

    // Go to the server.
    @goToServer()

    // Run the code on the server.
    @subtractFromAccount(sourceAccountNo, amount)
    @addToAccount(destinationAccountNo, amount)
    var transactionDeadline = @getTransactionDeadline()

    // Go back to the client.
    @goToClient()

    // Handle the results on the client.
    @setTransactionDeadline(transactionDeadline)
    "))
           (user-primitives (list (list "getSourceAccountNo"
                                        #'get-source-account-no 0)
                                  (list "getDestinationAccountNo"
                                        #'get-destination-account-no 0)
                                  (list "getAmount" #'get-amount 0)
                                  (list "goToServer" #'go-to-server 0)))
           (bytecode (shovel:get-bytecode sources)))
      (setf *transfer-ready* nil)
      (multiple-value-bind (result vm)
          (shovel:run-vm bytecode :sources sources
                         :user-primitives user-primitives)
        (declare (ignore result))
        (unless *transfer-ready*
          (error "Something is wrong."))
        (let ((serialized-bytecode (shovel:serialize-bytecode bytecode))
              (serialized-state (shovel-vm:serialize-vm-state vm)))
          ;; send request to server:
          (setf *sent-sources* sources)
          (setf *sent-bytecode* serialized-bytecode)
          (setf *sent-state* serialized-state))))

At this point we have moved the action to the server. Let's define the
server UDPs:

    (defun subtract-from-account (account amount)
      (declare (ignore account amount))
      :null)
    (defun add-to-account (account amount)
      (declare (ignore account amount))
      :null)
    (defun get-transaction-deadline ()
      "Judgment Day")
    (defun go-to-client ()
      (setf *transfer-ready* t)
      (values :null :nap))

We are now ready to resume the ShovelVM on the server:

    (let ((bytecode (shovel:deserialize-bytecode *sent-bytecode*))
          (sources *sent-sources*)
          (vm-state *sent-state*)
          (user-primitives (list (list "subtractFromAccount"
                                       #'subtract-from-account 2)
                                 (list "addToAccount"
                                       #'add-to-account 2)
                                 (list "getTransactionDeadline"
                                       #'get-transaction-deadline 0)
                                 (list "goToClient"
                                       #'go-to-client 0))))
      (setf *transfer-ready* nil)
      (multiple-value-bind (result vm)
          (shovel:run-vm bytecode
                         :sources sources
                         :user-primitives user-primitives
                         :state vm-state)
        (declare (ignore result))
        (unless *transfer-ready*
          (error "Something is wrong."))
        (let ((serialized-bytecode (shovel:serialize-bytecode bytecode))
              (serialized-state (shovel-vm:serialize-vm-state vm)))
          ;; send reply to client:
          (setf *sent-sources* sources)
          (setf *sent-bytecode* serialized-bytecode)
          (setf *sent-state* serialized-state))))

The server replied by updating the `*sent-*` variables. We need one
more UDP on the client:

    (defun set-transaction-deadline (deadline)
      (format t "The bank promised to do it by ~a." deadline)
      :null)

Now we can run the final part of the ShovelVM code on the client:

    (let ((bytecode (shovel:deserialize-bytecode *sent-bytecode*))
          (sources *sent-sources*)
          (vm-state *sent-state*)
          (user-primitives (list (list "setTransactionDeadline"
                                       #'set-transaction-deadline 1))))
      (multiple-value-bind (result vm)
          (shovel:run-vm bytecode
                         :sources sources
                         :user-primitives user-primitives
                         :state vm-state)
        (declare (ignore result vm))))
    The bank promised to do it by Judgment Day.
    
We're done with our RPC call, and the bank promised to transfer the
money soon. In fact, that final message makes me feel so good I want
to see it again (we can resume a VM more than once):

    (let ((bytecode (shovel:deserialize-bytecode *sent-bytecode*))
          (sources *sent-sources*)
          (vm-state *sent-state*)
          (user-primitives (list (list "setTransactionDeadline"
                                       #'set-transaction-deadline 1))))
      (multiple-value-bind (result vm)
          (shovel:run-vm bytecode
                         :sources sources
                         :user-primitives user-primitives
                         :state vm-state)
        (declare (ignore result vm))))
    The bank promised to do it by Judgment Day.

Lots of details got glossed over, some 'trivial' (such as actually
sending data back and forth between a client and a server, taking care
of exceptional situations, or wrapping the `run-vm` call on the server
in a transaction), some not so trivial (i.e. requiring more knowledge
of Shovel - redoing a `run-vm` in case it returned because of an
exceeded CPU quota, setting CPU and RAM quotas etc.). Some of the
tasks that require more information about Shovel are documented in the
description of an example which is slightly closer to real programming
tasks: [WebGuessNumber.md](WebGuessNumber.md).

[TBD] reduce transfered data by checking if the server doesn't already
have this program.

[TBD] simplify this section by removing *transfer-ready* and
associated text - the web-guess-number example will introduce similar
mechanism.

[TBD] a similiar simplification about *sent-sources* - we will
introduce the discussion about sending sources after discussing how to
reduce traffic via hash checking.
