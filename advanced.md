<!-- -*- markdown -*- -->

# Advanced (as in Non-Trivial) Topics

This document records how to approach a few non-trivial subjects
related to long-running, interruptible computations. The uses cases
are from business modelling problems.

## Parallel processes

If a Shovel process is associated with a document and that document is
handled in parallel by multiple actors (or needs to wait for one of
several events to occur), using just one Shovel VM may be inconvenient.

Parallel processes in business modelling in can be split in
Parallel-And and Parallel-Or classes.

Parallel-And means that processing continues after all the parallel
branches have completed (similar to thread joining in concurrent
programming).

Parallel-Or means that processing continues as soon as one branch
finishes.

It turns out that Parallel-And and Parallel-Or can be implemented
without changes to the actual Shovel language or VM. It is enough if
the framework in the host language:

 * manages multiple Shovel VMs (one for each active parallel process)
   for each document/business object and
 * provides UDPs for:
   * creating new processes,
   * recording some state as the result of the current branch,
   * waiting for a process to finish (and get some state out of it)
     and
   * signalling that siblings out to be thrown away and that
     processing should continue with the current process (to implement
     Parallel-Or modelling).
     
Parallel in this case means that we have multiple processes 'active'
at the same time (either running or waiting for an event). It doesn't
mean that the processes are executed in parallel - it is guaranteed
that only one of the processes is actually running at any given time.

### Implementing Parallel-And

Let's start with some example code:

    var pid1 = @allocatePid()
    var pid2 = @allocatePid()
    var pid3 = @allocatePid()
    var pid = @fork(pid1, pid2, pid3)
    if pid == pid1 {
      // ... the action on branch 1 ...
      @finish(state1)
    } else if pid == pid2 {
      // ... the action on branch 2 ...
      @finish(state2)
    } else if pid == pid3 {
      // ... the action on branch 3 ...
      @finish(null)
    }
    var stateDict = @wait(pid1, pid2, pid3)
    
Each document has an associated pool of Shovel VMs; each active Shovel
VM has an associated 'process id' (PID). Each document can have at
most N (say 1024) processes running simultaneously; the used and free
pids for each document are managed by the host language framework
(implement the set of used pids as a bit map?).

`@allocatePid` allocates a PID from the pool of free PIDs for the
current document and returns it.

`@fork` stops the current computation, serializes it and creates a new
Shovel process/VM for each argument, each associated with the PID
given as argument. The current process is thrown away BUT its PID is
NOT FREED; only the child processes remain. Since these processes are
run in separate Shovel VMs, they do not share any state - except via
UDPs.

The PID of the parent process is recorded with the list of children in
a list of 'fork records'. A 'fork record' contains:

 * the PID of the parent process and
 * a set containing the child PIDs passed to `@fork`.

In each branch process, `@fork` returns the pid of the current
process, so the code can branch on this value and execute branch
specific code. 

To signal branch completion, a branch process has to call `@finish`.
If the process means to send state to the process that will continue
after the branches are joined, it will call `@finish` with the value
representing the 'branch result'. If there is no significant branch
result, `@finish(null)` will do (see the third branch in the example
above). `@finish` does not suspend the process, it just marks it as
finished. Calling `@finish` multiple times on the same branch is an
error.

The first process to finish will reach the `@wait` statement. `@wait`
takes PIDs as arguments and returns only when all the processes
associated with the PIDs have called `@finish`. It returns a hash
containing (PID, finish-argument) pairs representing the results from
the branches. When `@wait` returns, only the process in which `@wait`
returned is still active, the others have been discarded and their
PIDs marked as free.

`@wait` creates a set out of the PIDs passed as arguments and checks
if there is a 'fork record' (see above) containing the same set of
child PIDs. If there is none, an error is thrown. If there is, that
'fork record' is thrown away and the PID of the current process is
changed to the parent PID recorded in the 'fork record'. The former
PID of the current process is freed. Matching child PID sets for
`@fork` and `@wait` pairs these calls thus preventing accidental
branch joins. Patching the current PID makes nested forks work (if the
parent branch is a child in another `@fork` call, it needs to have the
PID assigned by that `@fork` call when it calls `@finish` in order to
be marked as finished).

Patching the current PID means that one should not rely on the PIDs
being constant. This should not be a problem, as from the ShovelScript
programmer's point of view PIDs are only a way to identify branches
and match branches with branch results (notice that there is no
`@getPid` call, nor is it necessary). Maybe the PIDs should be called
CIDs/BIDs (child/branch identifiers) at the ShovelScript level
(because that is what they mean at that level).

It is an error if `@wait` is called from a branch process which has
not yet called `@finish`.

It is an error if the PID of the current process is not passed to
`@wait`.

Obviously, `@wait` could take forever if one of the branches fails to
call `@finish` (requiring that callers of `@wait` have already called
`@finish` alleviates the problem - if a process doesn't call `@finish`
but reaches `@wait` it will be terminated; in this case the host
language framework should set the 'branch result' of that process to
something describing the error; the 'branch result' is normally set by
calling `@finish`).

Parallel processes provide an interesting but probably expensive (in
Shovel) way of doing exception handling: spawn a process to perform an
operation with potential exceptions, look at the value returned by
`@wait` to determine if the operation performed normally or somehow
failed. An aproximation of the 'Erlang way'.


### Implementing Parallel-Or

This is very similar to Parallel-And, the only difference is replacing
`@wait` with `@discard` (described below). The example, modified for
Parallel-Or:

    var pid1 = @allocatePid()
    var pid2 = @allocatePid()
    var pid3 = @allocatePid()
    var pid = @fork(pid1, pid2, pid3)
    if pid == pid1 {
      // ... the action on branch 1 ...
      @finish(state1)
    } else if pid == pid2 {
      // ... the action on branch 2 ...
      @finish(state2)
    } else if pid == pid3 {
      // ... the action on branch 3 ...
      @finish(null)
    }
    var stateDict = @discard(pid1, pid2, pid3)
    
`@discard` doesn't wait for other branches to finish (i.e. call
`@finish`). The current branch is guaranteed to have finished, or else
it would not have reached the `@discard` call. Other branches may have
finished as well, and the state they passed to `@finish` is found in
`@discard`'s result.

Other than not waiting, `@discard` behaves exactly like `@wait`:

 * it throws an error if the current process hasn't called `@finish`;
 * it throws an error if the current process isn't passed to `@discard`;
 * it throws an error if it can't find the set of PIDs passed to it in
   any 'fork record';
 * it deletes the 'fork record' and patches the PID of the current
   process to be the parent PID from the 'fork record';
 * it frees the PID of the current process;
 * it throws away the processes listed which are not the current
   process and frees the PIDs.
   
### Conclusion

Parallel active processes (active meaning running or serialized and
waiting for an event) associated with a single document/business
object can be implemented with the current Shovel VM, by providing the
following five UDPs and the associated ecosystem in the host language:

 * `@allocatePid`
 * `@fork`
 * `@finish`
 * `@wait`
 * `@discard`
 
The biggest issue with this approach is that the writer of the Shovel
program needs to be careful about calling `@finish` and not messing up
the variables holding the PIDs. So there's plenty of room for
programmer error.

Programmer error can be controlled somehow by writing functions like
`ParallelAnd3` (below) which take 3 closures representing the branches
and one closure taking a list of results from branches. Since
ShovelScript doesn't yet have `apply`, we need a separate function for
each number of branches (we would need `apply` to call `@fork` and
`@wait` with a variable number of arguments, assuming that the
branches are passed as an array) - so we'll need `ParallelAnd2`,
`ParallelAnd3`, `ParallelAnd4` and so on.

Example implementation:

    var ParallelAnd3 = fn(branch1, branch2, branch3, after) {
      var pid1 = @allocatePid()
      var pid2 = @allocatePid()
      var pid3 = @allocatePid()
      var pid = @fork(pid1, pid2, pid3)
      if pid == pid1 {
        @finish(branch1())
      } else if pid == pid2 {
        @finish(branch2())
      } else if pid == pid3 {
        @finish(branch3())
      }
      var results = @wait(pid1, pid2, pid3)
      after(results[string(pid1)], results[string(pid2)], results[string(pid3)])
    }
    
Example usage:

    var branch1 = fn () {
      @doSomething()
      var result = @waitForSomething()
      result[0]
    }
    var branch2 = fn () {
      @doSomethingElse()
      var result = @waitForSomethingElse()
      result[1]
    }
    var branch3 = fn() {
      var result = @doSomethingEntirelyDifferent()
      result[7]
    }
    ParallelAnd3(branch1, branch2, branch3, fn results {
      @useResults(results[0], results[1], results[2])
    })
    
So using branches can be abstracted by ShovelScript functions - the
programmer only needs to know about `@ParallelAnd3` and friends.

## Transactions

We have two problems to solve: 

 * long-running transactions (e.g. just providing `@beginTransaction`,
   `@commit` and `@rollback` primitives mapped to corresponding
   operations in the host language won't do, we want transactions to
   be able to 'wrap' interruption points and migrate along with the
   potentially interrupted Shovel process in time and/or space);
 * transactions with compensating actions (e.g. we needed to send an
   email as part of the transaction, and continue inside the
   transaction based on some result of sending the email, but the
   transaction failed later and we need to send a 'compensating' email
   which 'cancels' the first one - for instance, by saying it was sent
   by mistake).

The solutions use the current Shovel implementation without changes.

## Long-Running Transactions

Instead of actually performing an action, UDPs called between
`@beginTransaction` and `@commit`/`@rollback` simply record the action
in a TODO list. The TODO list is serialized along with the Shovel VM
if the process is interrupted. `@commit` actually performs the actions
in the TODO list and `@rollback` throws the TODO list away.

Obviously, this approach easily also allows other tricks (e.g placing
checkpoints in the TODO list and rolling back to a given
checkpoint). The only requirement is the ability to serialize the
action for later execution.

Open question: what happens when a `@commit` fails? Possible answer:
`@commit`'s result encodes 'success' or 'failure', possibly with more
information for each (e.g. a log of executed operations for success or
an explanation for the failure).

## Transactions with Compensating Actions

Use the same TODO list idea, but keep an additional 'compensating'
TODO list (COTODO list :-) ). On `@rollback` run the actions in the
COTODO list before throwing the lists away. On `@commit` simply throw
the COTODO list away (if the commit succeeds). Questions: Should
`@commit` run the COTODO list if it fails? Should the COTODO list be
run manually via an UDP call (used if the `@commit` call failed)? My
guess is the safest bet is to make `@commit` run the COTODO list in
case of failure. What if actions in the COTODO list fail? `@commit`
should return a log of executed COTODO list actions.

For instance, a 'send email now' operation in a transaction will send
the email immediately without waiting for the `@commit` call, but take
two parameters: the email to send and the compensating email. The
action of sending the compensating email is placed in the
COTODO list.

# Thanks

The current document resulted from a discussion with Cristian Ionita -
he suggested most of the use cases, and together we came up with some
ideas about handling the use cases. All mistakes and omissions in the
current document are my own.
