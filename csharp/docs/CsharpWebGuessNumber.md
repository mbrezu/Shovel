<!-- -*- markdown -*- -->

# A Guess-the-Number Game with Shovel and C\# #

## Introduction

This document is written with the assumption that the reader is
familiar with
[the Shovel C# getting started guide](CsharpGettingStarted.md).

It covers an extended example of using interruptible Shovel VM
programs.

## Local Game

First, a local, non-interruptible version of the number guessing game.

The computer picks a number between 1 and 100 and asks the user to
guess what number it picked. For each user guess, the program replies
with 'too small', 'too large' or 'you guessed it'. When the user
guesses the number, they are congratulated and offered another game.

This section describes the code in the project
`Demos\_01_GuessTheNumberLocal`.

The ShovelScript code used:

    var game = fn () {
      var secretNumber = floor(@random() * 100 + 1)
      var attempt = 0
      var iteration = fn () {
        attempt = attempt + 1
        @print('Attempt ' + string(attempt) + ' - ')
        @print('enter a number between 1 and 100: ')
        var guess = @readInt()
        if guess < secretNumber {
          @printLn('Too small!')
          iteration()
        }
        else if guess > secretNumber {
          @printLn('Too large!')
          iteration()
        }
        else {
          @printLn('You guessed it in ' + string(attempt) + ' attempts! Congratulations!')
          @print('Another game? (y/n) ')
          if @readChar() == 'y' game()
        }
      }
      iteration()
    }

    game()

Iteration-via-recursion will please some people (hello, Scheme fans!)
; the others are allowed to be somewhat disgusted (maybe your disgust
will diminish if I tell you that the ShovelScript compiler optimizes
tail calls - hello again, Scheme fans!).

The primitives are defined in the `Udps` method. The primitives are:

 * `random` - returns a random `double`  between 0.0 and 1.0,
 * `print` - which prints a string to the console,
 * `printLn` - which is like `print` but it starts a new line after printing the content,
 * `readInt` - which reads a line from the console and parses it as an
   int (for the sake of simplicity, error checking has been carefully
   avoided; feel free to make `readInt` handle parsing errors better)
   and
 * `readChar` - which reads a line from the console and returns the first character in it.

Running the project will let you play the game in a console window.

If I remember correctly, you're not here to play little games
abundantly sprinkled with the best BASIC-in-the-early-80s scents,
you're here to learn more about writing interruptible Shovel programs.

## Web Version of the Game - Version 1

The challenge: turn this program into a little web site without
changing the Shovel program for the game - the only things allowed to
change are the UDPs and the environment provided with them.

The general idea:

 * use a web server (of course);
 * when a web page is requested and there is no serialized Shovel VM
   proces, a new Shovel VM process is created;
 * when printing messages, they are accumulated into content which
   will be rendered on web pages;
 * when the user is asked for input, a page with a HTML form is
   generated, the VM is interrupted and serialized, the page is
   served;
 * when the page is posted again to the server, the VM is resumed and
   passed the value from the form;
 * when the VM completes running (the user answers 'no' to the
   question at the end of the game), it is restarted.

Our first version will only deal with one user, for the sake of
simplicity. Adding a database backend and support for multiple users
is 'Version 2' stuff (next section).

We'll build a simple web server based on `HttpListener`. If you're
using Visual Studio and Windows Vista or newer, you'll need to start
Visual Studio 'as administrator' so your `HttpListener` instance will
be able to start listening for HTTP requests.

The code for this section is in project
`Demos\_02_GuessTheNumberWebOne`. It listens for HTTP requests at
`http://localhost:8080`.

The UDPs definitions (as usual, in method `Udps`) have
changed. `print` and `printLn` accumulate content in a static global
variable (remember, only one user for now). `readInt` and `readChar`
check a global variable, `readState`, which tells them if they have a
value to return to the user or not. If not, they will set `readState`
to reflect the object to be read from the user (an int or a char), and
stop the Shovel virtual machine by setting `result.After` to
`Shovel.UdpResult.AfterCall.NapAndRetryOnWakeUp`. This means that the
Shovel VM, when restarted, will call the UDP (`readInt` or `readChar`)
again. At that time, the `userInput` global will contain the string
typed by the user.

Let's have a look at `ServeGuessNumberRequest`, which handles the HTTP
requests. 

For now, ignore the `userInput` variable.

It compiles and starts the virtual machine using the state from
`shovelVmState` (which is initially `null`, meaning that the process
doesn't have any state and the VM will start executing the program
from the beginning).

When `RunVm` returns, it means that a) program execution has completed
or b) `readInt` or `readChar` have been called.

If a), we reinitialize everything and run things again from the start
of the ShovelScript program. We rely on the fact that our ShovelScript
program will call `readInt` or `readChar` before finishing.

If b), we need to do the HTTP equivalent of `Console.ReadLine`. This
means writing the page accumulated so far to the HTTP response, add a
form with a text box and focus the textbox (to make it easy to enter
the number or character and to make sure that the form is visible -
the user doesn't need to scroll down to see it).

The next time we'll be called, `userInput` will have some content
(from the HTTP GET parameter `input`). The Shovel VM will restart,
read the state, retry the call to `readInt` or `readChar` (which, this
time, will use the value from `userInput` and allow the VM to continue
executing).

Our web version doesn't really obey the user's wish at the end of the
program. If the user says 'no, I don't want another game', they still
get one (but in a fresh Shovel VM).

The HTML is as horrible as the JavaScript. Fix them if you like :-)
Furthermore, the current application supports only one user, it
doesn't save the state to some form of persistent storage and it
doesn't handle page reloads correctly. Time for version 2.

## Web Version of the Game - Version 2

The project `Demos\_03_GuessTheNumberWebMany` contains the code for
the new version.

To support multiple users, we use sessions stored in a database,
instead of just one session for everyone, stored in global
variables. Since these are just small examples and the goal is to run
them easily on both MonoDevelop/Mono/Linux and Visual
Studio/.NET/Windows, I chose to store the sessions in the file system
(too lazy to find out if there is, say, a Sqlite wrapper that won't
require different solution/project files to be usable on both Mono and
.NET for these examples).

The 'database' is implemented in `FileSystemDatabase.cs`. Features:

 * stores records as directories (one record per directory), with one
   file per field - the file name is the field name, the file content
   is the `byte` array representing the field's value;
 * the directory for each record is named by the value of the primary
   key (e.g. directory '100' for the record corresponding to primary
   key value '100');
 * all the 'record directories' are stored in a 'database directory';
 * locking is very primitive: all operations block on the current
   instance of the 'database server', so different processes can
   corrupt the data and all operations lock the entire 'database';
 * ... that's about it. Good enough for these examples.
 
The sessions are implemented by class `Session.cs`. Its fields are the
globals of the previous version, plus an integer key (the session
ID). A session knows how to save itself to a 'database' and the class
can create a session by loading it from the database.

There are a few differences in `Main.cs`. The
`ServeGuessNumberRequest` also reads a `sessionid` HTTP GET
parameter. Based on the presence of the `sessionid` in the 'database',
it wakes up an existing session or starts a new one.

If the Shovel VM completed program execution, it starts a new session
by redirecting to "/". Otherwise it serializes the state.

Note: try to ignore the commented lines after the call to
`SerializeVmState`. They are a spoiler :-)

Next, it saves the session to disk and writes the HTML to be served to
the browser. Note the hidden `sessionid` field which saves the ID of
the current session.

The `Udps` method has changed a bit. It has two arguments now, a
session and a string (the user input). The code is adapted to use the
fields of the session instead of the global variables (which are no
longer present). Besides these changes, the `Udps` code from the last
section is hopefully recognizable in the new version.

You can see the sessions in action. Starting this project, pointing
two browser windows at `http://localhost:80` and playing the game in
both should make it obvious they don't share state anymore.

### Page Reloads, the Back Button, Cloned Pages

There is still a big problem: reloading the page does not work
correctly (it processes the input again). During the guessing phase of
the game this is minor (it adds an extra try), but if one reloads
immediately after answering 'y' or 'n' to the final question in the
game, that character is interpreted as a number guess and the code
handling the HTTP request crashes. Our code is not correct - we
obviously should not process the same input twice.

One thing to try is HTTP redirecting the page after processing each
input so that there is no input anymore. This is tricky to get right
and it doesn't really solve all the associated problems, like the back
button and cloned pages.

The back button almost works (it returns to the previous page, but the
state of the session doesn't 'go back'). Furthermore, it could be
argued that entering a different input after using the back button
should branch the game (create a different 'future' - the guesses made
in the pages we backed up from are one branch (or 'future') of the
game; by entering other inputs we create a new branch/future). This is
important, if the back button works properly, we could cheat the
game: guess the number, then use the back button as much as we like
(maybe up to the first guess?) and then enter the secret number
directly.

Cloned pages are a similar problem: imagine that the user copies the
URL for one of the attempt pages, then pastes that address into
several other browser windows. He's probably trying to cheat, but
still - we shouldn't crash or process inputs twice (his cloned windows
should be mirrored by cloned sessions in our application).

The solution is obviously not HTTP redirects. One way to simulate this
branching is actually branching the VM states: create a new session
every time the session is saved. This way, we get persistent sessions,
for another value of 'persistent' (see for instance
[Wikipedia's *Persistent data structure* article](https://en.wikipedia.org/wiki/Persistent_data_structure)). These
are versioned sessions (mmm... time travel). In real applications we
have to account for state outside the sessions (launched missiles,
sent emails, purchased products etc.) - so versioning will probably
break down for 'outside state' and there's a need to compesate (show
the user a list of recently bought products so they don't buy
something twice by mistake). Anyway, our sessions are now 'values',
which turns out to be
[a very good thing](http://www.infoq.com/presentations/Value-Values).

Of course more sessions mean more disk space. For our toy application
this doesn't really matter - in a real application we'd have to delete
the old sessions now and then anyway. In a real application we may
also store data incrementally (maybe store diffs between session
versions?).

Implementing branching sessions is easy: just uncomment the

    //session.Id = fsd.GetFreshId ();
    
line in `ServeGuessNumberRequest`.

Try running the game again. See if you can guess the secret number in
one attempt!

A short aside about storage: our storage arrangements are a bit
wasteful - for each session we store not only the VM state, but also
the bytecode and source code. Usually the number of processes
(represented by VM states) is much, much larger than the number of
programs (represented by bytecode and source code). It makes sense to
store programs in a separate table and store pointers to that table in
the 'Sessions' table (such as the MD5 hashes of the bytecode and
source). This improvement is left as an exercise to the reader.

Adding a `LastAccessTime` field to a session (to be updated on disk
when the session is loaded) and deleting sessions older than, say, a
few hours, is also left as an exercise to the reader (since the file
system 'database' makes it rather ineffective to iterate over sessions
- and you'd have to write the code to do it -, you probably want to
switch over to a real database first - and put an index on
`LastAccessTime`). Since we have versioned sessions, maybe it's an
interesting idea to store the 'parent' session for each session and
update the 'LastAccessTime' for all parents recursively, so the 'back
button' still works for the entire branch (branches are either kept or
deleted as a whole). Implementing this idea is another exercise.

## Turning the Tables: the User Writes the Shovel Program

Time to go with the cloud (err... crowd) and let the users write the
programs (it's supposed to be more interesting for some of them).

It's not hard to transform the existing code to accept the source from
the user. The general idea:

 * when starting a fresh session, present the user with a HTML
   `textarea` where they can write the program; add buttons to fill
   the `textarea` with some predefined programs (e.g. the number
   guessing game);
 * when they press submit, a session is created if the program is
   compiled successfully; if not, the `textarea` with the program is
   shown again and below it the errors from the compiler - this should
   help them fix their program;
 * the program can use the UDPs provided to the number guessing game;
   an additional `readLine` user-defined primitive is added to give
   some extra freedom;
 * to help with debugging, if the program crashes the error message is
   showed on a new page and the user is presented with links to 1) go
   back one step and 2) start a new session/write a new program.

This idea is implemented in project
`Demos\_04_GuessTheNumberTurnTheTables`.

`Udps` changes very little (`readLine` is added).

The `ServeRequest` method is where the real action starts. Since all
the requests come to the same path, the different kinds of requests
are differentiated by the HTTP method and the presence or absence of
parameters.

If we get a POST, then the user probably just submitted a
program. Call `CompileProgram` to handle this situation.

Otherwise, look for the presence of `sessionid`. If not present, call
`ServeTheTextArea` to prompt the user to enter a ShovelScript
program. If the `sessionid` is invalid, also call
`ServeTheTextArea`. If we manage to load a session from disk, call
`ServeSession` to handle it.

`CompileProgram` reads in the body of the HTTP request and parses it
as a POST request to extract the `program` (is there a method to ask
HttpListener/HttpUtility/whatever to do this? - it seems odd to do
POST parameter extraction by hand). It then tries to compile the
program, creates a new session for the program and calls
`ServeSession` if everything goes well. If program compilation fails,
`ServeTheTextArea` is called again, with the current source and the
error message (to make it easy for the user to fix their program).

`ServeTheTextArea` serves a HTML page with a textarea (optionally
filled in with some source code). If an error message is passed to
`ServeTheTextArea`, it shows that too.

`ServeSession` is where the interesting stuff happens (i.e. `RunVm` is
called). If the execution of `RunVm` results in a run-time error, the
error is shown to the user via `ServeError`. On program completion, a
message is printed (if the program doesn't end by calling a `read*`
UDP, then the final part of the program's execution and/or messages
will be missed because a fresh session is started). Otherwise, it
means that a `read*` UDP has been called - and the already familiar
code serving the page content and a form follows.

### Adding CPU and RAM Quotas

CPU and RAM quotas are described in
[ShovelVmSpec.md](../../docs/ShovelVmSpec.md), section 'Quotas'. We
can specify that a VM won't use more than 100000 cells (that should be
under a megabyte) and not more than 1000000 ticks total (most probably
much less than a second total run-time - the time spent waiting for input
from the user is not counted). Just change the `RunVm` call in
`ServeSession` by changing the quota parameters from null to 100000:

    var vm = Shovel.Api.RunVm (
        Shovel.Api.DeserializeBytecode (session.ShovelVmBytecode), 
        ProgramSources (session.ShovelVmSources), 
        Udps (session, userInput), 
        session.ShovelVmState,
        totalTicksQuota: 100000,
        ticksUntilNextNapQuota: null,
        usedCellsQuota: 100000);


To make the server more resilient, we could limit the number of
threads serving requests and we could use another quota,
`ticksUntilNextNapQuota` (the VM is put to sleep after
`ticksUntilNextNapQuota` ticks and another VM can be resumed - but we
have to write the scheduler, all Shovel does is return from `RunVm`
when this quota is exceeded - we can determine that the quota is
exceeded if a) the program didn't complete and b) the `ReadState` field
of the current session is `Session.ReadStates.None`). Both these
changes are beyond the scope of this document.

That's all, folks!
