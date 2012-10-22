<!-- -*- markdown -*- -->

# A Guess-the-Number Game with Shovel and Common Lisp

## Introduction

This document is written with the assumption that the reader is
familiar with
[the Shovel Common Lisp getting started guide](ClGettingStarted.md).

It covers an extended example of using interruptible Shovel VM
programs.

## Local Game

First, a local, non-interruptible version of the number guessing game.

The computer picks a number between 1 and 100 and asks the user to
guess what number it picked. For each user guess, the program replies
with 'too small', 'too large' or 'you guessed it'. When the user
guesses the number, they are congratulated and offered another game.

Load Shovel: `(ql:quickload :shovel)`.

First, let's define some user-defined primitives (UDPs):

    (defparameter *udps* (list (list "print" #'princ 1)
                               (list "printLn" (lambda (x)
                                                 (princ x)
                                                 (terpri)
                                                 :null) 1)
                               (list "readChar" (lambda ()
                                                  (string (elt (read-line) 0)))
                                     0)
                               (list "readInt" (lambda ()
                                                 (nth-value 0 (parse-integer (read-line))))
                                     0)
                               (list "random" (lambda () (random 1.0d0)) 0)))

(for the sake of simplicity, error checking has been carefully avoided
- feel free to make `readInt` handle parsing errors better)

Now the game program:

    (defparameter *sources* (list "
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
    "))

Play the game:

    (shovel:run-code *sources* :user-primitives *udps*)

Iteration-via-recursion will please some people (hello, Scheme fans!)
- the others are allowed to be somewhat disgusted (maybe your disgust
will diminish if I tell you that the ShovelScript compiler optimizes
tail calls - hello again, Scheme fans!).

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

The web server: `(ql:quickload :hunchentoot)`. Start a web server:

    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                      :port 4242))

Define a global variable to hold the page content:

    (defvar *page-content* "")

Define the simple-minded CL functions for the `print` and `printLn`
UDPs:

    (defun web-print (content)
      (setf *page-content*
            (with-output-to-string (str)
              (write-string *page-content* str)
              (write-string "<span>" str)
              (write-string (hunchentoot:escape-for-html content) str)
              (write-string "</span>" str))))

    (defun web-print-ln (content)
      (setf *page-content*
            (with-output-to-string (str)
              (write-string *page-content* str)
              (write-string "<span>" str)
              (write-string (hunchentoot:escape-for-html content) str)
              (write-string "</span><br/>" str))))

A global variable to remember what we asked the user:

    (defvar *user-read* nil)

(`nil` means we don't want anything from the user, `:char` means we
want to read a character, `:int` means we need an integer)

The CL functions for `readInt` and `readChar`:

    (defun web-read-char ()
      (case *user-read*
        ((nil)
         (setf *user-read* :char)
         (values :null :nap-and-retry-on-wake-up))
        (:char
         (setf *user-read* nil)
         (string (elt (hunchentoot:get-parameter "input") 0)))))

    (defun web-read-int ()
      (case *user-read*
        ((nil)
         (setf *user-read* :int)
         (values :null :nap-and-retry-on-wake-up))
        (:int
         (setf *user-read* nil)
         (nth-value 0 (parse-integer (hunchentoot:get-parameter "input"))))))

These functions ask the VM to go to sleep, but retry the call when
resumed. They use the value of `*user-read*` to differentiate between
'hey, we need to send a page to the user' and 'oh, the VM was resumed,
called us again and the user provided some valuable input'. Again,
error handling was avoided carefully - you guessed it, for
simplicity's sake.

The code for `random` is the same, so we're ready to re-assemble the
value for `*udps*`:

    (defparameter *udps* (list (list "print" 'web-print 1)
                               (list "printLn" 'web-print-ln 1)
                               (list "readChar" 'web-read-char 0)
                               (list "readInt" 'web-read-int 0)
                               (list "random" (lambda () (random 1.0d0)) 0)))

Finally, we need a place to keep the VM state:

    (defvar *vm-state* nil)

A Hunchentoot easy handler to put it all together:

    (hunchentoot:define-easy-handler (guess :uri "/guess") ()
      (setf (hunchentoot:content-type*) "text/html")
      (alexandria:when-let (input (hunchentoot:get-parameter "input"))
        (setf *page-content* (concatenate 'string
                                          *page-content*
                                          input
                                          "<br/>")))
      (run-guess-number-vm))
      
If there's a GET parameter named `input`, we add it to the content
accumulated in `*page-content*` (this is a simple emulation of echoing
the input back when reading from standard input at the console).

The helper functions for the easy handler:

    (defun run-guess-number-vm ()
      (multiple-value-bind (result vm)
          (shovel:run-vm (shovel:get-bytecode *sources*)
                         :sources *sources*
                         :state *vm-state*
                         :user-primitives *udps*)
        (declare (ignore result))
        (cond ((shovel:vm-execution-complete vm)
               (setf *vm-state* nil)
               (setf *page-content* "Restarted application.<br/>")
               (run-guess-number-vm))
              (t
               (setf *vm-state* (shovel:serialize-vm-state vm))
               (generate-page)))))

    (defun generate-page ()
      (with-output-to-string (str)
        (write-string *page-content* str)
        (write-string "<form action='/guess' method='get'>" str)
        (write-string "<input type='text' name='input' id='shovel-input'/>" str)
        (write-string "<input type='submit' value='Submit'/>" str)
        (write-string "</form>" str)
        (write-string
          "<script type='text/javascript'>
          document.getElementById('shovel-input').focus()</script>"
          str)))

Go to [`http://localhost:4242/guess`](http://localhost:4242/guess) and
play the game.

`run-guess-number-vm` runs the VM with the saved state (`nil` if we
start a fresh VM). If the VM went to sleep, its state is serialized
and a page is served via
`generate-page`. `shovel:vm-execution-complete` is `T` only if the VM
really finished executing (not just sleeping, it really ran out of
bytecode). In that case, we reset `*page-content*` and show a fresh
page (so the game restarts no matter what you reply to the question at
the end - you simply get a fresh VM if you say 'no').

`generate-page` creates the HTML page from the content accumulated in
`*page-content*` and a HTML form (used to get the user's input).

The HTML is as horrible as the JavaScript. Fix them if you like :-)
Furthermore, the current application supports only one user, it
doesn't save the state to some form of persistent storage and it
doesn't handle page reloads correctly. Time for version 2.

## Web Version of the Game - Version 2

As the project grows larger, working at the SLIME REPL without saving
code is getting unproductive. Time to create a CL system to hold our
work (you can get the code for this section from
[Github](http://github.com/mbrezu/shovel-guess)). First, load
[Quickproject](http://www.xach.com/lisp/quickproject/):

    (ql:quickload :quickproject)

Then create a `shovel-guess` project in your
`quicklisp\local-projects` directory (the command below assumes that
`quicklisp` is installed directly in your home directory, adjust it to
fit your setup):

    (quickproject:make-project "~/quicklisp/local-projects/shovel-guess/")

Add the necessary dependencies to your system definition file by
editing `shovel-guess.asd` (add the `:depends-on` clause for the
project):

    ;;;; shovel-guess.asd

    (asdf:defsystem #:shovel-guess
      :serial t
      :description "Describe shovel-guess here"
      :author "Your Name <your.name@example.com>"
      :license "Specify license here"
      :depends-on (#:shovel
                   #:hunchentoot
                   #:sqlite)
      :components ((:file "package")
                   (:file "shovel-guess")))

We'll use `hunchentoot` for the web server (as before) and add
`sqlite` for persistent storage. Load the system:

    (ql:quickload :shovel-guess)

Now copy the `defun`s, `defvar`s and `defparameter`s from the previous
section in the `shovel-guess.lisp` file (wrap the `hunchentoot:start`
call into a parameterless `start-server` function).

Load `:shovel-guess` again to load the new code in `shovel-guess.lisp`
and switch to the `shovel-guess` package:

    (in-package :shovel-guess)

Now you can start the server:

    (start-server)

The game from the previous section is again available at
[`http://localhost:4242/guess`](http://localhost:4242/guess).

Time to improve on it.

### More Users and Persistent Storage

We'll be using a Sqlite database:

    (defparameter *db-path* (asdf:system-relative-pathname
                              :shovel-guess "state.db"))

with one table:

    (defun init-db ()
      (sqlite:with-open-database (db *db-path*)
        (sqlite:with-transaction db
          (sqlite:execute-non-query db "
      CREATE TABLE Sessions (Id INTEGER PRIMARY KEY,
                             LastAccessTime INTEGER,
                             VmState BINARY,
                             VmBytecode BINARY,
                             VmSources TEXT,
                             PageContent TEXT,
                             UserRead INT)"))))

    (init-db)

Fields description:

 * the session's ID is stored in field `Id`; every request without a
   session ID generates a new session;
 * we store a `LastAccessTime` in seconds since the CL Epoch so we can
   delete sessions which were not used for a long time;
 * we store the bytecode and the state for a sleeping VM in `VmState`
   and `VmBytecode`; we store the bytecode to make it easy to change
   our Shovel program without breaking older serialized VMs (which
   have state matching older versions of the program); to get correct
   error messages in case of multiple versions of the program, we also
   store the sources in `VmSources`;
 * because we store the page contents outside the VM, we need to also
   remember the current page contents in `PageContent`;
 * `UserRead` serves the same purpose as `*user-read*` in the previous
   section (we convert the keyword content of `*user-read*` to
   integers - see `save-session` below).

Copy the definitions for `*db-path*` and `init-db` to the
`shovel-guess.lisp`.

We no longer need the variables `*page-content*`, `*user-read*` and
`*vm-state*` - delete them from `shovel-guess.lisp`.

Define a struct to hold the session data:

    (defstruct session
      id last-access-time
      vm-state vm-bytecode vm-sources
      page-content user-read)

Also define a variable to hold the current session:

    (defvar *session* nil)

We need to change `web-print` and `web-print-ln` to use `*session*`
instead of the old variables:

    (defun web-print (content)
      (setf (session-page-content *session*)
            (with-output-to-string (str)
              (write-string (session-page-content *session*) str)
              (write-string "<span>" str)
              (write-string (hunchentoot:escape-for-html content) str)
              (write-string "</span>" str))))

    (defun web-print-ln (content)
      (setf (session-page-content *session*)
            (with-output-to-string (str)
              (write-string (session-page-content *session*) str)
              (write-string "<span>" str)
              (write-string (hunchentoot:escape-for-html content) str)
              (write-string "</span><br/>" str))))

Similar changes for `web-read-char` and `web-read-int`:

    (defun web-read-char ()
      (case (session-user-read *session*)
        ((nil)
         (setf (session-user-read *session*) :char)
         (values :null :nap-and-retry-on-wake-up))
        (:char
         (setf (session-user-read *session*) nil)
         (string (elt (hunchentoot:get-parameter "input") 0)))))

    (defun web-read-int ()
      (case (session-user-read *session*)
        ((nil)
         (setf (session-user-read *session*) :int)
         (values :null :nap-and-retry-on-wake-up))
        (:int
         (setf (session-user-read *session*) nil)
         (nth-value 0 (parse-integer (hunchentoot:get-parameter "input"))))))

Note that the signatures for the four functions above haven't changed,
and we can leave variable `*udps*` alone. We'll pass the current
session to these function by binding `*session*` (we can do this
because special variable bindings are thread local in SBCL and CCL -
this is a theoretically non-portable technique as the CL specification
doesn't say much about threads, but in practice every CL
implementation supporting threads does the same thing with special
variable bindings).

The new easy handler:

    (hunchentoot:define-easy-handler (guess :uri "/guess") ()
      (setf (hunchentoot:content-type*) "text/html")
      (alexandria:if-let (session-id (hunchentoot:get-parameter "sessionid"))
        (use-session session-id)
        (start-new-session)))

The HTML form will have an extra hidden field holding the ID of the
current session. The handler above checks if there is such a parameter
and tries to use it.

    (defun start-new-session ()
      (let ((*session* (make-session :id nil
                                     :last-access-time nil
                                     :vm-state nil
                                     :vm-bytecode (shovel:get-bytecode *sources*)
                                     :vm-sources (first *sources*)
                                     :page-content ""
                                     :user-read nil)))
        (run-session)))

If there is no session, `start-new-session` creates one.

    (defun run-session ()
      (multiple-value-bind (result vm)
          (shovel:run-vm (session-vm-bytecode *session*)
                         :sources (make-sources (session-vm-sources *session*))
                         :user-primitives *udps*
                         :state (session-vm-state *session*))
        (declare (ignore result))
        (cond ((shovel:vm-execution-complete vm)
               (start-new-session))
              (t
               (setf (session-vm-state *session*) (shovel:serialize-vm-state vm))
               (save-session)
               (generate-page)))))
               
A small helper function used by `run-session`:

    (defun make-sources (program)
      (list (shovel:make-source-file :name "your-program"
                                     :contents program)))               

`run-guess-number-vm` is replaced by `run-session` (you can delete the
definition of `run-guess-number-vm`).

    (defun save-session ()
      (within-sqlite-transaction
        (let* ((id (or (session-id *session*)
                       (sqlite:execute-single db "SELECT MAX(Id) + 1 FROM Sessions")))
               (insert (not (session-id *session*)))
               (bytecode (coerce (shovel:serialize-bytecode (session-vm-bytecode *session*))
                                 '(simple-array (unsigned-byte 8) (*))))
               (state (coerce (session-vm-state *session*)
                              '(simple-array (unsigned-byte 8) (*))))
               (user-read-code (ecase (session-user-read *session*)
                                 ((nil) 0)
                                 (:int 1)
                                 (:char 2))))
          (unless (session-id *session*)
            (setf (session-id *session*) id))
          (cond (insert
                 (sqlite:execute-non-query db "INSERT INTO Sessions (Id, LastAccessTime,
    VmState, VmBytecode, VmSources, PageContent, UserRead) VALUES (?, ?, ?, ?, ?, ?, ?)"
                                           id
                                           (get-universal-time)
                                           state
                                           bytecode
                                           (session-vm-sources *session*)
                                           (session-page-content *session*)
                                           user-read-code))
                (t (sqlite:execute-non-query db "UPDATE Sessions SET
    LastAccessTime = ?,
    VmState = ?,
    VmBytecode = ?,
    VmSources = ?,
    PageContent = ?,
    UserRead = ?
    WHERE Id = ?
    "
                                             (get-universal-time)
                                             state
                                             bytecode
                                             (session-vm-sources *session*)
                                             (session-page-content *session*)
                                             user-read-code
                                             id))))))

The Sqlite DB access functions are rather large. `save-session` saves
the current session to a database. It massages the session a bit to
make it acceptable for the Sqlite CL binding used (convert the byte
arrays to simple arrays, convert the `user-read` field to a number).

Since Sqlite apparently has issues with concurent accesses, a
[Bordeaux threads](http://common-lisp.net/project/bordeaux-threads/)
lock is used to serialize all accesses to the DB:

    (defvar *sqlite-lock* (bt:make-lock "sqlite-lock"))

A macro helps hide some details:

    (defmacro within-sqlite-transaction (&body body)
      `(bt:with-lock-held (*sqlite-lock*)
         (sqlite:with-open-database (db *db-path*)
           (sqlite:with-transaction db
             ,@body))))

If there is a session ID among the GET parameters, we use it:

    (defun use-session (session-id)
      (let ((*session* (load-session session-id)))
        (cond (*session*
               (alexandria:when-let (input (hunchentoot:get-parameter "input"))
                 (setf (session-page-content *session*)
                       (concatenate 'string
                                    (session-page-content *session*)
                                    input
                                    "<br/>")))
               (run-session))
              (t (start-new-session)))))

If for some reason the session is not found (`load-session` returns
`nil`), we start a new session.

    (defun load-session (session-id)
      (within-sqlite-transaction
        (let ((session-items
               (first (sqlite:execute-to-list db "SELECT Id, LastAccessTime, VmState,
    VmBytecode, VmSources, PageContent, UserRead FROM Sessions WHERE Id = ?" session-id))))
          (when session-items
            (make-session :id (elt session-items 0)
                          :last-access-time (elt session-items 1)
                          :vm-state (elt session-items 2)
                          :vm-bytecode (shovel:deserialize-bytecode (elt session-items 3))
                          :vm-sources (elt session-items 4)
                          :page-content (elt session-items 5)
                          :user-read (ecase (elt session-items 6)
                                       ((0) nil)
                                       ((1) :int)
                                       ((2) :char)))))))

Finally, `generate-page` is now:

    (defun generate-page ()
      (with-output-to-string (str)
        (write-string (session-page-content *session*) str)
        (write-string "<form action='/guess' method='get'>" str)
        (write-string "<input type='text' name='input' id='shovel-input'/>" str)
        (format str "<input type='hidden' name='sessionid' value='~d' id='shovel-input'/>"
                (session-id *session*))
        (write-string "<input type='submit' value='Submit'/>" str)
        (write-string "</form>" str)
        (write-string
          "<script type='text/javascript'>
          document.getElementById('shovel-input').focus()</script>"
          str)))

Note the extra `input` element used to hold the session ID.

You can now [try the game](http://localhost:4242/guess) again. Start
it in two browser windows and notice that they don't share state.

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
important, we can now cheat the game: guess the number, then use the
back button as much as we like (maybe up to the first guess?) and then
enter the secret number directly.

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

Implementing branching sessions is easy: just change the
`save-session` function to always allocate a new ID to the saved
session, and always use SQL `INSERT`. Oddly, this bug fix makes the
code shorter:

    (defun save-session ()
      (within-sqlite-transaction
        (let* ((id (sqlite:execute-single db "SELECT MAX(Id) + 1 FROM Sessions"))
               (bytecode (coerce (shovel:serialize-bytecode (session-vm-bytecode *session*))
                                 '(simple-array (unsigned-byte 8) (*))))
               (state (coerce (session-vm-state *session*)
                              '(simple-array (unsigned-byte 8) (*))))
               (user-read-code (ecase (session-user-read *session*)
                                 ((nil) 0)
                                 (:int 1)
                                 (:char 2))))
          (setf (session-id *session*) id)
          (sqlite:execute-non-query db "INSERT INTO Sessions (Id, LastAccessTime,
    VmState, VmBytecode, VmSources, PageContent, UserRead) VALUES (?, ?, ?, ?, ?, ?, ?)"
                                    id
                                    (get-universal-time)
                                    state
                                    bytecode
                                    (session-vm-sources *session*)
                                    (session-page-content *session*)
                                    user-read-code))))

(change `save-session` in your code or switch to Git branch
`back-button` if you're using the code from Github).

[Try it](http://localhost:4242/guess)! See if you can guess the secret
number in one attempt!

A short aside about storage: our storage arrangements are a bit
wasteful - for each session we store not only the VM state, but also
the bytecode and source code. Usually the number of processes
(represented by VM states) is much, much larger than the number of
programs (represented by bytecode and source code). It makes sense to
store programs in a separate table and store pointers to that table in
the 'Sessions' table (such as the MD5 hashes of the bytecode and
source). This improvement is left as an exercise to the reader.

Updating the `LastAccessTime` when a session is loaded and deleting
sessions older than, say, a few hours, is also left as an exercise to
the reader. Since we have versioned sessions, maybe it's an
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
   
We can start by deleting `*sources*` from `shovel-guess.lisp`.

Next, other easy changes (switch to branch `turn-tables` if you're
using the Github code for `shovel-guess`). Update `*udps*`:

    (defparameter *udps* (list (list "print" 'web-print 1)
                               (list "printLn" 'web-print-ln 1)
                               (list "readChar" 'web-read-char 0)
                               (list "readInt" 'web-read-int 0)
                               (list "readLine" 'web-read-line 0)
                               (list "random" (lambda () (random 1.0d0)) 0)))

`web-read-line` is:

    (defun web-read-line ()
      (case (session-user-read *session*)
        ((nil)
         (setf (session-user-read *session*) :line)
         (values :null :nap-and-retry-on-wake-up))
        (:line
         (setf (session-user-read *session*) nil)
         (hunchentoot:get-parameter "input"))))

We also need to change `save-session` and `load-session` to accept
`:line` as a valid type for a `session-user-read` values: add `(:line
3)` in the `ecase` expression in `save-session` and `((3) :line)` in
the ecase expression in `load-session`.

We'll name the easy handler differently:

    (hunchentoot:define-easy-handler (whatever :uri "/whatever") ()
      (setf (hunchentoot:content-type*) "text/html")
      (alexandria:if-let (session-id (hunchentoot:get-parameter "sessionid"))
        (use-session session-id)
        (start-new-session)))

(feel free to come up with a better name)

Change `start-new-session`:

    (defun start-new-session ()
      (alexandria:if-let (program (hunchentoot:post-parameter "program"))
        (multiple-value-bind (result error)
            (ignore-errors
              (shovel:get-bytecode (make-sources program)))
          (if result
              (create-and-run-session result program)
              (concatenate 'string
                           "<pre>"
                           (hunchentoot:escape-for-html
                            (with-output-to-string (str)
                              (print-object error str)))
                           "</pre>")))
        (with-output-to-string (str)
          (write-string "<form action='/whatever' method='post'>" str)
          (write-string "<textarea name='program'
    id='shovel-input' rows='30' cols='80'></textarea><br/>" str)
          (write-string "<input type='submit' value='Submit'/>" str)
          (write-string "</form>" str)
          (write-string "<script type='text/javascript'>
    document.getElementById('shovel-input').focus()</script>"
                        str))))

(the buttons which fill the `textarea` with predefined programs are
left as an exercise to the reader - we'll just copy-paste the programs
for now)

`create-and-run-session` is similar to the old `start-new-session`,
but it receives the bytecode and the sources for the VM as parameters:

    (defun create-and-run-session (bytecode program)
      (let ((*session* (make-session :id nil
                                     :last-access-time nil
                                     :vm-state nil
                                     :vm-bytecode bytecode
                                     :vm-sources program
                                     :page-content ""
                                     :user-read nil)))
        (run-session)))

We have to change `run-session` to display the ShovelScript run-time
errors:

    (defun run-session ()
      (handler-case
          (multiple-value-bind (result vm)
              (shovel:run-vm (session-vm-bytecode *session*)
                             :sources (make-sources (session-vm-sources *session*))
                             :user-primitives *udps*
                             :state (session-vm-state *session*))
            (declare (ignore result))
            (cond ((shovel:vm-execution-complete vm)
                   (concatenate 'string
                                (session-page-content *session*)
                                "<p>Program completed.</p>"))
                  (t
                   (setf (session-vm-state *session*) (shovel:serialize-vm-state vm))
                   (save-session)
                   (generate-page))))
        (shovel:shovel-error (err)
          (concatenate 'string
                       "<pre>"
                       (hunchentoot:escape-for-html
                        (with-output-to-string (str)
                          (print-object err str)))
                       "</pre>"))))

The function was also changed to just print a message when the program
completes (if the program doesn't end by calling a `read*` UDP, then
the final part of the program's execution and/or messages will be
missed because a fresh session is started). Adding a link to the
previous page and a link to start another session if there's an error
is left as an exercise to the reader.

Since we changed the name of the easy handler, `generate-page` also
needs a little care (rename `guess` to `whatever` in the form's `method`
attribute):

    (defun generate-page ()
      (with-output-to-string (str)
        (write-string (session-page-content *session*) str)
        (write-string "<form action='/whatever' method='get'>" str)
        (write-string "<input type='text' name='input' id='shovel-input'/>" str)
        (format str "<input type='hidden' name='sessionid' value='~d' id='shovel-input'/>"
                (session-id *session*))
        (write-string "<input type='submit' value='Submit'/>" str)
        (write-string "</form>" str)
        (write-string "<script type='text/javascript'>
    document.getElementById('shovel-input').focus()</script>"
                      str)))

Let's [try it out](http://localhost:4242/whatever) with the following program:

    @print('What is your name? ')
    var name = @readLine()
    @printLn('Hello, ' + name)
    
It works! (try the Github code, branch `turn-tables`, if it doesn't
work for you - either I messed up when copying code into this document
or you missed/mistyped/mispasted something)

What about this slightly broken program?

    @print('What is your name? ')
    var name = @readLine()
    name + 1
    
After you enter your name (let's assume it's "John"), you will get an
error message:

    Shovel error in file 'your-program' at line 3, column 1: 
    Arguments must have the same type (numbers or strings or arrays).

    Current stack trace:
    file 'your-program' line 3: name + 1
    file 'your-program' line 3: ^^^^^^^^

    Current environment:

    Frame starts at:
    file 'your-program' line 2: var name = @readLine()
    file 'your-program' line 2: ^^^^^^^^^^^^^^^^^^^^^^
    Frame variables are:
    name = "John"

Try it again, with the number guessing game:

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

Fun, isn't it?

We just exposed our server to the whims of remote users. What happens
if someone writes (by mistake or malice) a program with an infinite
loop or a program that allocates a lot of memory (or both)? We should
teach our programs to cope with such problems.

### Adding CPU and RAM Quotas

CPU and RAM quotas are described in
[ShovelVmSpec.md](ShovelVmSpec.md), section 'Quotas'. We can specify
that a VM won't use more than 100000 cells (that should be under a
megabyte) and not more than 1000000 ticks total (most probably less
than a second total run-time - the time spent waiting for input from
the user is not counted). The new `run-session`:

    (defun run-session ()
      (handler-case
          (multiple-value-bind (result vm)
              (shovel:run-vm (session-vm-bytecode *session*)
                             :sources (make-sources (session-vm-sources *session*))
                             :user-primitives *udps*
                             :state (session-vm-state *session*)
                             :cells-quota (expt 10 5)
                             :total-ticks-quota (expt 10 6))
            (declare (ignore result))
            (cond ((shovel:vm-execution-complete vm)
                   (concatenate 'string
                                (session-page-content *session*)
                                "<p>Program completed.</p>"))
                  (t
                   (setf (session-vm-state *session*) (shovel:serialize-vm-state vm))
                   (save-session)
                   (generate-page))))
        (shovel:shovel-error (err)
          (concatenate 'string
                       "<pre>"
                       (hunchentoot:escape-for-html
                        (with-output-to-string (str)
                          (print-object err str)))
                       "</pre>"))))

The `:cells-quota` and `:total-ticks-quota` parameters were added to
the `shovel:run-vm` call.

To make the server more resilient, we could limit the number of
threads serving requests and we could use another quota,
`:ticks-until-next-nap n` (the VM is put to sleep after `n` ticks and
another VM can be resumed - but we have to write the scheduler, all
Shovel does is return from `shovel:run-vm` when this quota is
exceeded). Both these changes are beyond the scope of this document.

That's all, folks!

