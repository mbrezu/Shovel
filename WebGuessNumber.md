-*- markdown -*-

# A Web Guessing Number with Shovel and Common Lisp

## Introduction

This document is written with the assumption that the reader is
familiar with
[the Shovel Common Lisp getting started guide](ClGettingStarted.md).

It covers an extended example of using interruptible Shovel VM
programs.

## Local Game

First, a local, non-interruptible version of the number guessing game.

The computer picks a number between 1 and 100 and asks the user to
guess what number it picked. For each user guess, it replies with 'too
small', 'too large' or 'you guessed it'. When the user guesses the
number, he is congratulated and offered another game.

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
      var iteration = fn () {
        @print('Enter a number between 1 and 100: ')
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
          @printLn('You guessed it! Congratulations!')
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
              (write-string "<pre>" str)
              (write-string (hunchentoot:escape-for-html content) str)
              (write-string "</pre>" str))))

    (defun web-print-ln (content)
      (setf *page-content*
            (with-output-to-string (str)
              (write-string *page-content* str)
              (write-string "<pre>" str)
              (write-string (hunchentoot:escape-for-html content) str)
              (write-string "</pre><br/>" str))))

A global variable to remember what to ask the user:

    (defvar *user-read* nil)

(`NIL` means we don't want anything from the user, `:char` means we
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
      
... and its helper functions:

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
        (write-string "<script type='text/javascript'>document.getElementById('shovel-input').focus()</script>"
                      str)))

Go to [`http://localhost:4242/guess`](http://localhost:4242/guess) and play the game.

The easy handler adds the `input` GET parameter to the content for
future pages, then calls `run-guess-number-vm`.

`run-guess-number-vm` runs the VM with the saved state (`NIL` if we
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

[TBD]

## Turning the Tables: the User Writes the Shovel Program

[TBD]

