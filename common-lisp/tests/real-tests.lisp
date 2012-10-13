
(in-package #:shovel-tests)

(def-suite :shovel-tests)

(in-suite :shovel-tests)

(test test-constants
  (is (= 1 (shovel:naked-run-code (list "1"))))
  (is (string= "a" (shovel:naked-run-code (list "'a'"))))
  (is (= 1.23 (shovel:naked-run-code (list "1.23")))))

(test some-operators
  (is (= 2 (shovel:naked-run-code (list "1 + 1")))))

(test complex
  (is (equalp #(1 2 3 3 4 5)
              (shovel:naked-run-code
               (list (shovel:stdlib)
                     "stdlib.sort(array(3, 1, 2, 5, 4, 3), fn (a, b) a < b)")))))

(test tokenizer-error-message
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var g = fn x x + 2
var f = fn x g(x) + 2
f('1")))))
               "Shovel error in file 'test.shr' at end of file: Expected an end quote, but reached the end of file.

"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var g = fn #x x + 2
var f = fn x g(x) + 2
f('1')")))))
               "Shovel error in file 'test.shr' at line 2, column 12: Unexpected character '#'.
file 'test.shr' line 2: var g = fn #x x + 2
file 'test.shr' line 2:            ^

")))

(test parser-error-message
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
b(]
")))))
               "Shovel error in file 'test.shr' at line 2, column 3: Unexpected token ']'.
file 'test.shr' line 2: b(]
file 'test.shr' line 2:   ^

"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var a = fn [x] 1
")))))
               "Shovel error in file 'test.shr' at line 2, column 12: Expected a identifier, but got '['.
file 'test.shr' line 2: var a = fn [x] 1
file 'test.shr' line 2:            ^

"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var fn = 1
")))))
               "Shovel error in file 'test.shr' at line 2, column 5: Expected a identifier, but got 'fn'.
file 'test.shr' line 2: var fn = 1
file 'test.shr' line 2:     ^^

"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var slice = 1
")))))
               "Shovel error in file 'test.shr' at line 2, column 5: Name 'slice' is reserved for a primitive.
file 'test.shr' line 2: var slice = 1
file 'test.shr' line 2:     ^^^^^

")))

(test code-generator-error-message
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var a = 1
var a = 2")))))
               "Shovel error in file 'test.shr' at line 3, column 5: Variable 'a' is already defined in this frame in file '\"test.shr\"', at line 2, column 5.
file 'test.shr' line 3: var a = 2
file 'test.shr' line 3:     ^

"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
b = 3
")))))
               "Shovel error in file 'test.shr' at line 2, column 1: Undefined variable 'b'.
file 'test.shr' line 2: b = 3
file 'test.shr' line 2: ^^^^^

"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
b + 1
")))))
               "Shovel error in file 'test.shr' at line 2, column 1: Undefined variable 'b'.
file 'test.shr' line 2: b + 1
file 'test.shr' line 2: ^

")))

(test vm-error-message
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var g = fn x x + 2
var f = fn x g(x) + 2
f('1')")))))
               "Shovel error in file 'test.shr' at line 2, column 14: Arguments must have the same type (numbers or strings or arrays).

Current stack trace:
file 'test.shr' line 2: var g = fn x x + 2
file 'test.shr' line 2:              ^^^^^
file 'test.shr' line 3: var f = fn x g(x) + 2
file 'test.shr' line 3:              ^^^^
file 'test.shr' line 4: f('1')
file 'test.shr' line 4: ^^^^^^

Current environment:

Frame starts at:
file 'test.shr' line 2: var g = fn x x + 2
file 'test.shr' line 2:            ^
Frame variables are:
x = \"1\"

Frame starts at:
file 'stdlib.shr' line 2: var stdlib = { [...content snipped...]
file 'stdlib.shr' line 2: ^^^^^^^^^^^^^^
Frame variables are:
stdlib = hash(\"filter\", [...callable...], \"forEach\", [...callable...], \"forEachWithIndex\", [...callable...], \"forIndex\", [...callable...], \"map\", [...callable...], \"mapWithIndex\", [...callable...], \"max\", [...callable...], \"min\", [...callable...], \"reduceFromLeft\", [...callable...], \"reverse\", [...callable...], \"sort\", [...callable...], \"while\", [...callable...])
g = [...callable...]
f = [...callable...]



")))

(test code-printer
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:print-code (list "var a = 1
var b = 2
var c = fn (x, y) x + y"))))
               "    VM-VERSION 1

    VM-SOURCES-MD5 352CC9384F33B7778F01A32F66438B39

    VM-BYTECODE-MD5 ?

    FILE-NAME <unspecified-1>

    ; file '<unspecified-1>' line 1: var a = 1 [...content snipped...]
    ; file '<unspecified-1>' line 1: ^^^^^^^^^
    NEW-FRAME a, b, c

    ; file '<unspecified-1>' line 1: var a = 1
    ; file '<unspecified-1>' line 1:         ^
    CONST 1

    ; file '<unspecified-1>' line 1: var a = 1
    ; file '<unspecified-1>' line 1: ^^^^^^^^^
    LSET 0, 0

    POP

    ; file '<unspecified-1>' line 2: var b = 2
    ; file '<unspecified-1>' line 2:         ^
    CONST 2

    ; file '<unspecified-1>' line 2: var b = 2
    ; file '<unspecified-1>' line 2: ^^^^^^^^^
    LSET 0, 1

    POP

    JUMP L2

    ; file '<unspecified-1>' line 3: var c = fn (x, y) x + y
    ; file '<unspecified-1>' line 3:         ^^^^^^^^^^^^^^^
FN1:

    ; file '<unspecified-1>' line 3: var c = fn (x, y) x + y
    ; file '<unspecified-1>' line 3:             ^^^^
    NEW-FRAME x, y

    ARGS 2

    ; file '<unspecified-1>' line 3: var c = fn (x, y) x + y
    ; file '<unspecified-1>' line 3:                   ^
    LGET 0, 0

    ; file '<unspecified-1>' line 3: var c = fn (x, y) x + y
    ; file '<unspecified-1>' line 3:                       ^
    LGET 0, 1

    ; file '<unspecified-1>' line 3: var c = fn (x, y) x + y
    ; file '<unspecified-1>' line 3:                     ^
    PRIM0 +

    ; file '<unspecified-1>' line 3: var c = fn (x, y) x + y
    ; file '<unspecified-1>' line 3:                   ^^^^^
    CALLJ 2

L2:
    FN FN1, 2

    ; file '<unspecified-1>' line 3: var c = fn (x, y) x + y
    ; file '<unspecified-1>' line 3: ^^^^^^^^^^^^^^^^^^^^^^^
    LSET 0, 2

    DROP-FRAME

NIL
")))

(test bytecode-serializer
  (let* ((instructions (shovel:get-bytecode (list (shovel:stdlib))))
         (serialized-instructions (shovel:serialize-bytecode instructions))
         (deserialized-instructions (shovel:deserialize-bytecode
                                     serialized-instructions)))
    (is (equalp instructions deserialized-instructions))))

(defun test-serializer (program user-primitives)
  (let* ((sources (list (shovel:stdlib)
                        program))
         (bytecode (shovel:get-bytecode sources)))
    (multiple-value-bind (first-run vm)
        (shovel-vm:run-vm
         bytecode
         :sources sources
         :user-primitives user-primitives)
      (declare (ignore first-run))
      (let* ((bytes (shovel-vm:serialize-vm-state vm))
             (second-run (shovel-vm:run-vm
                          bytecode
                          :sources sources
                          :user-primitives user-primitives
                          :state bytes)))
        second-run))))

(test vm-serializer-1
  (let (flag)
    (labels ((halt ()
               (cond ((not flag)
                      (setf flag t)
                      (values :null :nap-and-retry-on-wake-up))
                     (t "It works!"))))
      (let* ((my-program "@halt()")
             (user-primitives (list (list "halt" #'halt 0))))
        (is (string= (test-serializer my-program user-primitives) "It works!"))))))

(test vm-serializer-2
  (let (flag)
    (labels ((halt (a b c)
               (cond ((not flag)
                      (setf flag t)
                      (values :null :nap))
                     (t (+ a b c)))))
      (let* ((my-program "
var a = 1
var b = 2
var c = 3
var d = fn () {
  @halt(a, b, c)
}
d()
c = 4
@halt(a, b, c)
")
             (user-primitives (list (list "halt" #'halt 3))))
        (is (= (test-serializer my-program user-primitives) 7))))))

(test vm-serializer-3
  (let (flag)
    (labels ((halt (name age)
               (cond ((not flag)
                      (setf flag t)
                      (values :null :nap-and-retry-on-wake-up))
                     (t (format nil "~a is ~d years old." name age)))))
      (let* ((my-program "
var person = fn (name, age) {
  var this = hash('name', name, 'age', age)
  this.print = fn () { @halt(this.name, this.age) }
  this
}
var makeFamily = fn () {
  var names = array('John', 'Jane', 'Andrew')
  var ages = array(22, 23, 2)
  var makePerson = fn (x) {
    person(names[x], ages[x]).print()
  }
  array(makePerson(0), makePerson(1), makePerson(2))
}
var family = makeFamily()
")
             (user-primitives (list (list "halt" #'halt 2))))
        (is (equalp (test-serializer my-program user-primitives)
                    #("John is 22 years old."
                      "Jane is 23 years old."
                      "Andrew is 2 years old.")))))))

(test array-boundaries
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var a = arrayN(10)
a[10] = 1
")))))
               "Shovel error in file 'test.shr' at line 3, column 1: Invalid 0-based index (10 for an array with 10 elements).

Current stack trace:
file 'test.shr' line 3: a[10] = 1
file 'test.shr' line 3: ^^^^^^^^^

Current environment:

Frame starts at:
file 'stdlib.shr' line 2: var stdlib = { [...content snipped...]
file 'stdlib.shr' line 2: ^^^^^^^^^^^^^^
Frame variables are:
stdlib = hash(\"filter\", [...callable...], \"forEach\", [...callable...], \"forEachWithIndex\", [...callable...], \"forIndex\", [...callable...], \"map\", [...callable...], \"mapWithIndex\", [...callable...], \"max\", [...callable...], \"min\", [...callable...], \"reduceFromLeft\", [...callable...], \"reverse\", [...callable...], \"sort\", [...callable...], \"while\", [...callable...])
a = array(null, null, null, null, null, null, null, null, null, null)



"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var a = arrayN(10)
a[-1] = 1
")))))
               "Shovel error in file 'test.shr' at line 3, column 1: Index less than 0.

Current stack trace:
file 'test.shr' line 3: a[-1] = 1
file 'test.shr' line 3: ^^^^^^^^^

Current environment:

Frame starts at:
file 'stdlib.shr' line 2: var stdlib = { [...content snipped...]
file 'stdlib.shr' line 2: ^^^^^^^^^^^^^^
Frame variables are:
stdlib = hash(\"filter\", [...callable...], \"forEach\", [...callable...], \"forEachWithIndex\", [...callable...], \"forIndex\", [...callable...], \"map\", [...callable...], \"mapWithIndex\", [...callable...], \"max\", [...callable...], \"min\", [...callable...], \"reduceFromLeft\", [...callable...], \"reverse\", [...callable...], \"sort\", [...callable...], \"while\", [...callable...])
a = array(null, null, null, null, null, null, null, null, null, null)



")))

(test array-push
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var a = array()
push(a, 1)
push(a, 2)
push(a, 3)
a
")))))
               "#(1 2 3)
"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var a = 1
push(a, 1)
")))))
               "Shovel error in file 'test.shr' at line 3, column 1: First argument must be a vector.

Current stack trace:
file 'test.shr' line 3: push(a, 1)
file 'test.shr' line 3: ^^^^^^^^^^

Current environment:

Frame starts at:
file 'stdlib.shr' line 2: var stdlib = {
file 'stdlib.shr' line 2: ^^^^^^^^^
Frame variables are:
stdlib = hash(\"filter\", [...callable...], \"forEach\", [...callable...], \"forEachWithIndex\", [...callable...], \"forIndex\", [...callable...], \"map\", [...callable...], \"mapWithIndex\", [...callable...], \"max\", [...callable...], \"min\", [...callable...], \"reduceFromLeft\", [...callable...], \"reverse\", [...callable...], \"sort\", [...callable...], \"while\", [...callable...])
a = 1



")))

(test array-pop
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var a = array(1, 2, 3)
pop(a)
")))))
               "3
"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var a = array(1, 2, 3)
pop(a)
pop(a)
")))))
               "2
"))

  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code
                    (list (shovel:stdlib)
                          (shovel-types:make-shript-file :name "test.shr"
                                                         :contents "
var a = array()
pop(a)
")))))
               "Shovel error in file 'test.shr' at line 3, column 1: Can't pop from an empty array.

Current stack trace:
file 'test.shr' line 3: pop(a)
file 'test.shr' line 3: ^^^^^^

Current environment:

Frame starts at:
file 'stdlib.shr' line 2: var stdlib = { [...content snipped...]
file 'stdlib.shr' line 2: ^^^^^^^^^^^^^^
Frame variables are:
stdlib = hash(\"filter\", [...callable...], \"forEach\", [...callable...], \"forEachWithIndex\", [...callable...], \"forIndex\", [...callable...], \"map\", [...callable...], \"mapWithIndex\", [...callable...], \"max\", [...callable...], \"min\", [...callable...], \"reduceFromLeft\", [...callable...], \"reverse\", [...callable...], \"sort\", [...callable...], \"while\", [...callable...])
a = array()



")))

(test vm-serialization-and-push-pop-arrays
  (let (flag)
    (labels ((halt (a)
               (cond ((not flag)
                      (setf flag t)
                      (values :null :nap))
                     (t a))))
      (let* ((my-program "
var a = array(10, 20, 30)
@halt(a)
push(a, 40)
pop(a)
pop(a)
@halt(a)
")
             (user-primitives (list (list "halt" #'halt 1))))
        (is (equalp (test-serializer my-program user-primitives)
                    #(10 20)))))))

(test comparison
  (is (= 1 (shovel:naked-run-code (list "var a = true if a == true 1 else 2"))))
  (is (= 2 (shovel:naked-run-code (list "var a = false if a == true 1 else 2"))))
  (is (= 1 (shovel:naked-run-code (list "var a = null if a == null 1 else 2"))))
  (is (= 2 (shovel:naked-run-code (list "var a = null if a != null 1 else 2"))))
  (is (= 2 (shovel:naked-run-code (list "var a = 3 if a == null 1 else 2"))))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code (list "var a = 30 if a == true 1 else 2"))))
               "Shovel error in file '<unspecified-1>' at line 1, column 15: At least one of the arguments must be null or they must have the same type (numbers, strings, booleans, hashes or arrays).

Current stack trace:
file '<unspecified-1>' line 1: var a = 30 if a == true 1 else 2
file '<unspecified-1>' line 1:               ^^^^^^^^^

Current environment:

Frame starts at:
file '<unspecified-1>' line 1: var a = 30 if a == true 1 else 2
file '<unspecified-1>' line 1: ^^^^^^^^^^
Frame variables are:
a = 30



"))
  (is (= 1 (shovel:naked-run-code (list "
var a = array(1)
var b = a
if a == b 1 else 2"))))
  (is (= 2 (shovel:naked-run-code (list "
var a = array(1)
var b = array(1)
if a == b 1 else 2"))))
  (is (= 1 (shovel:naked-run-code (list "
var a = hash('a', 1)
var b = a
if a == b 1 else 2"))))
  (is (= 2 (shovel:naked-run-code (list "
var a = hash('a', 1)
var b = hash('a', 1)
if a == b 1 else 2")))))

(test string-upper-lower
  (is (string= "john" (shovel:naked-run-code (list "lower('John')"))))
  (is (string= "JOHN" (shovel:naked-run-code (list "upper('John')")))))

(test string-accesses-are-strings
  (is (string= "tt" (shovel:naked-run-code (list "var a = 'test' a[0] + a[3]"))))
  (is (string= "tt" (shovel:naked-run-code
                     (list "var a = 'test' slice(a, 0, 1) + slice(a, 3, 4)"))))
  (is (= 5 (shovel:naked-run-code
            (list "var a = array(1, 2, 3, 4) a[0] + a[3]")))))

(test assignment-of-one-character-strings-to-string-accesses
  (is (string= "text" (shovel:naked-run-code (list "
var a = 'test'
a[2] = 'x'
a"))))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code (list "
var a = 'test'
a[2] = 'x1'
a"))))
               "Shovel error in file '<unspecified-1>' at line 3, column 1: Must provide a one character string as right hand side for the assignment.

Current stack trace:
file '<unspecified-1>' line 3: a[2] = 'x1'
file '<unspecified-1>' line 3: ^^^^^^^^^^^

Current environment:

Frame starts at:
file '<unspecified-1>' line 2: var a = 'test'
file '<unspecified-1>' line 2: ^^^^^^^^^^^^^^
Frame variables are:
a = \"test\"



")))

(test broken-vm-state-match
  (let (flag)
    (labels ((halt (a)
               (cond ((not flag)
                      (setf flag t)
                      (values :null :nap-and-retry-on-wake-up))
                     (t a))))
      (let* ((my-program-1 "var a = 10 @halt(a)")
             (my-program-2 "var b = 10 @halt(b)")
             (user-primitives (list (list "halt" #'halt 1))))
        (let* ((sources-1 (list (shovel:stdlib) my-program-1))
               (sources-2 (list (shovel:stdlib) my-program-2))
               (bytecode-1 (shovel:get-bytecode sources-1))
               (bytecode-2 (shovel:get-bytecode sources-2)))
          (multiple-value-bind (first-run vm)
              (shovel-vm:run-vm
               bytecode-1
               :sources sources-1
               :user-primitives user-primitives)
            (declare (ignore first-run))
            (multiple-value-bind (value error)
                (ignore-errors
                  (let* ((bytes (shovel-vm:serialize-vm-state vm))
                         (second-run (shovel-vm:run-vm
                                      bytecode-2
                                      :sources sources-2
                                      :user-primitives user-primitives
                                      :state bytes)))
                    second-run))
              (declare (ignore value))
              (is (eql 'shovel-types:shovel-vm-match-error
                       (type-of error)))
              (is (string=
                   "VM bytecode MD5 and serialized VM bytecode MD5 do not match."
                   (shovel-types:error-message error))))))))))

(defun mess-with (bytes)
  (if (= 0 (aref bytes 10))
      (setf (aref bytes 10) 1)
      (setf (aref bytes 10) 0)))

(test bytecode-serializer-checksum-failure
  (let* ((instructions (shovel:get-bytecode (list (shovel:stdlib))))
         (serialized-instructions (shovel:serialize-bytecode instructions)))
    (mess-with serialized-instructions)
    (signals shovel-types:shovel-broken-checksum
      (shovel:deserialize-bytecode serialized-instructions))))

(test bytecode-serializer-version-failure
  (let* ((instructions (shovel:get-bytecode (list (shovel:stdlib))))
         (serialized-instructions
          (let ((shovel-vm:*version* (1+ shovel-vm:*version*)))
            (shovel:serialize-bytecode instructions))))
    (signals shovel-types:shovel-version-too-large
      (shovel:deserialize-bytecode serialized-instructions))))

(test vm-state-serializer-checksum-failure
  (let (flag)
    (labels ((halt (a)
               (cond ((not flag)
                      (setf flag t)
                      (values :null :nap-and-retry-on-wake-up))
                     (t a))))
      (let* ((my-program "var a = 10 @halt(a)")
             (user-primitives (list (list "halt" #'halt 1))))
        (let* ((sources (list (shovel:stdlib) my-program))
               (bytecode (shovel:get-bytecode sources)))
          (multiple-value-bind (first-run vm)
              (shovel-vm:run-vm
               bytecode
               :sources sources
               :user-primitives user-primitives)
            (declare (ignore first-run))
            (let ((bytes (shovel-vm:serialize-vm-state vm)))
              (mess-with bytes)
              (signals shovel-types:shovel-broken-checksum
                (shovel-vm:deserialize-vm-state vm bytes)))))))))

(test vm-state-serializer-version-failure
  (let (flag)
    (labels ((halt (a)
               (cond ((not flag)
                      (setf flag t)
                      (values :null :nap-and-retry-on-wake-up))
                     (t a))))
      (let* ((my-program "var a = 10 @halt(a)")
             (user-primitives (list (list "halt" #'halt 1))))
        (let* ((sources (list (shovel:stdlib) my-program))
               (bytecode (shovel:get-bytecode sources)))
          (multiple-value-bind (first-run vm)
              (shovel-vm:run-vm
               bytecode
               :sources sources
               :user-primitives user-primitives)
            (declare (ignore first-run))
            (let ((bytes (let ((shovel-vm:*version* (1+ shovel-vm:*version*)))
                           (shovel-vm:serialize-vm-state vm))))
              (signals shovel-types:shovel-version-too-large
                (shovel-vm:deserialize-vm-state vm bytes)))))))))

(test non-local-exit-1
  (is (= (shovel:naked-run-code (list "
var h = fn x x + 2
var g = fn x h(x) + 2
var f = fn x block 'f' g(x) + 2
f(1)
"))
         7))
  (is (= 10 (shovel:naked-run-code (list "
var h = fn x return 'f' 10
var g = fn x h(x) + 2
var f = fn x block 'f' g(x) + 2
f(1)
"))))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code (list "
var h = fn x return 'g' 10
var g = fn x h(x) + 2
var f = fn x block 'f' g(x) + 2
f(1)
"))))
               "Shovel error in file '<unspecified-1>' at line 2, column 14: Cannot find block 'g'.

Current stack trace:
file '<unspecified-1>' line 2: var h = fn x return 'g' 10
file '<unspecified-1>' line 2:              ^^^^^^^^^^^^^
file '<unspecified-1>' line 3: var g = fn x h(x) + 2
file '<unspecified-1>' line 3:              ^^^^
file '<unspecified-1>' line 4: var f = fn x block 'f' g(x) + 2
file '<unspecified-1>' line 4:                        ^^^^
file '<unspecified-1>' line 5: f(1)
file '<unspecified-1>' line 5: ^^^^

Current environment:

Frame starts at:
file '<unspecified-1>' line 2: var h = fn x return 'g' 10
file '<unspecified-1>' line 2:            ^
Frame variables are:
x = 1

Frame starts at:
file '<unspecified-1>' line 2: var h = fn x return 'g' 10 [...content snipped...]
file '<unspecified-1>' line 2: ^^^^^^^^^^^^^^^^^^^^^^^^^^
Frame variables are:
h = [...callable...]
g = [...callable...]
f = [...callable...]



"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code (list "block 1 2"))))
               "Shovel error in file '<unspecified-1>' at line 1, column 1: The name of a block must be a string.

Current stack trace:
file '<unspecified-1>' line 1: block 1 2
file '<unspecified-1>' line 1: ^^^^^^^^^

Current environment:



"))
  (is (string= (with-output-to-string (str)
                 (let ((*standard-output* str))
                   (shovel:run-code (list "block 'a' return 1 3"))))
               "Shovel error in file '<unspecified-1>' at line 1, column 11: The name of a block must be a string.

Current stack trace:
file '<unspecified-1>' line 1: block 'a' return 1 3
file '<unspecified-1>' line 1:           ^^^^^^^^^^

Current environment:



")))

(test vm-serializer-and-non-local-exits
  (let (flag)
    (labels ((halt ()
               (cond ((not flag)
                      (setf flag t)
                      (values :null :nap-and-retry-on-wake-up))
                     (t "ole!"))))
      (let* ((my-program "block 'f' @halt()")
             (user-primitives (list (list "halt" #'halt 0))))
        (is (string= "ole!" (test-serializer my-program user-primitives)))))))

(defun run-tests ()
  (fiveam:run! :shovel-tests))
