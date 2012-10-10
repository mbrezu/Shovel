
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
               "Shovel error in file 'test.shr' at line 2, column 5: 'fn' is a reserved keyword.
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
x = \"1\"
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
               "    NEW-FRAME a, b, c
    FILE-NAME <unspecified-1>
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
  (let* ((instructions (shovel-compiler:assemble-instructions
                        (shovel-compiler:compile-sources-to-instructions
                         (list (shovel:stdlib)))))
         (serialized-instructions (shovel:serialize-bytecode instructions))
         (deserialized-instructions (shovel:deserialize-bytecode
                                     serialized-instructions)))
    (is (equalp instructions deserialized-instructions))))

(defun test-serializer (program user-primitives)
  (let* ((sources (list (shovel:stdlib)
                        program))
         (bytecode (shovel-compiler:assemble-instructions
                    (shovel-compiler:compile-sources-to-instructions
                     sources))))
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
                      (values nil :nap-and-retry-on-wake-up))
                     (t "It works!"))))
      (let* ((my-program "@halt()")
             (user-primitives (list (list "halt" #'halt 0))))
        (is (string= (test-serializer my-program user-primitives) "It works!"))))))

(test vm-serializer-2
  (let (flag)
    (labels ((halt (a b c)
               (cond ((not flag)
                      (setf flag t)
                      (values nil :nap))
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
                      (values nil :nap-and-retry-on-wake-up))
                     (t (format nil "~a is ~d years old." name age)))))
      (let* ((my-program "
var person = fn (name, age) {
  var this = hash('name', name, 'age', age)
  this.print = fn () { @halt(this.name, this.age) }
  return this
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

(test array-margins
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
stdlib = hash(\"filter\", [...callable...], \"forEach\", [...callable...], \"forEachWithIndex\", [...callable...], \"forIndex\", [...callable...], \"map\", [...callable...], \"mapWithIndex\", [...callable...], \"max\", [...callable...], \"min\", [...callable...], \"reduceFromLeft\", [...callable...], \"reverse\", [...callable...], \"sort\", [...callable...], \"while\", [...callable...])
a = array(null, null, null, null, null, null, null, null, null, null)


")))

(defun run-tests ()
  (fiveam:run! :shovel-tests))
