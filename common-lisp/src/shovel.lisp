;; Copyright (c) 2012, Miron Brezuleanu
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; shovel.lisp

(in-package #:shovel)

(defmacro handle-errors (&body body)
  `(let ((action (lambda () ,@body)))
     (if debug
         (progn (princ (funcall action)) (terpri))
         (handler-case
             (progn
               (princ (funcall action))
               (terpri))
           (shovel:shovel-error (er)
             (princ er)
             (terpri))))
     (values)))

(let ((stdlib-file
       (shovel:make-source-file
        :name "stdlib.sho"
        :contents "
var stdlib = {
   var min = fn (a, b) if a < b a else b
   var max = fn (a, b) if a > b a else b
   var while = fn (condition, body) {
     if condition() {
       body()
       while(condition, body)
     }
   }
   var forIndex = fn (arr, fun) {
     var i = 0
     while (fn () i < length(arr), fn () {
       fun(i)
       i = i + 1
     })
   }
   var forEach = fn (arr, fun) {
     forIndex(arr, fn i fun(arr[i]))
   }
   var forEachWithIndex = fn (arr, fun) {
     forIndex(arr, fn i fun(arr[i], i))
   }
   var map = fn (arr, fun) {
     var result = arrayN(length(arr))
     forIndex(arr, fn i result[i] = fun(arr[i]))
     result
   }
   var mapWithIndex = fn (arr, fun) {
     var result = arrayN(length(arr))
     forIndex(arr, fn i result[i] = fun(arr[i], i))
     result
   }
   var filter = fn (arr, fun) {
     var result = arrayN(length(arr))
     var ptr = 0
     forIndex(arr, fn i if fun(arr[i]) {
       result[ptr] = arr[i]
       ptr = ptr + 1
     })
     slice(result, 0, ptr)
   }
   var reduceFromLeft = fn (arr, initialElement, fun) {
     var result = initialElement
     forEach(arr, fn item result = fun(result, item))
     result
   }
   var qsort = fn (arr, lessThan) {
     if length(arr) == 0 || length(arr) == 1
     arr
     else {
       var pivot = arr[0]
       var butFirst = slice(arr, 1, -1)
       var lesser = filter(butFirst, fn el lessThan(el, pivot))
       var greater = filter(butFirst, fn el !lessThan(el, pivot))
       qsort(lesser, lessThan) + array(pivot) + qsort(greater, lessThan)
     }
   }
   var reverse = fn (arr) {
     var result = arrayN(length(arr))
     forIndex(arr, fn i result[length(arr) - 1 - i] = arr[i])
     result
   }

   var getPrefixedBlockName = {
     var namedBlockCounter = 0
     fn (prefix) {
       namedBlockCounter = namedBlockCounter + 1
       prefix + '_' +string(namedBlockCounter)
     }
   }
   var getBlockName = fn () getPrefixedBlockName('block')

   var tryAndThrow = {
     var tryStack = array()
     var throw = fn (error) {
       var blockName = pop(tryStack)
       push(tryStack, array(error))
       return blockName null
     }
     var try = fn (tryCode, catchCode) {
       var newBlockName = getPrefixedBlockName('tryCatchBlock')
       push(tryStack, newBlockName)
       var exitValue = block newBlockName tryCode()
       var stackTop = pop(tryStack)
       if isArray(stackTop) catchCode(stackTop[0])
       else exitValue
     }
     array(try, throw)
   }

   var repeat = fn (count, fun) {
     var counter = 0
     while (fn () counter < count, fn () {
       fun()
       counter = counter + 1
     })
   }

   hash('min', min,
        'max', max,
        'while', while,
        'forIndex', forIndex,
        'forEach', forEach,
        'forEachWithIndex', forEachWithIndex,
        'map', map,
        'mapWithIndex', mapWithIndex,
        'filter', filter,
        'reduceFromLeft', reduceFromLeft,
        'sort', qsort,
        'reverse', reverse,
        'getPrefixedBlockName', getPrefixedBlockName,
        'getBlockName', getBlockName,
        'try', tryAndThrow[0],
        'throw' , tryAndThrow[1],
        'repeat', repeat
       )
}
")))
  (defun stdlib ()
    "Source code for the Shovel standard library."
    stdlib-file))

(defun get-bytecode (sources)
  "Compiles and assembles SOURCES into bytecode that can be passed to RUN-VM."
  (let* ((result (assemble-instructions
                  (compile-sources-to-instructions sources))))
    (dotimes (i (length result))
      (let ((instruction (aref result i)))
        (when (eq :vm-bytecode-md5
                  (instruction-opcode instruction))
          (setf (instruction-arguments instruction)
                (compute-instructions-md5 result)))))
    result))

(defun naked-run-code (sources &key user-primitives)
  "Compiles and runs SOURCES with the user-defined primitives defined
by USER-PRIMITIVES (see the documentation for RUN-VM for a description
of the format for USER-PRIMITIVES). Returns the first value returned
by RUN-VM."
  (nth-value 0
             (run-vm (get-bytecode sources)
                     :sources sources
                     :user-primitives user-primitives)))

(defun run-code (sources &key user-primitives debug)
  "Runs NAKED-RUN-CODE and prints the result (or the exception thrown)."
  (let ((*print-circle* t))
    (handle-errors (naked-run-code sources :user-primitives user-primitives))))

(defun print-code (sources &key debug)
  "Compiles SOURCES and prints the resulting bytecode to *STANDARD-OUTPUT*."
  (let ((*print-circle* t))
    (handle-errors
      (show-instructions sources
                         (compile-sources-to-instructions sources)))))

(defmacro get-serialization-code (symbol)
  (case symbol
    (:verbatim 0)
    (:true 1)
    (:false 2)
    (:guest-null 3)))

(defun encode-arguments (arguments)
  (cond ((eq :null arguments)
         (make-array 1 :initial-element (get-serialization-code :guest-null)))
        ((eq :true arguments)
         (make-array 1 :initial-element (get-serialization-code :true)))
        ((eq :false arguments)
         (make-array 1 :initial-element (get-serialization-code :false)))
        (t (let ((result (make-array 2)))
             (setf (aref result 0) (get-serialization-code :verbatim))
             (setf (aref result 1) arguments)
             result))))

(defun decode-arguments (encoded-arguments)
  (let ((head (first encoded-arguments)))
    (cond
      ((= (get-serialization-code :verbatim) head) (second encoded-arguments))
      ((= (get-serialization-code :guest-null) head) :null)
      ((= (get-serialization-code :true) head) :true)
      ((= (get-serialization-code :false) head) :false))))

(defun serialize-bytecode (bytecode)
  "Serializes the output of SHOVEL-COMPILER:ASSEMBLE-INSTRUCTIONS into
an array of bytes."
  (let ((transformed-bytecode
         (mapcar (lambda (instruction)
                   (list (instruction-opcode instruction)
                         (encode-arguments
                          (instruction-arguments instruction))
                         (instruction-start-pos instruction)
                         (instruction-end-pos instruction)
                         (instruction-comments instruction)
                         (instruction-opcode-num instruction)))
                 (coerce bytecode 'list))))
    (messagepack-encode-with-md5-checksum transformed-bytecode)))

(defun deserialize-bytecode (bytes)
  "Deserializes an array of bytes into a vector of instructions."
  (let ((messagepack:*decoder-prefers-lists* t))
    (let* ((bytecode-list
            (check-md5-checksum-and-messagepack-decode bytes))
           (result (make-array (length bytecode-list))))
      (loop
         for bytecode in bytecode-list
         for i from 0
         do (setf (aref result i)
                  (make-instruction
                   :opcode (find-symbol (string-upcase (first bytecode)) :keyword)
                   :arguments (decode-arguments (second bytecode))
                   :start-pos (third bytecode)
                   :end-pos (fourth bytecode)
                   :comments (fifth bytecode)
                   :opcode-num (sixth bytecode))))
      result)))



(defun increment-ticks (ticks)
  "To be called from user-defined primitives. Increments the number of
executed ticks for the executing VM by TICKS.

This function can be used by user-defined primitives that want to
model their CPU cost (i.e. any primitive call uses 1 tick, but a CPU
intensive primitive can model the fact that it is expensive by calling
INCREMENT-TICKS with a wisely chosen number of ticks)."
  (when *ticks-incrementer*
    (funcall *ticks-incrementer* ticks)))

(defun increment-cells (cells)
  "To be called from user-defined primitives. Increments the number of
allocated cells for the executing VM by CELLS.

This function can be used by user-defined primitives that want to
model their memory usage (i.e. tell the VM that they allocated memory
which they made available to the VM via their return value)."
  (when *cells-incrementer*
    (funcall *cells-incrementer* cells)))

(defun vm-used-cells (vm)
  "The number of cells currently used by the VM."
  (vm-really-used-cells vm))

(defun vm-version (vm)
  "This function returns the version encoded in this particular VM's
bytecode (meaning you need an interpreter with at least this version
to resume this VM)."
  (get-vm-version vm))

(defun vm-bytecode-md5 (vm)
  "Returns the MD5 hash for the bytecode for this VM."
  (get-vm-bytecode-md5 vm))

(defun vm-sources-md5 (vm)
  "Returns the MD5 hash for the sources used to compile the bytecode
  for this VM."
  (get-vm-sources-md5 vm))


