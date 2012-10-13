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
           (shovel-types:shovel-error (er)
             (princ er)
             (terpri))))
     (values)))

(let ((stdlib-file
       (shovel-types:make-shript-file
        :name "stdlib.shr"
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
        'throw' , tryAndThrow[1]
       )
}
")))
  (defun stdlib ()
    stdlib-file))

(defun get-bytecode (sources)
  (let* ((result (shovel-compiler:assemble-instructions
                  (shovel-compiler:compile-sources-to-instructions sources))))
    (dotimes (i (length result))
      (let ((instruction (aref result i)))
        (when (eq :vm-bytecode-md5
                  (shovel-types:instruction-opcode instruction))
          (setf (shovel-types:instruction-arguments instruction)
                (shovel-compiler:compute-instructions-md5 result)))))
    result))

(defun naked-run-code (sources &key user-primitives)
  (nth-value 0
             (shovel-vm:run-vm (get-bytecode sources)
                               :sources sources
                               :user-primitives user-primitives)))

(defun run-code (sources &key user-primitives debug)
  (let ((*print-circle* t))
    (handle-errors (naked-run-code sources :user-primitives user-primitives))))

(defun print-code (sources &key debug)
  (let ((*print-circle* t))
    (handle-errors
      (shovel-compiler:show-instructions
       sources
       (shovel-compiler:compile-sources-to-instructions sources)))))

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
                   (list (shovel-types:instruction-opcode instruction)
                         (encode-arguments
                          (shovel-types:instruction-arguments instruction))
                         (shovel-types:instruction-start-pos instruction)
                         (shovel-types:instruction-end-pos instruction)
                         (shovel-types:instruction-comments instruction)))
                 (coerce bytecode 'list))))
    (shovel-utils:messagepack-encode-with-md5-checksum transformed-bytecode)))

(defun deserialize-bytecode (bytes)
  "Deserializes an array of bytes into a vector of instructions."
  (let ((messagepack:*decoder-prefers-lists* t))
    (let* ((bytecode-list
            (shovel-utils:check-md5-checksum-and-messagepack-decode bytes))
           (result (make-array (length bytecode-list))))
      (loop
         for bytecode in bytecode-list
         for i from 0
         do (setf (aref result i)
                  (shovel-types:make-instruction
                   :opcode (find-symbol (string-upcase (first bytecode)) :keyword)
                   :arguments (decode-arguments (second bytecode))
                   :start-pos (third bytecode)
                   :end-pos (fourth bytecode)
                   :comments (fifth bytecode))))
      result)))
