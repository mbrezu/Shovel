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
     return result
   }
   var mapWithIndex = fn (arr, fun) {
     var result = arrayN(length(arr))
     forIndex(arr, fn i result[i] = fun(arr[i], i))
     return result
   }
   var filter = fn (arr, fun) {
     var result = arrayN(length(arr))
     var ptr = 0
     forIndex(arr, fn i if fun(arr[i]) {
       result[ptr] = arr[i]
       ptr = ptr + 1
     })
     return slice(result, 0, ptr)
   }
   var reduceFromLeft = fn (arr, initialElement, fun) {
     var result = initialElement
     forEach(arr, fn item result = fun(result, item))
     return result
   }
   var qsort = fn (arr, lessThan) {
     if length(arr) == 0 || length(arr) == 1 return arr
     var pivot = arr[0]
     var butFirst = slice(arr, 1, -1)
     var lesser = filter(butFirst, fn el lessThan(el, pivot))
     var greater = filter(butFirst, fn el !lessThan(el, pivot))
     return qsort(lesser, lessThan) + array(pivot) + qsort(greater, lessThan)
   }
   var reverse = fn (arr) {
     var result = arrayN(length(arr))
     forIndex(arr, fn i result[length(arr) - 1 - i] = arr[i])
     return result
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
        'reverse', reverse
       )
}
")))
  (defun stdlib ()
    stdlib-file))

(defun naked-run-code (sources &key user-primitives)
  (setf sources (shovel-utils:prepare-sources sources))
  (shovel-vm:run-vm
   (shovel-compiler:assemble-instructions
    (shovel-compiler:compile-sources-to-instructions sources))
   :sources sources
   :user-primitives user-primitives))

(defun run-code (sources &key user-primitives debug)
  (let ((*print-circle* t))
    (handle-errors (naked-run-code sources :user-primitives user-primitives))))

(defun print-code (sources &key debug)
  (setf sources (shovel-utils:prepare-sources sources))
  (let ((*print-circle* t))
    (handle-errors
      (shovel-compiler:show-instructions
       sources
       (shovel-compiler:compile-sources-to-instructions sources)))))
