;;;; package.lisp

(defpackage #:shovel-tests
  (:use #:cl #:fiveam)
  #. (import-from-shovel-vm)
  (:export
   :run-tests))
