;;;; shovel.lisp

(in-package #:shovel)

(defun run-code (source &optional user-primitives)
  (let ((*print-circle* t))
    (print
     (shovel-vm:run-vm
      (shovel-compiler:assemble-instructions
       (shovel-compiler:compile-string-to-instructions source))
      user-primitives))
    (values)))

(defun print-code (source)
  (shovel-compiler:show-instructions
       (shovel-compiler:compile-string-to-instructions source)))
