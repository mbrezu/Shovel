;;;; shovel.lisp

(in-package #:shovel)

(defmacro handle-errors (&body body)
  `(let ((action (lambda () ,@body)))
     (if debug
         (progn (princ (funcall action)) (terpri))
         (multiple-value-bind (result error)
             (ignore-errors (funcall action))
           (unless error (princ result) (terpri))
           (when error (princ error) (terpri))))
     (values)))

(defun run-code (source &key user-primitives debug)
  (let ((*print-circle* t))
    (handle-errors (shovel-vm:run-vm
                    (shovel-compiler:assemble-instructions
                     (shovel-compiler:compile-string-to-instructions source))
                    :source source
                    :user-primitives user-primitives))))

(defun print-code (source &key debug)
  (let ((*print-circle* t))
    (handle-errors
      (shovel-compiler:show-instructions
       (shovel-compiler:compile-string-to-instructions source)))))
