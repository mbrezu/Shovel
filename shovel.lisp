;;;; shovel.lisp

(in-package #:shovel)

;;; "shovel" goes here. Hacks and glory await!

(defclass vm ()
  ((instructions :accessor instructions
                 :initarg :instructions
                 :initform (make-array 10 :adjustable t :fill-pointer 0))
   (stack :accessor stack :initarg :stack :initform nil)
   (control-stack :accessor control-stack :initarg :control-stack :initform nil)
   (program-counter :accessor program-counter :initarg :program-counter :initform 0)
   (addresses :accessor addresses :initarg :addresses :initform (make-hash-table :test #'equal))
   (primitives :accessor primitives :initarg :primitives :initform (make-hash-table :test #'equal))
   (environment :accessor environment
                :initarg :environment
                :initform (list (make-hash-table :test #'equal)))))

(defun vm-finished (vm)
  (>= (program-counter vm)
      (length (instructions vm))))

(defun step-vm (vm)
  (when (vm-finished vm)
    (error "No more opcodes"))
  (let ((instruction (elt (instructions vm) (program-counter vm))))
    (incf (program-counter vm))
    (execute-opcode vm
                    (if (consp instruction) (first instruction) instruction)
                    instruction)))

(defun lookup-frame (environment name)
  (cond ((null environment) nil)
        (t (multiple-value-bind (value present)
               (gethash name (first environment))
             (declare (ignore value))
             (cond (present (first environment))
                   (t (get-from-environment (rest environment) name)))))))

(defun get-from-environment (environment name)
  (let ((frame (lookup-frame environment name)))
    (cond (frame (values (gethash name frame) t))
          (t (values nil nil)))))

(defun set-to-environment (environment name value)
  (let ((frame (lookup-frame environment name)))
    (cond (frame (setf (gethash name frame) value))
          (t (setf (gethash name (first environment)) value)))))

(defun execute-opcode (vm opcode instruction)
  (labels ((do-jump (address)
             (let ((label address))
               (setf (program-counter vm) (gethash (addresses vm) label)))))
    (ecase opcode
      ((jmp) (do-jump (second instruction)))
      ((jmp-indirect) (do-jump (pop (stack vm))))
      ((call)
       (push (program-counter vm) (control-stack vm))
       (do-jump (second instruction)))
      ((call-indirect)
       (push (program-counter vm) (control-stack vm))
       (do-jump (pop (stack vm))))
      ((ret)
       (setf (program-counter vm) (pop (control-stack vm))))
      ((add-environment-frame)
       (push (make-hash-table :test #'equal) (environment vm)))
      ((pop-environment-frame)
       (pop (environment vm)))
      ((call-primitive)
       (funcall (gethash (second instruction) (primitives vm))
                vm))
      ((call-primitive-indirect)
       (funcall (gethash (pop (stack vm)) (primitives vm))
                vm))
      ((load)
       (push (get-from-environment (environment vm) (second instruction))
             (stack vm)))
      ((load-indirect)
       (push (get-from-environment (environment vm) (pop (stack vm)))
             (stack vm)))
      ((store)
       (set-to-environment (environment vm) (second instruction) (pop (stack vm))))
      ((define)
       (setf (gethash (second instruction) (first (environment vm)))
             nil))
      ((define-indirect)
       (setf (gethash (pop (stack vm)) (first (environment vm)))
             nil))
      ((push)
       (push (second instruction) (stack vm)))
      ((pop)
       (pop (stack vm))))))

(defun run-vm (vm)
  (loop
     while (not (vm-finished vm))
     do (step-vm vm)))

(defun install-primitive (vm name code)
  (setf (gethash name (primitives vm)) code))

(defmacro with-args ((vm count) &body body)
  (let ((g-vm (gensym "vm")))
    `(let* ((,g-vm ,vm)
            ,@(loop
                 for i from count downto 1
                 collect (list (mabu:mksymb "ARG" i) `(pop (stack ,g-vm)))))
       ,@body)))

(defvar *basic-primitives*)

(setf *basic-primitives* (list "." (lambda (vm) (print (pop (stack vm))))
                               "dup" (lambda (vm) (let ((top (pop (stack vm))))
                                                    (push top (stack vm))
                                                    (push top (stack vm))))
                               "add" (lambda (vm) (with-args (vm 2)
                                                    (push (+ arg1 arg2) (stack vm))))
                               "subtract" (lambda (vm) (with-args (vm 2)
                                                         (push (- arg1 arg2) (stack vm))))
                               "multiply" (lambda (vm) (with-args (vm 2)
                                                         (push (* arg1 arg2) (stack vm))))
                               "divide" (lambda (vm) (with-args (vm 2)
                                                       (push (* arg1 arg2) (stack vm))))))

(defun install-primitives (vm primitive-list)
  (dolist (primitive (mabu:group primitive-list 2))
    (install-primitive vm (first primitive) (second primitive))))

(defun add-instruction (vm instruction)
  (vector-push-extend instruction (instructions vm)))

(defun make-basic-vm ()
  (let ((vm (make-instance 'vm)))
    (install-primitives vm *basic-primitives*)
    vm))

(defun test-vm ()
  (let ((vm (make-basic-vm)))
    (add-instruction vm '(push 10))
    (add-instruction vm '(push 20))
    (add-instruction vm '(call-primitive "dup"))
    (add-instruction vm '(call-primitive "."))
    (add-instruction vm '(call-primitive "add"))
    (add-instruction vm '(push "."))
    (add-instruction vm 'call-primitive-indirect)
    (run-vm vm)))
