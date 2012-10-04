
(in-package :shovel-vm)

(defstruct vm
  bytecode program-counter
  current-environment
  stack
  user-primitives)

(defstruct return-address
  program-counter
  environment)

(defstruct callable
  (primitive nil)
  (program-counter nil)
  (environment nil))

(defmacro def-binary-prim0 (op &key (lisp-op op) type-1 type-2)
  `(list ,(mu-base:mkstr op)
         (make-callable :primitive (lambda (t1 t2)
                                     ,@(if type-1 (list '(check-type t1 number)))
                                     ,@(if type-2 (list '(check-type t2 number)))
                                     (,lisp-op t1 t2)))))

(defmacro def-binary-prim0-ar (op &key (lisp-op op))
  `(def-binary-prim0 ,op :lisp-op ,lisp-op :type-1 'number :type-2 'number))

(defparameter *primitives*
  (list (def-binary-prim0-ar +)
        (def-binary-prim0-ar -)
        (def-binary-prim0-ar *)
        (def-binary-prim0-ar /)
        (def-binary-prim0 "||" :lisp-op or)
        (def-binary-prim0 && :lisp-op and)
        (def-binary-prim0 <=)
        (def-binary-prim0 <)
        (def-binary-prim0 >=)
        (def-binary-prim0 >)
        (def-binary-prim0 == :lisp-op eql)
        (def-binary-prim0 != :lisp-op /=)))

(defun find-required-primitive (name)
  (let ((primitive-record (assoc name *primitives* :test #'string=)))
    (unless primitive-record
      (error "Shovel VM: unknown prim0 '~a'." name))
    (second primitive-record)))

(defun run-vm (bytecode &optional user-primitives)
  (let ((vm (make-vm :bytecode bytecode
                     :program-counter 0
                     :current-environment nil
                     :stack nil
                     :user-primitives (make-hash-table :test #'equal))))
    (dolist (user-primitive user-primitives)
      (setf (gethash (first user-primitive) (vm-user-primitives vm))
            (make-callable :primitive (second user-primitive))))
    (loop while (step-vm vm))
    (first (vm-stack vm))))

(defun vm-not-finished (vm)
  (< (vm-program-counter vm) (length (vm-bytecode vm))))

(defun step-vm (vm)
  (when (vm-not-finished vm)
    (let* ((instruction (elt (vm-bytecode vm) (vm-program-counter vm)))
           (opcode (instruction-opcode instruction))
           (args (instruction-arguments instruction)))
      (case opcode
        (:new-frame
         (push (make-array args) (vm-current-environment vm))
         (incf (vm-program-counter vm)))
        (:drop-frame
         (pop (vm-current-environment vm))
         (incf (vm-program-counter vm)))
        (:const
         (push args (vm-stack vm))
         (incf (vm-program-counter vm)))
        (:prim0
         (push (find-required-primitive args) (vm-stack vm))
         (incf (vm-program-counter vm)))
        (:prim
         (push (find-user-primitive vm args) (vm-stack vm))
         (incf (vm-program-counter vm)))
        (:call (handle-call vm args t))
        (:callj (handle-call vm args nil))
        (:lset
         (set-in-environment (vm-current-environment vm)
                             (first args) (second args)
                             (first (vm-stack vm)))
         (incf (vm-program-counter vm)))
        (:pop
         (pop (vm-stack vm))
         (incf (vm-program-counter vm)))
        (:lget
         (push (get-from-environment (vm-current-environment vm)
                                     (first args) (second args))
               (vm-stack vm))
         (incf (vm-program-counter vm)))
        (:jump (setf (vm-program-counter vm) args))
        (:tjump (jump-if (pop (vm-stack vm)) vm args))
        (:fjump (jump-if (not (pop (vm-stack vm))) vm args))
        (:fn
         (push (make-callable :program-counter args
                              :environment (vm-current-environment vm))
               (vm-stack vm))
         (incf (vm-program-counter vm)))
        (:args (handle-args vm args))
        (:return
          (let ((other-stack (cddr (vm-stack vm)))
                (result (first (vm-stack vm)))
                (retaddr (second (vm-stack vm))))
            (apply-return-address vm retaddr)
            (setf (vm-stack vm)
                  (cons result other-stack))))
        (t (error "Shovel VM: Unknown instruction '~a'." opcode)))))
  (vm-not-finished vm))

(defun find-user-primitive (vm primitive-name)
  (or (gethash primitive-name (vm-user-primitives vm))
      (error "Shovel VM: Unknown user primitive '~a'." primitive-name)))

(defun jump-if (value vm jump-address)
  (if value
      (setf (vm-program-counter vm) jump-address)
      (incf (vm-program-counter vm))))

(defun apply-return-address (vm retaddr)
  (setf (vm-program-counter vm) (return-address-program-counter retaddr)
        (vm-current-environment vm) (return-address-environment retaddr)))

(defun handle-args (vm args)
  (let ((arg-values (subseq (vm-stack vm) 0 args)))
    (setf (vm-stack vm) (subseq (vm-stack vm) args))
    (setf arg-values (nreverse arg-values))
    (dotimes (i (length arg-values))
      (set-in-environment (vm-current-environment vm)
                          0 i
                          (nth i arg-values))))
  (incf (vm-program-counter vm)))

(defun handle-call (vm args save-return-address)
  (let ((callable (pop (vm-stack vm))))
    (unless (callable-p callable)
      (error "Shovel VM: cannot call object."))
    (if (callable-primitive callable)
        (call-primitive callable vm args save-return-address)
        (call-function callable vm args save-return-address))))

(defun call-function (callable vm args save-return-address)
  (when save-return-address
    (setf (vm-stack vm)
          (append
           (subseq (vm-stack vm) 0 args)
           (cons (make-return-address :program-counter (1+ (vm-program-counter vm))
                                      :environment (vm-current-environment vm))
                 (subseq (vm-stack vm) args)))))
  (setf (vm-program-counter vm) (callable-program-counter callable)
        (vm-current-environment vm) (callable-environment callable)))

(defun call-primitive (callable vm args save-return-address)
  (let* ((arg-values (subseq (vm-stack vm) 0 args))
         (result (apply (callable-primitive callable) (reverse arg-values))))
    (setf (vm-stack vm) (subseq (vm-stack vm) args))
    (if save-return-address
        (incf (vm-program-counter vm))
        (apply-return-address vm (pop (vm-stack vm))))
    (push result (vm-stack vm))))

(defun set-in-environment (environment frame-number var-index value)
  (setf (aref (nth frame-number environment) var-index) value))

(defun get-from-environment (enviroment frame-number var-index)
  (aref (nth frame-number enviroment) var-index))
