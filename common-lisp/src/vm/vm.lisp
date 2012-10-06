
(in-package :shovel-vm)

(defvar *error-raiser*)

(defstruct vm
  bytecode program-counter
  current-environment
  stack
  user-primitives
  (last-start-pos nil)
  (last-end-pos nil)
  (source nil))

(defstruct return-address
  program-counter
  environment)

(defstruct callable
  (prim0 nil)
  (prim nil)
  (num-args nil)
  (program-counter nil)
  (environment nil))

(defmacro def-prim0 (op lisp-op &optional (arity 2))
  `(list ,(mu-base:mkstr op) ',lisp-op ,arity))

(defparameter *primitives*
  (let ((prim0-alist
         (list
          ;; Arithmetic operators:
          (def-prim0 + shovel-vm-prim0:add)
          (def-prim0 - shovel-vm-prim0:subtract)
          (def-prim0 "unary-minus" shovel-vm-prim0:unary-minus 1)
          (def-prim0 * shovel-vm-prim0:multiply)
          (def-prim0 / shovel-vm-prim0:divide)
          (def-prim0 << shovel-vm-prim0:shift-left)
          (def-prim0 >> shovel-vm-prim0:shift-right)
          (def-prim0 % shovel-vm-prim0:modulo)
          (def-prim0 "pow" shovel-vm-prim0:pow)

          ;; Relational operators:
          (def-prim0 < shovel-vm-prim0:less-than)
          (def-prim0 <= shovel-vm-prim0:less-than-or-equal)
          (def-prim0 > shovel-vm-prim0:greater-than)
          (def-prim0 >= shovel-vm-prim0:greater-than-or-equal)
          (def-prim0 == shovel-vm-prim0:are-equal)
          (def-prim0 != shovel-vm-prim0:are-not-equal)

          ;; Logic operators:
          (def-prim0 && shovel-vm-prim0:logical-and)
          (def-prim0 "||" shovel-vm-prim0:logical-or)
          (def-prim0 ! shovel-vm-prim0:logical-not 1)

          ;; Bitwise operators:
          (def-prim0 "&" shovel-vm-prim0:bitwise-and)
          (def-prim0 "|" shovel-vm-prim0:bitwise-or)
          (def-prim0 "^" shovel-vm-prim0:bitwise-xor)

          ;; Hash constructor:
          (def-prim0 "hash" shovel-vm-prim0:hash-constructor nil)

          ;; Hash table has key?
          (def-prim0 "hasKey" shovel-vm-prim0:has-key 2)

          ;; Keys for hash table
          (def-prim0 "keys" shovel-vm-prim0:get-hash-table-keys 1)

          ;; Array constructors:
          (def-prim0 "array" shovel-vm-prim0:array-constructor nil)
          (def-prim0 "arrayN" shovel-vm-prim0:array-constructor-n 1)

          ;; Array and hash set and get:
          (def-prim0 "svm_gref" shovel-vm-prim0:array-or-hash-get)
          (def-prim0 "svm_gref_dot" shovel-vm-prim0:hash-get-dot)
          (def-prim0 "svm_set_indexed" shovel-vm-prim0:array-or-hash-set 3)

          ;; String or array length:
          (def-prim0 "length" shovel-vm-prim0:get-length 1)

          ;; String or array slice:
          (def-prim0 "slice" shovel-vm-prim0:get-slice 3)

          ;; Current date/time:
          (def-prim0 "utcSecondsSinceUnixEpoch"
              shovel-vm-prim0:utc-seconds-since-unix-epoch 0)

          ;; Date/time construction/deconstruction:
          (def-prim0 "decodeTime" shovel-vm-prim0:decode-time 1)
          (def-prim0 "encodeTime" shovel-vm-prim0:encode-time 1)

          ;; Object types:
          (def-prim0 "isString" shovel-vm-prim0:shovel-is-string 1)
          (def-prim0 "isHash" shovel-vm-prim0:shovel-is-hash 1)
          (def-prim0 "isBool" shovel-vm-prim0:shovel-is-bool 1)
          (def-prim0 "isArray" shovel-vm-prim0:shovel-is-array 1)
          (def-prim0 "isNumber" shovel-vm-prim0:shovel-is-number 1)
          (def-prim0 "isCallable" shovel-vm-prim0:shovel-is-callable 1)

          ;; Stringification:
          (def-prim0 "string" shovel-vm-prim0:shovel-string 1)
          (def-prim0 "stringRepresentation"
              shovel-vm-prim0:shovel-string-representation 1)

          ;; Parsing numbers:
          (def-prim0 "parseInt" shovel-vm-prim0:parse-int 1)
          (def-prim0 "parseFloat" shovel-vm-prim0:parse-float 1))))
    (alexandria:alist-hash-table prim0-alist :test #'equal)))

(defun write-environment (env stream)
  (when env
    (let ((frame (car env)))
      (loop
         for i from 0 to (1- (length frame))
         do (let ((var (elt frame i)))
              (format stream "~a = ~a~%"
                      (car var)
                      (shovel-vm-prim0:shovel-string-representation (cdr var))))))
    (write-environment (cdr env) stream)))

(defun write-stack-trace (vm stream &optional stack-dump)
  (let ((source (vm-source vm)))
    (labels ((print-call-site (start-pos end-pos)
               (if (and start-pos end-pos)
                   (if source
                       (dolist (line
                                 (extract-relevant-source source
                                                          start-pos end-pos))
                         (write-string line stream)
                         (terpri stream))
                       (progn
                         (format stream "Call from line ~d, column ~d."
                                 (pos-line start-pos) (pos-column start-pos))
                         (terpri stream)))
                   (progn
                     (format stream "Call from unknown source location.")
                     (terpri stream))))
             (iter (stack)
               (when stack
                 (if (return-address-p (car stack))
                     (let* ((pc (return-address-program-counter (car stack)))
                            (call-site (elt (vm-bytecode vm) (1- pc))))
                       (print-call-site (instruction-start-pos call-site)
                                        (instruction-end-pos call-site)))
                     (if stack-dump
                         (format stream "~a~%"
                                 (shovel-vm-prim0:shovel-string-representation
                                  (car stack)))))
                 (iter (cdr stack)))))
      (unless stack-dump
        (print-call-site (vm-last-start-pos vm) (vm-last-end-pos vm)))
      (iter (vm-stack vm)))))

(defun raise-shovel-error (vm message)
  (setf message
        (with-output-to-string (str)
          (write-string message str) (terpri str) (terpri str)
          (write-string "Current stack trace:" str) (terpri str)
          (write-stack-trace vm str)
          (terpri str)
          (write-string "Current environment:" str) (terpri str)
          (write-environment (vm-current-environment vm) str)))
  (error
   (alexandria:if-let (pos (vm-last-start-pos vm))
     (make-condition 'shovel-error
                     :message message
                     :line (pos-line pos)
                     :column (pos-column pos))
     (make-condition 'shovel-error :message message))))

(defun find-required-primitive (vm name)
  (let ((primitive (gethash name *primitives*)))
    (unless primitive
      (raise-shovel-error vm (format nil "Unknown prim0 '~a'." name)))
    primitive))

(defun run-vm (bytecode &key source user-primitives)
  (let ((vm (make-vm :bytecode bytecode
                     :program-counter 0
                     :current-environment nil
                     :stack nil
                     :user-primitives (make-hash-table :test #'equal)
                     :source source)))
    (dolist (user-primitive user-primitives)
      (setf (gethash (first user-primitive) (vm-user-primitives vm))
            (rest user-primitive)))
    (loop while (step-vm vm))
    (first (vm-stack vm))))

(defun vm-not-finished (vm)
  (< (vm-program-counter vm) (length (vm-bytecode vm))))

(defun check-bool (vm)
  (unless (shovel-vm-prim0:is-bool (first (vm-stack vm)))
    (raise-shovel-error vm "Argument must be a boolean.")))

(defun step-vm (vm)
  (when (vm-not-finished vm)
    (let* ((shovel-vm:*error-raiser* (lambda (message)
                                       (raise-shovel-error vm message)))
           (instruction (elt (vm-bytecode vm) (vm-program-counter vm)))
           (opcode (instruction-opcode instruction))
           (args (instruction-arguments instruction)))
      (alexandria:when-let ((start-pos (instruction-start-pos instruction))
                            (end-pos (instruction-end-pos instruction)))
        (setf (vm-last-start-pos vm) start-pos
              (vm-last-end-pos vm) end-pos))
      (case opcode
        (:new-frame
         (let ((new-frame (make-array (length args))))
           (loop
              for i = 0 then (1+ i)
              for var in args
              do (setf (aref new-frame i) (cons var :null)))
           (push new-frame (vm-current-environment vm)))
         (incf (vm-program-counter vm)))
        (:drop-frame
         (pop (vm-current-environment vm))
         (incf (vm-program-counter vm)))
        (:const
         (push args (vm-stack vm))
         (incf (vm-program-counter vm)))
        (:prim0
         (push (make-callable :prim0 args) (vm-stack vm))
         (incf (vm-program-counter vm)))
        (:prim
         (push (make-callable :prim args) (vm-stack vm))
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
        (:tjump
         (check-bool vm)
         (jump-if (pop (vm-stack vm)) vm args))
        (:fjump
         (check-bool vm)
         (jump-if (shovel-vm-prim0:logical-not (pop (vm-stack vm))) vm args))
        (:fn
         (push (make-callable :program-counter (first args)
                              :environment (vm-current-environment vm)
                              :num-args (second args))
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
        (t (error "Shovel internal WTF: unknown instruction '~a'." opcode)))))
  (vm-not-finished vm))

(defun find-user-primitive (vm primitive-name)
  (or (gethash primitive-name (vm-user-primitives vm))
      (raise-shovel-error vm
                          (format nil "Unknown user primitive '~a'."
                                  primitive-name))))

(defun jump-if (value vm jump-address)
  (if (shovel-vm-prim0:is-true value)
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

(defun handle-call (vm num-args save-return-address)
  (let ((callable (pop (vm-stack vm))))
    (unless (callable-p callable)
      (raise-shovel-error vm (format nil "Object [~a] is not callable."
                                     (shovel-vm-prim0:shovel-string-representation
                                      callable))))
    (if (or (callable-prim callable) (callable-prim0 callable))
        (call-primitive callable vm num-args save-return-address)
        (call-function callable vm num-args save-return-address))))

(defun arity-error (vm expected-arity actual-arity)
  (raise-shovel-error
   vm (format nil "Function of ~d arguments called with ~d arguments."
              expected-arity actual-arity)))

(defun call-function (callable vm num-args save-return-address)
  (when save-return-address
    (setf (vm-stack vm)
          (append
           (subseq (vm-stack vm) 0 num-args)
           (cons (make-return-address :program-counter (1+ (vm-program-counter vm))
                                      :environment (vm-current-environment vm))
                 (subseq (vm-stack vm) num-args)))))
  (when (and (callable-num-args callable )
             (/= (callable-num-args callable) num-args))
    (arity-error vm (callable-num-args callable) num-args))
  (setf (vm-program-counter vm) (callable-program-counter callable)
        (vm-current-environment vm) (callable-environment callable)))

(defun call-primitive (callable vm num-args save-return-address)
  (let* ((arg-values (subseq (vm-stack vm) 0 num-args))
         (primitive-record (or (alexandria:if-let (prim0 (callable-prim0 callable))
                                 (find-required-primitive vm prim0))
                               (alexandria:if-let (prim (callable-prim callable))
                                 (find-user-primitive vm prim))))
         (primitive (first primitive-record))
         (primitive-arity (second primitive-record)))
    (when (and primitive-arity (/= primitive-arity num-args))
      (arity-error vm primitive-arity num-args))
    (let ((result (apply primitive (reverse arg-values))))
      (setf (vm-stack vm) (subseq (vm-stack vm) num-args))
      (if save-return-address
          (incf (vm-program-counter vm))
          (apply-return-address vm (pop (vm-stack vm))))
      (push result (vm-stack vm)))))

(defun set-in-environment (environment frame-number var-index value)
  (setf (cdr (aref (nth frame-number environment) var-index)) value))

(defun get-from-environment (enviroment frame-number var-index)
  (cdr (aref (nth frame-number enviroment) var-index)))
