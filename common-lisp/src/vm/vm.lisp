
(in-package :shovel-vm)

(defvar *error-raiser*)

(defstruct vm
  bytecode program-counter
  current-environment
  stack
  user-primitives
  (last-start-pos nil)
  (last-end-post nil)
  (source nil))

(defstruct return-address
  program-counter
  environment)

(defstruct callable
  (prim0 nil)
  (prim nil)
  (program-counter nil)
  (environment nil))

(defmacro def-prim0 (op lisp-op)
  `(cons ,(mu-base:mkstr op) ',lisp-op))

(defparameter *primitives*
  (let ((prim0-alist
         (list
          ;; Arithmetic operators:
          (def-prim0 + shovel-vm-prim0:add)
          (def-prim0 - shovel-vm-prim0:subtract)
          (def-prim0 "unary-minus" shovel-vm-prim0:unary-minus)
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
          (def-prim0 ! shovel-vm-prim0:logical-not)

          ;; Bitwise operators:
          (def-prim0 "&" shovel-vm-prim0:bitwise-and)
          (def-prim0 "|" shovel-vm-prim0:bitwise-or)
          (def-prim0 "^" shovel-vm-prim0:bitwise-xor)

          ;; Hash constructor:
          (def-prim0 "hash" shovel-vm-prim0:hash-constructor)
          (def-prim0 "array" shovel-vm-prim0:array-constructor)
          (def-prim0 "arrayN" shovel-vm-prim0:array-constructor-n)
          (def-prim0 "svm_gref" shovel-vm-prim0:array-or-hash-get)
          (def-prim0 "svm_set_indexed" shovel-vm-prim0:array-or-hash-set)
          (def-prim0 "length" shovel-vm-prim0:get-length)
          (def-prim0 "keys" shovel-vm-prim0:get-hash-table-keys)
          (def-prim0 "slice" shovel-vm-prim0:get-slice)

          ;; Current date/time:
          (def-prim0 "utcSecondsSinceUnixEpoch"
              shovel-vm-prim0:utc-seconds-since-unix-epoch)
          
          ;; Date/time construction/deconstruction:
          (def-prim0 "decodeTime" shovel-vm-prim0:decode-time)
          (def-prim0 "encodeTime" shovel-vm-prim0:encode-time)
          
          ;; Object types:
          (def-prim0 "isString" shovel-vm-prim0:shovel-is-string)
          (def-prim0 "isHash" shovel-vm-prim0:shovel-is-hash)
          (def-prim0 "isBool" shovel-vm-prim0:shovel-is-bool)
          (def-prim0 "isArray" shovel-vm-prim0:shovel-is-array)
          (def-prim0 "isNumber" shovel-vm-prim0:shovel-is-number)
          (def-prim0 "isCallable" shovel-vm-prim0:shovel-is-callable)
          
          ;; Stringification:
          (def-prim0 "string" shovel-vm-prim0:shovel-string)
          (def-prim0 "stringRepresentation" 
              shovel-vm-prim0:shovel-string-representation)
          
          ;; Parsing numbers:
          (def-prim0 "parseInt" shovel-vm-prim0:parse-int)
          (def-prim0 "parseFloat" shovel-vm-prim0:parse-float))))
    (alexandria:alist-hash-table prim0-alist :test #'equal)))

(defun raise-shovel-error (vm message)
  (alexandria:when-let ((source (vm-source vm))
                        (pos (vm-last-start-pos vm)))
    (setf message (format nil "~a~%~a" message (highlight-position source pos))))
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
            (second user-primitive)))
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
      (unless (member opcode '(:call :callj))
        (alexandria:when-let ((start-pos (instruction-start-pos instruction))
                              (end-pos (instruction-end-pos instruction)))
          (setf (vm-last-start-pos vm) start-pos
                (vm-last-end-post vm) end-pos)))
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
      (raise-shovel-error vm (format nil "Object is not callable.")))
    (if (or (callable-prim callable) (callable-prim0 callable))
        (call-primitive callable vm num-args save-return-address)
        (call-function callable vm num-args save-return-address))))

(defun call-function (callable vm num-args save-return-address)
  (when save-return-address
    (setf (vm-stack vm)
          (append
           (subseq (vm-stack vm) 0 num-args)
           (cons (make-return-address :program-counter (1+ (vm-program-counter vm))
                                      :environment (vm-current-environment vm))
                 (subseq (vm-stack vm) num-args)))))
  (setf (vm-program-counter vm) (callable-program-counter callable)
        (vm-current-environment vm) (callable-environment callable)))

(defun call-primitive (callable vm num-args save-return-address)
  (let* ((arg-values (subseq (vm-stack vm) 0 num-args))
         (primitive (or (alexandria:if-let (prim0 (callable-prim0 callable))
                          (find-required-primitive vm prim0))
                        (alexandria:if-let (prim (callable-prim callable))
                          (find-user-primitive vm prim))))
         (result (apply primitive (reverse arg-values))))
    (setf (vm-stack vm) (subseq (vm-stack vm) num-args))
    (if save-return-address
        (incf (vm-program-counter vm))
        (apply-return-address vm (pop (vm-stack vm))))
    (push result (vm-stack vm))))

(defun set-in-environment (environment frame-number var-index value)
  (setf (aref (nth frame-number environment) var-index) value))

(defun get-from-environment (enviroment frame-number var-index)
  (aref (nth frame-number enviroment) var-index))
