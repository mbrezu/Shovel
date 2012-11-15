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

(in-package :shovel-vm)

(defvar *error-raiser*)

(defvar *ticks-incrementer* nil)

(defvar *cells-incrementer* nil)

(defvar *cells-increment-herald* nil)

(defstruct vm
  bytecode
  (program-counter 0 :type fixnum)
  current-environment
  stack
  user-primitives
  (used-cells 0 :type fixnum)
  (executed-ticks 0 :type integer)
  (executed-ticks-since-last-nap 0 :type integer)
  (sources nil)
  (should-take-a-nap nil)
  (user-defined-primitive-error nil)
  (programming-error nil)
  (cells-quota nil :type (or null integer))
  (total-ticks-quota nil :type (or null integer))
  (until-next-nap-ticks-quota nil :type (or null integer))
  (runs nil))

(defun get-vm-user-defined-primitive-error (vm)
  "Returns the unhandled condition thrown from a user-defined
primitive, or NIL if none."
  (vm-user-defined-primitive-error vm))

(defun get-vm-programming-error (vm)
  "Returns the programming error thrown while running VM, or NIL if
none."
  (vm-program-counter vm))

(defstruct return-address
  program-counter
  environment)

(defstruct named-block name end-address environment)

(defstruct callable
  (prim0 nil)
  (prim nil)
  (num-args nil :type (or null fixnum))
  (program-counter nil :type (or null fixnum))
  (environment nil)
  (cached-prim nil))

(defstruct env-frame introduced-at-program-counter var-names vars)

(defmacro def-prim0 (op lisp-op &optional (arity 2))
  `(list ,(format nil "~a" op) #',lisp-op ,arity))

(defparameter *primitives*
  (let ((prim0-alist
         (list
          ;; Arithmetic operators:
          (def-prim0 + shovel-vm-prim0::add)
          (def-prim0 - shovel-vm-prim0::subtract)
          (def-prim0 "unary-minus" shovel-vm-prim0::unary-minus 1)
          (def-prim0 * shovel-vm-prim0::multiply)
          (def-prim0 / shovel-vm-prim0::divide)
          (def-prim0 << shovel-vm-prim0::shift-left)
          (def-prim0 >> shovel-vm-prim0::shift-right)
          (def-prim0 % shovel-vm-prim0::modulo)
          (def-prim0 "pow" shovel-vm-prim0::pow)
          (def-prim0 "floor" floor 1)

          ;; Relational operators:
          (def-prim0 < shovel-vm-prim0::less-than)
          (def-prim0 <= shovel-vm-prim0::less-than-or-equal)
          (def-prim0 > shovel-vm-prim0::greater-than)
          (def-prim0 >= shovel-vm-prim0::greater-than-or-equal)
          (def-prim0 == shovel-vm-prim0::are-equal)
          (def-prim0 != shovel-vm-prim0::are-not-equal)

          ;; Logic operators:
          (def-prim0 && shovel-vm-prim0::logical-and)
          (def-prim0 "||" shovel-vm-prim0::logical-or)
          (def-prim0 ! shovel-vm-prim0::logical-not 1)

          ;; Bitwise operators:
          (def-prim0 "&" shovel-vm-prim0::bitwise-and)
          (def-prim0 "|" shovel-vm-prim0::bitwise-or)
          (def-prim0 "^" shovel-vm-prim0::bitwise-xor)

          ;; Hash constructor:
          (def-prim0 "hash" shovel-vm-prim0::hash-constructor nil)

          ;; Hash table has key?
          (def-prim0 "hasKey" shovel-vm-prim0::has-key 2)

          ;; Keys for hash table
          (def-prim0 "keys" shovel-vm-prim0::get-hash-table-keys 1)

          ;; Array constructors:
          (def-prim0 "array" shovel-vm-prim0::array-constructor nil)
          (def-prim0 "arrayN" shovel-vm-prim0::array-constructor-n 1)

          ;; Array push and pop:
          (def-prim0 "push" shovel-vm-prim0::array-push)
          (def-prim0 "pop" shovel-vm-prim0::array-pop 1)

          ;; Array and hash set and get:
          (def-prim0 "svm_gref" shovel-vm-prim0::array-or-hash-get)
          (def-prim0 "svm_gref_dot" shovel-vm-prim0::hash-get-dot)
          (def-prim0 "svm_set_indexed" shovel-vm-prim0::array-or-hash-set 3)

          ;; String or array length:
          (def-prim0 "length" shovel-vm-prim0::get-length 1)

          ;; String or array slice:
          (def-prim0 "slice" shovel-vm-prim0::get-slice 3)

          ;; String 'upper' and 'lower':
          (def-prim0 "upper" shovel-vm-prim0::string-upper 1)
          (def-prim0 "lower" shovel-vm-prim0::string-lower 1)

          ;; Current date/time:
          (def-prim0 "utcSecondsSinceUnixEpoch"
              shovel-vm-prim0::utc-seconds-since-unix-epoch 0)

          ;; Date/time construction/deconstruction:
          (def-prim0 "decodeTime" shovel-vm-prim0::decode-time 1)
          (def-prim0 "encodeTime" shovel-vm-prim0::encode-time 1)

          ;; Object types:
          (def-prim0 "isString" shovel-vm-prim0::shovel-is-string 1)
          (def-prim0 "isHash" shovel-vm-prim0::shovel-is-hash 1)
          (def-prim0 "isBool" shovel-vm-prim0::shovel-is-bool 1)
          (def-prim0 "isArray" shovel-vm-prim0::shovel-is-array 1)
          (def-prim0 "isNumber" shovel-vm-prim0::shovel-is-number 1)
          (def-prim0 "isInteger" shovel-vm-prim0::shovel-is-integer 1)
          (def-prim0 "isCallable" shovel-vm-prim0::shovel-is-callable 1)

          ;; Stringification:
          (def-prim0 "string" shovel-vm-prim0::shovel-string 1)
          (def-prim0 "stringRepresentation"
              shovel-vm-prim0::shovel-string-representation 1)

          ;; Parsing numbers:
          (def-prim0 "parseInt" shovel-vm-prim0::parse-int 1)
          (def-prim0 "parseFloat" shovel-vm-prim0::parse-float 1)

          ;; Exception throwing:
          (def-prim0 "panic" shovel-vm-prim0::panic 1)
          )))
    (alexandria:alist-hash-table prim0-alist :test #'equal)))

(defun write-environment (env vm stream)
  (when env
    (let ((frame (car env)))
      (alexandria:when-let (pc (env-frame-introduced-at-program-counter frame))
        (let ((instruction (aref (vm-bytecode vm) pc)))
          (format stream "Frame starts at:~%")
          (print-line-for vm stream
                          (env-frame-introduced-at-program-counter frame)
                          (instruction-start-pos instruction)
                          (instruction-end-pos instruction))))
      (format stream "Frame variables are:~%")
      (loop
         for i from 0 to (1- (length (env-frame-vars frame)))
         do (let ((val (elt (env-frame-vars frame) i))
                  (var (elt (env-frame-var-names frame) i)))
              (format stream "~a = ~a~%"
                      var
                      (shovel-vm-prim0::shovel-string-representation val))))
      (terpri stream))
    (write-environment (cdr env) vm stream)))

(defun find-file-name (vm program-counter)
  (let (result)
    (loop
       for pc from program-counter downto 0
       when (eq :file-name (instruction-opcode (aref (vm-bytecode vm) pc)))
       do
         (setf result (instruction-arguments (aref (vm-bytecode vm) pc)))
         (loop-finish))
    result))

(defun print-line-for (vm stream
                       program-counter
                       character-start-pos character-end-pos)
  (let (found-location)
    (when (and character-start-pos character-end-pos)
      (alexandria:when-let (sources (vm-sources vm))
        (alexandria:when-let*
            ((file-name (find-file-name vm program-counter))
             (source-file (find-source sources file-name))
             (content (shovel:source-file-contents source-file))
             (start-pos (find-position file-name
                                       content
                                       character-start-pos))
             (end-pos (find-position file-name
                                     content
                                     character-end-pos)))
          (dolist (line (extract-relevant-source sources
                                                 start-pos end-pos))
            (write-string line stream)
            (terpri stream))
          (setf found-location t))))
    (unless found-location
      (format stream "... unknown source location, program counter ~d ..."
              program-counter)
      (terpri stream))))

(defun wake-up-vm (vm)
  "If VM went to sleep and you want to resume it without rebuilding
it, you have to call this function before calling RUN-VM."
  (setf (vm-should-take-a-nap vm) nil))

(defun find-start-end-pos (vm)
  (labels ((find-em (pc)
             (if (< pc 0)
                 (values nil nil)
                 (let* ((bytecode (vm-bytecode vm))
                        (instruction (aref bytecode pc)))
                   (alexandria:if-let ((start-pos (instruction-start-pos instruction))
                                       (end-pos (instruction-end-pos instruction)))
                     (values start-pos end-pos)
                     (find-em (1- pc)))))))
    (find-em (vm-program-counter vm))))

(defun write-stack-trace (vm stream &optional stack-dump)
  (labels ((iter (stack)
             (when stack
               (if (return-address-p (car stack))
                   (let* ((pc (return-address-program-counter (car stack)))
                          (call-site (elt (vm-bytecode vm) (1- pc))))
                     (print-line-for vm stream pc
                                     (instruction-start-pos call-site)
                                     (instruction-end-pos call-site)))
                   (if stack-dump
                       (format stream "~a~%"
                               (shovel-vm-prim0::shovel-string-representation
                                (car stack)))))
               (iter (cdr stack)))))
    (unless stack-dump
      (multiple-value-bind (start-pos end-pos)
          (find-start-end-pos vm)
        (print-line-for vm stream
                        (vm-program-counter vm)
                        start-pos end-pos)))
    (iter (vm-stack vm))))

(defun raise-shovel-error (vm message)
  (setf message
        (with-output-to-string (str)
          (write-string message str) (terpri str) (terpri str)
          (write-string "Current stack trace:" str) (terpri str)
          (write-stack-trace vm str)
          (terpri str)
          (write-string "Current environment:" str) (terpri str) (terpri str)
          (write-environment (vm-current-environment vm) vm str)))
  (let (pos file-name line column)
    (setf file-name (find-file-name vm (vm-program-counter vm)))
    (when (and file-name (vm-sources vm))
      (alexandria:when-let* ((source-file (find-source (vm-sources vm) file-name))
                             (content (shovel:source-file-contents source-file)))
        (setf pos (find-position file-name content (find-start-end-pos vm)))
        (when pos
          (setf line (pos-line pos))
          (setf column (pos-column pos)))))
    (error
     (if (and file-name line column)
         (make-condition 'shovel:shovel-error
                         :message message
                         :file file-name
                         :line line
                         :column column)
         (make-condition 'shovel:shovel-error :message message)))))

(defun find-required-primitive (vm name)
  (let ((primitive (gethash name *primitives*)))
    (unless primitive
      (raise-shovel-error vm (format nil "Unknown prim0 '~a'." name)))
    primitive))

(defun run-vm (bytecode &key
                          sources user-primitives state vm
                          cells-quota total-ticks-quota until-next-nap-ticks-quota)
  "Runs a Shovel VM made from BYTECODE or supplied as VM. Optionally,
the sources for the VM are supplied as SOURCES. User-defined
primitives are provided via USER-PRIMITIVES (a list containing a list
of name, CL callable, number of arguments for each primitive). If you
are resuming a serialized VM, provide the serialized state as STATE.

If you want to limit the CPU and memory usage of the Shovel process,
you can use the *-QUOTA parameters:

 * CELLS-QUOTA limits the total number of cons cells the process can
   use; this is not a hard limit, but the VM will be stopped with a
   SHOVEL-QUOTA-EXCEPTION if its memory usage is near this limit;

 * TOTAL-TICKS-QUOTA limits the total number of Shovel VM instructions
   this process can run;

 * UNTIL-NEXT-NAP-TICKS-QUOTA limits the number of ticks (executed
   Shovel VM instructions) until RUN-VM returns and sets the VM state
   to 'napping'. Use WAKE-UP-VM to revive a VM and rerun RUN-VM if you
   want to resume the VM.

Example of passing PRINT and PRIN1 as user-defined primitives:

    (run-vm bytecode
            :sources sources
            :user-primitives '((\"print\" #'print 1)
                               (\"prin1\" #'prin1 1)))

Finishes when the VM finishes or goes to sleep.

Returns two values: the top of the VM stack and the VM itself."
  (unless vm
    (setf vm (make-vm :bytecode bytecode
                      :program-counter 0
                      :current-environment nil
                      :stack nil
                      :user-primitives (make-hash-table :test #'equal)
                      :sources sources))
    (setf (vm-runs vm) (make-array (length bytecode) :initial-element nil)))
  (when cells-quota
    (setf (vm-cells-quota vm) cells-quota))
  (when total-ticks-quota
    (setf (vm-total-ticks-quota vm) total-ticks-quota))
  (when until-next-nap-ticks-quota
    (setf (vm-until-next-nap-ticks-quota vm) until-next-nap-ticks-quota))
  (when state
    (deserialize-vm-state vm state))
  (dolist (user-primitive user-primitives)
    (setf (gethash (car user-primitive) (vm-user-primitives vm))
          (rest user-primitive)))
  (setf (vm-executed-ticks-since-last-nap vm) 0)
  (handler-bind ((error (lambda (condition)
                          (setf (vm-programming-error vm) condition))))
    (let ((*error-raiser* (lambda (message)
                            (raise-shovel-error vm message)))
          (*ticks-incrementer* (lambda (ticks)
                                 (incf (vm-executed-ticks vm) ticks)
                                 (incf (vm-executed-ticks-since-last-nap vm) ticks)))
          (*cells-incrementer* (lambda (cells)
                                 (increment-cells-quota vm cells)))
          (*cells-increment-herald*
           (lambda (cells)
             (when (and (vm-cells-quota vm)
                        (> cells (vm-cells-quota vm)))
               (error (make-condition 'shovel:shovel-cell-quota-exceeded))))))
      (locally
          (declare (optimize speed))
        (loop while (step-vm vm)))
      (values (car (vm-stack vm)) vm))))

(defun get-vm-stack (vm)
  "Returns a string representation of the current stack for VM."
  (with-output-to-string (str)
    (write-stack-trace vm str)))

(defun get-vm-environment (vm)
  "Returns a string representation of the current environment for VM."
  (with-output-to-string (str)
    (write-environment (vm-current-environment vm) vm str)))

(declaim (inline vm-is-live))
(defun vm-is-live (vm)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (inline vm-execution-complete))
  (and
   (not (vm-execution-complete vm))
   (not (vm-should-take-a-nap vm))))

(declaim (inline vm-execution-complete))
(defun vm-execution-complete (vm)
  "T if the VM finished execution (not asleep, actually finished)."
  (declare (optimize speed (safety 0))
           (type vm vm))
  (= (vm-program-counter vm)
     (length (the simple-array (vm-bytecode vm)))))

(declaim (inline check-bool))
(defun check-bool (vm)
  (declare (optimize speed (safety 0))
           (type vm vm))
  (unless (shovel-vm-prim0::is-bool (car (vm-stack vm)))
    (raise-shovel-error vm "Argument must be a boolean.")))

(declaim (inline check-vm-without-error))
(defun check-vm-without-error (vm)
  (declare (optimize speed (safety 0))
           (type vm vm))
  (alexandria:when-let (err (vm-user-defined-primitive-error vm))
    (error err))
  (alexandria:when-let (err (vm-programming-error vm))
    (error err)))

(declaim (inline check-cells-quota))
(defun check-cells-quota (vm)
  (declare (optimize speed (safety 0))
           (type vm vm))
  (labels ((quota-exceeded ()
             (and (vm-cells-quota vm)
                  (> (the fixnum (vm-used-cells vm))
                     (the fixnum (vm-cells-quota vm))))))
    (when (quota-exceeded)
      (vm-count-used-cells vm)
      (when (quota-exceeded)
        (error (make-condition 'shovel:shovel-cell-quota-exceeded))))))

(defun vm-count-used-cells (vm)
  (setf (vm-used-cells vm) (count-active-objects-cells vm)))

(declaim (inline increment-cells-quota))
(defun increment-cells-quota (vm cells)
  (declare (optimize speed (safety 0)))
  (incf (the fixnum (vm-used-cells vm)) (the fixnum cells)))

(defun count-active-objects-cells (vm)
  (let ((visited  (make-hash-table :test #'eq)))
    (labels ((count-structure (structure)
               (if (gethash structure visited)
                   0
                   (cond
                     ((or (null structure)
                          (eq :null structure)
                          (eq :false structure)
                          (eq :true structure)
                          (numberp structure))
                      0)
                     ((consp structure)
                      (setf (gethash structure visited) t)
                      (+ 2
                         (count-structure (car structure))
                         (count-structure (cdr structure))))
                     ((stringp structure)
                      (setf (gethash structure visited) t)
                      (1+ (length structure)))
                     ((callable-p structure)
                      (setf (gethash structure visited) t)
                      (+ 5
                         (count-structure (callable-environment structure))
                         (count-structure (callable-prim0 structure))
                         (count-structure (callable-prim structure))))
                     ((env-frame-p structure)
                      (setf (gethash structure visited) t)
                      (+ 2
                         (count-structure (env-frame-vars structure))))
                     ((return-address-p structure)
                      (setf (gethash structure visited) t)
                      (+ 2
                         (count-structure (return-address-environment structure))))
                     ((named-block-p structure)
                      (setf (gethash structure visited) t)
                      (+ 3
                         (count-structure (named-block-name structure))
                         (count-structure (named-block-environment structure))))
                     ((vectorp structure)
                      (setf (gethash structure visited) t)
                      (let ((result (length structure)))
                        (dotimes (i (length structure))
                          (incf result (count-structure (aref structure i))))
                        (1+ result)))
                     ((hash-table-p structure)
                      (setf (gethash structure visited) t)
                      (let ((result 0))
                        (maphash (lambda (key value)
                                   (incf result 2)
                                   (incf result (count-structure key))
                                   (incf result (count-structure value)))
                                 structure)
                        (1+ result)))
                     (t (error "Shovel Internal WTF: can't count cells in structure."))))))
      (+ (count-structure (vm-current-environment vm))
         (count-structure (vm-stack vm))))))

(declaim (inline check-ticks-quota))
(defun check-ticks-quota (vm)
  (declare (type vm vm)
           (optimize (speed 1) (safety 0)))
  (when (and (vm-total-ticks-quota vm)
             (>= (vm-executed-ticks vm)
                 (vm-total-ticks-quota vm)))
    (error (make-condition 'shovel:shovel-total-ticks-quota-exceeded)))
  (when (and (vm-until-next-nap-ticks-quota vm)
             (>= (vm-executed-ticks-since-last-nap vm)
                 (vm-until-next-nap-ticks-quota vm)))
    (setf (vm-should-take-a-nap vm) t)))

(declaim (inline inc-pc))
(defun inc-pc (vm)
  (declare (optimize speed (safety 0))
           (type vm vm))
  (incf (vm-program-counter vm)))

(defun handle-jump (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (setf (vm-program-counter vm) args)))

(defun handle-const (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (push args (vm-stack vm))
    (inc-pc vm)
    ;; for the pushed value
    (increment-cells-quota vm 1)))

(defun handle-prim0 (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (unless (instruction-cache instruction)
      (setf (instruction-cache instruction)
            (make-callable :prim0 args
                           :cached-prim (cons nil (find-required-primitive vm args)))))
    (push (instruction-cache instruction)
          (vm-stack vm))
    (inc-pc vm)
    ;; for the pushed callable
    (increment-cells-quota vm 1)))

(defun handle-prim (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (unless (instruction-cache instruction)
      (setf (instruction-cache instruction)
            (make-callable :prim args
                           :cached-prim (cons nil (find-user-primitive vm args)))))
    (push (instruction-cache instruction) (vm-stack vm))
    (inc-pc vm)
    ;; for the pushed callable
    (increment-cells-quota vm 1)))

(defun handle-fjump (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (check-bool vm)
    (jump-if (shovel-vm-prim0::logical-not (pop (vm-stack vm))) vm args)))

(defun handle-lset (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (set-in-environment (vm-current-environment vm)
                        (car args) (second args)
                        (car (vm-stack vm)))
    (inc-pc vm)))

(defun handle-pop (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (ignore instruction))
  (pop (vm-stack vm))
  (inc-pc vm))

(defun handle-lget (vm instruction)
  (declare (ignore vm instruction))
  (error "Shovel Internal WTF: this should have been optimized away."))

(defun handle-fn (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (push (make-callable :program-counter (car args)
                         :environment (vm-current-environment vm)
                         :num-args (second args))
          (vm-stack vm))
    ;; for the pushed callable (5 fields)
    (increment-cells-quota vm 5)
    (inc-pc vm)))

(defun handle-new-frame (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (unless (instruction-cache instruction)
    (let* ((args (instruction-arguments instruction)))
      (declare (type list args))
      (setf (instruction-cache instruction)
            (cons (length args) args))))
  (let* ((cache (instruction-cache instruction))
         (var-names (cdr cache))
         (frame-count (the fixnum (car cache))))
    (push (make-env-frame :introduced-at-program-counter (vm-program-counter vm)
                          :var-names var-names
                          :vars (make-array frame-count :initial-element :null))
          (vm-current-environment vm))
    ;; for the frame, the saved program counter and the contents:
    (increment-cells-quota vm frame-count)
    (inc-pc vm)))

(defun handle-drop-frame (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (ignore instruction))
  (pop (vm-current-environment vm))
  (inc-pc vm))

(defun handle-return (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (ignore instruction))
  (let ((other-stack (cddr (vm-stack vm)))
        (result (car (vm-stack vm)))
        (retaddr (second (vm-stack vm))))
    (apply-return-address vm retaddr)
    (setf (vm-stack vm)
          (cons result other-stack))))

(defun handle-block (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((name (pop (vm-stack vm)))
        (args (instruction-arguments instruction)))
    (unless (stringp name)
      (raise-shovel-error vm "The name of a block must be a string."))
    (push (make-named-block :name name
                            :end-address args
                            :environment (vm-current-environment vm))
          (vm-stack vm)))
  (increment-cells-quota vm 3) ;; for the named block record (3 fields)
  (inc-pc vm))

(defun handle-pop-block (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (ignore instruction))
  (let ((return-value (car (vm-stack vm)))
        (named-block (second (vm-stack vm)))
        (rest-of-the-stack (cddr (vm-stack vm))))
    (unless (named-block-p named-block)
      (raise-shovel-error vm "Invalid context for POP_BLOCK."))
    (setf (vm-stack vm) (cons return-value rest-of-the-stack)))
  (inc-pc vm))

(defun handle-block-return (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (ignore instruction))
  (let ((return-value (car (vm-stack vm)))
        (name (second (vm-stack vm))))
    (unless (stringp name)
      (raise-shovel-error vm "The name of a block must be a string."))
    (multiple-value-bind (named-block stack-below)
        (find-named-block vm (vm-stack vm) name)
      (setf (vm-stack vm) (list* return-value
                                 named-block
                                 stack-below))
      (setf (vm-current-environment vm)
            (named-block-environment named-block))
      (setf (vm-program-counter vm)
            (named-block-end-address named-block)))))

(defun handle-context (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (ignore instruction))
  (let ((stack-trace (with-output-to-string (str)
                       (write-stack-trace vm str)))
        (current-environment (with-output-to-string (str)
                               (write-environment (vm-current-environment vm)
                                                  vm str)))
        (context (make-hash-table :test #'equal)))
    (setf (gethash "stack" context) stack-trace)
    (setf (gethash "environment" context) current-environment)
    (push context (vm-stack vm))
    ;; for the pushed hash: 1 (the pushed hash), 1 (the hash), 2 keys
    ;; and 2 values, the length of the strings.
    (increment-cells-quota vm (+ 6 (length stack-trace) (length current-environment)))
    (inc-pc vm)))

(defun handle-tjump (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (check-bool vm)
    (jump-if (pop (vm-stack vm)) vm args)))

(defun handle-nop (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (ignore instruction))
  (inc-pc vm))

(defun handle-call (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (handle-call-impl vm args t)))

(defun handle-callj (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (handle-call-impl vm args nil)))

(defun handle-args (vm instruction)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type instruction instruction))
  (let ((args (instruction-arguments instruction)))
    (declare (type fixnum args))
    (when (< 0 args)
      (let* ((stack (vm-stack vm))
             (return-address (if (return-address-p (car stack)) (pop stack)))
             (environment (vm-current-environment vm)))
        (loop
           for i from (1- args) downto 0
           do (set-in-top-frame environment i (pop stack)))
        (if return-address
            (setf (vm-stack vm) (cons return-address stack))
            (setf (vm-stack vm) stack)))))
  (inc-pc vm))

(defparameter *instruction-handlers*
  (vector #'handle-jump
          #'handle-const
          #'handle-prim0
          #'handle-prim
          #'handle-call
          #'handle-callj
          #'handle-fjump
          #'handle-lset
          #'handle-pop
          #'handle-lget
          #'handle-fn
          #'handle-new-frame
          #'handle-drop-frame
          #'handle-args
          #'handle-return
          #'handle-block
          #'handle-pop-block
          #'handle-block-return
          #'handle-context
          #'handle-tjump
          #'handle-nop))

(declaim (inline step-vm))
(defun step-vm (vm)
  (declare (optimize speed (safety 0))
           (type vm vm))
  (check-vm-without-error vm)
  (check-ticks-quota vm)
  (check-cells-quota vm)
  (when (vm-is-live vm)
    (let ((pc (vm-program-counter vm)))
      (let ((run (aref (the (simple-array t *) (vm-runs vm)) pc)))
        (unless run
          (setf run (compute-vm-run vm pc)
                (aref (the (simple-array t *) (vm-runs vm)) pc) run))
        (locally (declare (type (function (vm) t) run))
          (funcall run vm)))))
  (vm-is-live vm))

(defparameter *prim0-run-stoppers* (list "arrayN" "stringRepresentation" "svm_set_indexed"))

(defun process-run (run)
  (declare (optimize speed (safety 0)))
  (let ((lget-hash (make-hash-table :test #'equal))
        lgets
        (max-frame-number nil)
        (max-var-index nil))
    (loop
       for instruction in run
       when (eq :lget (instruction-opcode instruction))
       do (let ((args (instruction-arguments instruction)))
            (unless (gethash args lget-hash)
              (setf (gethash args lget-hash) instruction)
              (let ((frame-number (first args))
                    (var-index (second args)))
                (declare (type fixnum frame-number var-index))
                (when (or (not max-frame-number) (< max-frame-number frame-number))
                  (setf max-frame-number frame-number))
                (when (or (not max-var-index) (< max-var-index var-index))
                  (setf max-var-index var-index))))))
    (maphash (lambda (k v)
               (declare (ignore k)
                        (type instruction v))
               (let ((args (instruction-arguments v)))
                 (push (cons (first args) (second args)) lgets)))
             lget-hash)
    ;; (format t "lgets ~a (~a ~a)~%" lgets max-frame-number max-var-index)
    (let ((var-array (if max-frame-number
                         (make-array (1+ max-frame-number) :initial-element nil))))
      (when var-array
        (dolist (lget lgets)
          (let ((frame-number (car lget)))
            (declare (type fixnum frame-number))
            (setf (aref var-array frame-number)
                  (make-array (1+ max-var-index) :initial-element nil)))))
      (labels ((calculate-vars (vm)
                 (declare (optimize speed (safety 0))
                          (type vm vm))
                 ;; (print "calculating vars")
                 (let ((env (vm-current-environment vm)))
                   (dolist (lget lgets)
                     (let ((frame-number (car lget))
                           (var-index (cdr lget)))
                       (declare (type fixnum frame-number var-index))
                       (let ((frame (aref var-array frame-number)))
                         (declare (type (simple-array t *) frame))
                         (setf (aref frame var-index)
                               (get-from-environment env frame-number var-index))))))))
        (let* ((funs
                (mapcar (lambda (instruction)
                          (declare (optimize speed (safety 0))
                                   (type instruction instruction)
                                   (type (simple-array t *) *instruction-handlers*))
                          (let ((opcode (instruction-opcode instruction)))
                            (if (eq :lget opcode)
                                (let* ((args (instruction-arguments instruction))
                                       (frame-number (first args))
                                       (var-index (second args)))
                                  (declare (type fixnum frame-number var-index))
                                  (lambda (vm)
                                    (declare (optimize speed (safety 0))
                                             (type vm vm))
                                    (let ((frame (aref var-array frame-number)))
                                      (declare (type (simple-array t *) frame))
                                      (push (aref frame var-index)
                                            (vm-stack vm)))
                                    ;; for the pushed value
                                    (increment-cells-quota vm 1)
                                    (inc-pc vm)))
                                (let ((opcode-num (instruction-opcode-num instruction)))
                                  (declare (type fixnum opcode-num))
                                  (when (or (< opcode-num 0) (> opcode-num 20))
                                    (error "Shovel Internal WTF: broken instruction."))
                                  (let ((instruction-handler (aref *instruction-handlers* opcode-num)))
                                    (declare (type (function (vm instruction) t) instruction-handler))
                                    (lambda (vm)
                                      (declare (optimize speed (safety 0))
                                               (type vm vm))
                                      ;; (print opcode)
                                      (funcall instruction-handler vm instruction)))))))
                        run))
               (fun-count (length funs)))
          (labels ((execute-run (vm)
                     (declare (optimize speed (safety 0)))
                     ;; (print ">>>> start run")
                     (dolist (fun funs)
                       (declare (type (function (vm) t) fun))
                       (funcall fun vm))
                     ;; (print ">>>> end run")
                     (locally (declare (optimize (speed 1)))
                       (increment-vm-ticks vm fun-count))))
            (if var-array
                (lambda (vm)
                  (declare (optimize speed (safety 0))
                           (type vm vm))
                  (calculate-vars vm)
                  (execute-run vm))
                #'execute-run)))))))

(defun compute-vm-run (vm pc)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type fixnum pc))
  (let ((max-pc (1- (length (the simple-array (vm-bytecode vm)))))
        (last-opcode)
        (last-prim0))
    (labels ((get-instruction (pc)
               (declare (optimize speed)
                        (type fixnum pc))
               (aref (the (simple-array instruction *) (vm-bytecode vm)) pc))
             (process-instruction (pc)
               (declare (optimize speed (safety 0))
                        (type fixnum pc))
               (let ((instruction (get-instruction pc)))
                 (declare (type instruction instruction))
                 (let ((opcode (the symbol (instruction-opcode instruction))))
                   (when (eq opcode :prim0)
                     (setf last-prim0 (instruction-arguments instruction)))
                   ;; (format t "op = ~a lastop = ~a last-prim0 = ~a~%"
                   ;;         opcode last-opcode last-prim0)
                   (let ((stop (or (and (eq :call opcode)
                                        (not (eq :prim0 last-opcode)))
                                   (= pc max-pc)
                                   (member opcode
                                           (list :lset :jump :fjump :tjump
                                                 :drop-frame :args
                                                 :return :callj :block-return))
                                   (and (eq :new-frame opcode)
                                        (not (eq :args (instruction-opcode (the instruction
                                                                             (get-instruction (1+ pc)))))))
                                   (and (eq :call opcode)
                                        (eq :prim0 last-opcode)
                                        (stringp last-prim0)
                                        (member last-prim0 *prim0-run-stoppers* :test #'string=)))))
                     (setf last-opcode opcode)
                     (values instruction stop)))))
             (accumulate (pc &optional acc)
               (multiple-value-bind (func stop)
                   (process-instruction pc)
                 (if stop
                     (nreverse (cons func acc))
                     (accumulate (the fixnum (incf (the fixnum pc)))
                                 (cons func acc))))))
      (process-run (accumulate pc)))))

(declaim (inline increment-vm-ticks))
(defun increment-vm-ticks (vm inc)
  (declare (type vm vm))
  (incf (vm-executed-ticks vm) inc)
  (incf (vm-executed-ticks-since-last-nap vm) inc))

(defun find-named-block (vm stack block-name)
  (cond ((null stack)
         (raise-shovel-error vm (format nil "Cannot find block '~a'." block-name)))
        ((and (named-block-p (car stack))
              (string= block-name (named-block-name (car stack))))
         (values (car stack) (rest stack)))
        (t (find-named-block vm (rest stack) block-name))))

(defun find-user-primitive (vm primitive-name)
  (or (gethash primitive-name (vm-user-primitives vm))
      (raise-shovel-error vm
                          (format nil "Unknown user primitive '~a'."
                                  primitive-name))))

(defun jump-if (value vm jump-address)
  (declare (optimize speed (safety 0))
           (type vm vm))
  (if (shovel-vm-prim0::is-true value)
      (setf (vm-program-counter vm) jump-address)
      (incf (vm-program-counter vm))))

(defun apply-return-address (vm retaddr)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (return-address retaddr))
  (setf (vm-program-counter vm) (return-address-program-counter retaddr)
        (vm-current-environment vm) (return-address-environment retaddr)))

(defun handle-call-impl (vm num-args save-return-address)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type fixnum num-args))
  (let ((callable (pop (vm-stack vm))))
    (unless (callable-p callable)
      (raise-shovel-error vm (format nil "Object [~a] is not callable."
                                     (shovel-vm-prim0::shovel-string-representation
                                      callable))))
    (locally (declare (type callable callable))
      (if (or (callable-prim callable) (callable-prim0 callable))
          (call-primitive callable vm num-args save-return-address)
          (call-function callable vm num-args save-return-address))))
  (when save-return-address
    ;; for the pushed return address:
    (increment-cells-quota vm 1)))

(defun arity-error (vm expected-arity actual-arity)
  (raise-shovel-error
   vm (format nil "Function of ~d arguments called with ~d arguments."
              expected-arity actual-arity)))

(declaim (inline call-function))
(defun call-function (callable vm num-args save-return-address)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type callable callable)
           (type fixnum num-args))
  (when save-return-address
    (push (make-return-address :program-counter (the fixnum (1+ (vm-program-counter vm)))
                               :environment (vm-current-environment vm))
          (vm-stack vm)))
  (when (and (callable-num-args callable)
             (/= (the fixnum (callable-num-args callable))
                 (the fixnum num-args)))
    (arity-error vm (callable-num-args callable) num-args))
  (setf (vm-program-counter vm) (callable-program-counter callable)
        (vm-current-environment vm) (callable-environment callable)))

(defun is-shovel-type (data)
  (cond ((or (stringp data) (numberp data)
             (eq :true data) (eq :false data) (eq :null data)
             (callable-p data))
         t)
        ((vectorp data)
         (dotimes (i (length data))
           (unless (is-shovel-type (aref data i))
             (return-from is-shovel-type nil)))
         t)
        ((hash-table-p data)
         (maphash (lambda (key value)
                    (unless (and (stringp key) (is-shovel-type value))
                      (return-from is-shovel-type nil)))
                  data)
         t)))

(defun reversed-prefix (list n &optional acc)
  (declare (optimize speed (safety 0))
           (type fixnum n))
  (if (= n 0) (values acc list)
      (reversed-prefix (rest list) (the fixnum (1- n)) (cons (car list) acc))))

(defun finish-calling-required-primitive (primitive arg-values
                                          save-return-address new-stack
                                          vm)
  (let ((result (apply primitive arg-values)))
    (finish-primitive-call vm result
                           save-return-address new-stack)))

(defun finish-calling-required-binary-primitive (primitive arg1 arg2
                                                 save-return-address new-stack
                                                 vm)
  (declare (optimize speed (safety 0))
           (type (function (t t) t) primitive))
  (let ((result (funcall primitive arg1 arg2)))
    (finish-primitive-call vm result
                           save-return-address new-stack)))

(defun finish-calling-user-defined-primitive (primitive arg-values
                                              current-program-counter callable
                                              save-return-address new-stack
                                              vm)
  (multiple-value-bind(result what-next)
      (handler-case
          (apply primitive arg-values)
        (error (err)
          (setf (vm-user-defined-primitive-error vm) err)
          (values :null :nap-and-retry-on-wake-up)))
    (unless (is-shovel-type result)
      (raise-shovel-error
       vm
       (format nil "User defined primitive returned invalid value (~a).

A 'valid value' (with Common Lisp as the host language) is:

 * :null, :true or :false;
 * a string;
 * a number;
 * an array of elements that are themselves valid values;
 * a hash with strings as keys and valid values.
"
               result)))
    (let (should-finish-this-call)
      (cond ((or (null what-next)
                 (eq :continue what-next))
             (setf should-finish-this-call t))
            ((eq :nap what-next)
             (setf (vm-should-take-a-nap vm) t)
             (setf should-finish-this-call t))
            ((eq :nap-and-retry-on-wake-up what-next)
             (setf (vm-should-take-a-nap vm) t)
             (setf (vm-program-counter vm) current-program-counter)
             (push callable (vm-stack vm)))
            (t (raise-shovel-error
                vm
                "A user-defined primitive returned an unknown second value.")))
      (when should-finish-this-call
        (finish-primitive-call vm result
                               save-return-address new-stack)))))

(declaim (inline finish-primitive-call))
(defun finish-primitive-call (vm result save-return-address new-stack)
  (declare (optimize speed (safety 0))
           (type vm vm))
  (setf (vm-stack vm) new-stack)
  (if save-return-address
      (incf (vm-program-counter vm))
      (apply-return-address vm (pop (vm-stack vm))))
  (increment-cells-quota vm 1)
  (push result (vm-stack vm)))

(defmacro handle-generic-arity (is-required-primitive)
  `(multiple-value-bind (arg-values new-stack)
       (reversed-prefix (vm-stack vm) num-args)
     (let* ((primitive-record (callable-cached-prim callable))
            (primitive (second primitive-record))
            (primitive-arity (third primitive-record)))
       (declare (type (or fixnum null) num-args primitive-arity))
       (when (and primitive-arity (/= primitive-arity num-args))
         (arity-error vm primitive-arity num-args))
       ,(if is-required-primitive
            '(finish-calling-required-primitive
              primitive arg-values save-return-address
              new-stack vm)
            '(finish-calling-user-defined-primitive
              primitive arg-values
              (vm-program-counter vm) callable
              save-return-address new-stack vm)))))

(defun handle-prim0-generic-arity (vm callable num-args save-return-address)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type callable callable)
           (type fixnum num-args))
  (handle-generic-arity t))

(defun handle-prim-generic-arity (vm callable num-args save-return-address)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type callable callable)
           (type fixnum num-args))
  (handle-generic-arity nil))

(defmacro handle-binary-arity (is-required-primitive)
  `(progn
     (when (/= 2 num-args)
       (arity-error vm 2 num-args))
     (let* ((primitive-record (callable-cached-prim callable))
            (primitive (second primitive-record))
            (stack (vm-stack vm))
            (arg2 (car stack))
            (arg1 (second stack))
            (new-stack (cddr stack)))
       ,(if is-required-primitive
            '(finish-calling-required-binary-primitive
              primitive arg1 arg2 save-return-address
              new-stack vm)
            '(finish-calling-user-defined-primitive
              primitive (list arg1 arg2)
              (vm-program-counter vm) callable
              save-return-address new-stack vm)))))

(defun handle-prim0-binary-arity (vm callable num-args save-return-address)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type callable callable)
           (type fixnum num-args))
  (handle-binary-arity t))

(defun handle-prim-binary-arity (vm callable num-args save-return-address)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type callable callable)
           (type fixnum num-args))
  (handle-binary-arity nil))

(declaim (inline call-primitive))
(defun call-primitive (callable vm num-args save-return-address)
  (declare (optimize speed (safety 0))
           (type vm vm)
           (type callable callable)
           (type fixnum num-args))
  (unless (callable-cached-prim callable)
    (setf (callable-cached-prim callable)
          (cons nil
                (or (alexandria:if-let (prim0 (callable-prim0 callable))
                      (find-required-primitive vm prim0))
                    (alexandria:if-let (prim (callable-prim callable))
                      (find-user-primitive vm prim))))))
  (let* ((cache (callable-cached-prim callable))
         (arity (third cache))
         (is-required-primitive (callable-prim0 callable)))
    (declare (type (or null fixnum) arity))
    (unless (car cache)
      (setf (car cache)
            (if is-required-primitive
                (if (and arity (= 2 arity))
                    #'handle-prim0-binary-arity
                    #'handle-prim0-generic-arity)
                (if (and arity (= 2 arity))
                    #'handle-prim-binary-arity
                    #'handle-prim-generic-arity))))
    (funcall (the (function (vm callable fixnum t) t)
               (car cache))
             vm callable num-args save-return-address)))

(declaim (inline set-in-top-frame))
(defun set-in-top-frame (environment var-index value)
  (declare (optimize speed (safety 0)))
  (let ((frame (car environment)))
    (declare (type env-frame frame))
    (let ((vars (env-frame-vars frame)))
      (declare (type (simple-array t *) vars))
      (setf (aref vars var-index) value))))

(declaim (inline set-in-environment))
(defun set-in-environment (environment frame-number var-index value)
  (declare (optimize speed (safety 0))
           (type fixnum frame-number var-index))
  (let ((frame (nth frame-number environment)))
    (declare (type env-frame frame))
    (let ((frame-vars (env-frame-vars frame)))
      (declare (type (simple-array t *) frame-vars))
      (setf (aref frame-vars var-index) value))))

(declaim (inline get-from-environment))
(defun get-from-environment (environment frame-number var-index)
  (declare (optimize speed (safety 0))
           (type fixnum frame-number var-index)
           (type list environment))
  (let ((frame (nth frame-number environment)))
    (declare (type env-frame frame))
    (let ((frame-vars (env-frame-vars frame)))
      (declare (type (simple-array t *) frame-vars))
      (aref frame-vars var-index))))

(defstruct serializer-state (hash (make-hash-table)) (array nil))

(defmacro get-serialization-code (symbol)
  (case symbol
    (:host-null 1)
    (:true 2)
    (:false 3)
    (:guest-null 4)
    (:cons 5)
    (:array 6)
    (:hash 7)
    (:callable 8)
    (:return-address 9)
    (:env-frame 10)
    (:named-block 11)
    (:simple-array 12)))

(defun serialize (object ss)
  (labels ((store-one (obj &key (store-as obj))
             (let ((new-index (length (serializer-state-array ss))))
               (setf (gethash store-as (serializer-state-hash ss)) new-index)
               (push obj (serializer-state-array ss))
               new-index))
           (make-array-1 (value)
             (make-array 1 :initial-element value)))
    (alexandria:if-let (object-index (gethash object (serializer-state-hash ss)))
      object-index
      (cond ((or (stringp object) (numberp object)) (store-one object))
            ((null object) (store-one (make-array-1
                                       (get-serialization-code :host-null))
                                      :store-as object))
            ((eq :true object) (store-one (make-array-1
                                           (get-serialization-code :true))
                                          :store-as object))
            ((eq :false object) (store-one (make-array-1
                                            (get-serialization-code :false))
                                           :store-as object))
            ((eq :null object) (store-one (make-array-1
                                           (get-serialization-code :guest-null))
                                          :store-as object))
            ((consp object)
             (let* ((result-array (make-array 3))
                    (result (store-one result-array :store-as object)))
               (setf (aref result-array 0) (get-serialization-code :cons))
               (let* ((car-index (serialize (car object) ss))
                      (cdr-index (serialize (cdr object) ss)))
                 (setf (aref result-array 1) car-index)
                 (setf (aref result-array 2) cdr-index))
               result))
            ((vectorp object)
             (let* ((result-array (make-array (1+ (length object))))
                    (result (store-one result-array :store-as object)))
               (setf (aref result-array 0)
                     (if (typep object '(simple-array t *))
                         (get-serialization-code :simple-array)
                         (get-serialization-code :array)))
               (loop
                  for i from 0 to (1- (length object))
                  do (setf (aref result-array (1+ i))
                           (serialize (aref object i) ss)))
               result))
            ((hash-table-p object)
             (let* ((result-array (make-array (1+ (* 2 (hash-table-count object)))))
                    (i 0)
                    (result (store-one result-array :store-as object)))
               (setf (aref result-array i) (get-serialization-code :hash))
               (incf i)
               (maphash (lambda (key value)
                          (setf (aref result-array i) (serialize key ss))
                          (incf i)
                          (setf (aref result-array i) (serialize value ss))
                          (incf i))
                        object)
               result))
            ((callable-p object)
             (let* ((result-array (make-array 6))
                    (result (store-one result-array :store-as object)))
               (setf (aref result-array 0) (get-serialization-code :callable))
               (setf (aref result-array 1) (serialize (callable-prim0 object) ss))
               (setf (aref result-array 2) (serialize (callable-prim object) ss))
               (setf (aref result-array 3)
                     (serialize (callable-num-args object) ss))
               (setf (aref result-array 4)
                     (serialize (callable-program-counter object) ss))
               (setf (aref result-array 5)
                     (serialize (callable-environment object) ss))
               result))
            ((return-address-p object)
             (let* ((result-array (make-array 3))
                    (result (store-one result-array :store-as object)))
               (setf (aref result-array 0) (get-serialization-code :return-address))
               (setf (aref result-array 1)
                     (serialize (return-address-program-counter object) ss))
               (setf (aref result-array 2)
                     (serialize (return-address-environment object) ss))
               result))
            ((env-frame-p object)
             (let* ((result-array (make-array 4))
                    (result (store-one result-array :store-as object)))
               (setf (aref result-array 0) (get-serialization-code :env-frame))
               (setf (aref result-array 1)
                     (serialize (env-frame-introduced-at-program-counter object)
                                ss))
               (setf (aref result-array 2) (serialize (env-frame-var-names object) ss))
               (setf (aref result-array 3) (serialize (env-frame-vars object) ss))
               result))
            ((named-block-p object)
             (let* ((result-array (make-array 4))
                    (result (store-one result-array :store-as object)))
               (setf (aref result-array 0) (get-serialization-code :named-block))
               (setf (aref result-array 1)
                     (serialize (named-block-name object) ss))
               (setf (aref result-array 2)
                     (serialize (named-block-end-address object) ss))
               (setf (aref result-array 3)
                     (serialize (named-block-environment object) ss))
               result))
            (t (error "Internal error: Don't know how to serialize object!"))))))

(defstruct deserializer-state (array nil) (objects nil))

(defun deserialize (index ds)
  (let ((serialized-object (aref (deserializer-state-array ds) index))
        (deserialize-error
         "Internal error: Don't know how to deserialize object!"))
    (if (or (stringp serialized-object)
            (numberp serialized-object))
        serialized-object
        (symbol-macrolet ((object-ref (aref (deserializer-state-objects ds) index)))
          (alexandria:if-let (object object-ref)
            object
            (cond ((consp serialized-object)
                   (let ((result (cons nil nil)))
                     (setf object-ref result)
                     (setf (car result) (deserialize (car serialized-object) ds))
                     (setf (cdr result) (deserialize (cdr serialized-object) ds))
                     result))
                  ((vectorp serialized-object)
                   (if (= 0 (length serialized-object))
                       (error deserialize-error))
                   (let ((code (aref serialized-object 0)))
                     (cond
                       ((= code (get-serialization-code :host-null)) nil)
                       ((= code (get-serialization-code :guest-null)) :null)
                       ((= code (get-serialization-code :true)) :true)
                       ((= code (get-serialization-code :false)) :false)
                       ((= code (get-serialization-code :cons))
                        (let ((result (cons nil nil)))
                          (setf object-ref result)
                          (setf (car result)
                                (deserialize (aref serialized-object 1) ds))
                          (setf (cdr result)
                                (deserialize (aref serialized-object 2) ds))
                          result))
                       ((or (= code (get-serialization-code :array))
                            (= code (get-serialization-code :simple-array)))
                        (let* ((n (1- (length serialized-object)))
                               (result (if (= code (get-serialization-code :array))
                                           (make-array n :adjustable t :fill-pointer n)
                                           (make-array n))))
                          (setf object-ref result)
                          (loop
                             for i from 0 to (1- (length result))
                             do (setf (aref result i)
                                      (deserialize (aref serialized-object (1+ i))
                                                   ds)))
                          result))
                       ((= code (get-serialization-code :hash))
                        (let ((result (make-hash-table :test #'equal)))
                          (setf object-ref result)
                          (loop
                             for i from 0 to (- (length serialized-object) 2) by 2
                             do (let ((key (deserialize
                                            (aref serialized-object (1+ i)) ds))
                                      (value (deserialize
                                              (aref serialized-object (+ 2 i)) ds)))
                                  (setf (gethash key result) value)))
                          result))
                       ((= code (get-serialization-code :callable))
                        (let ((result (make-callable)))
                          (setf object-ref result)
                          (setf (callable-prim0 result)
                                (deserialize (aref serialized-object 1) ds))
                          (setf (callable-prim result)
                                (deserialize (aref serialized-object 2) ds))
                          (setf (callable-num-args result)
                                (deserialize (aref serialized-object 3) ds))
                          (setf (callable-program-counter result)
                                (deserialize (aref serialized-object 4) ds))
                          (setf (callable-environment result)
                                (deserialize (aref serialized-object 5) ds))
                          result))
                       ((= code (get-serialization-code :return-address))
                        (let ((result (make-return-address)))
                          (setf object-ref result)
                          (setf (return-address-program-counter result)
                                (deserialize (aref serialized-object 1) ds))
                          (setf (return-address-environment result)
                                (deserialize (aref serialized-object 2) ds))
                          result))
                       ((= code (get-serialization-code :env-frame))
                        (let ((result (make-env-frame
                                       :introduced-at-program-counter nil
                                       :vars nil)))
                          (setf object-ref result)
                          (setf (env-frame-introduced-at-program-counter result)
                                (deserialize (aref serialized-object 1) ds))
                          (setf (env-frame-var-names result)
                                (deserialize (aref serialized-object 2) ds))
                          (setf (env-frame-vars result)
                                (deserialize (aref serialized-object 3) ds))
                          result))
                       ((= code (get-serialization-code :named-block))
                        (let ((result (make-named-block
                                       :name nil
                                       :end-address nil
                                       :environment nil)))
                          (setf object-ref result)
                          (setf (named-block-name result)
                                (deserialize (aref serialized-object 1) ds))
                          (setf (named-block-end-address result)
                                (deserialize (aref serialized-object 2) ds))
                          (setf (named-block-environment result)
                                (deserialize (aref serialized-object 3) ds))
                          result))
                       (t (error deserialize-error)))))
                  (t
                   (error deserialize-error))))))))

(defun get-vm-arguments-for-opcode (vm opcode)
  (dotimes (i (length (vm-bytecode vm)))
    (let ((instruction (aref (vm-bytecode vm) i)))
      (when (eq opcode (instruction-opcode instruction))
        (alexandria:when-let (result (instruction-arguments instruction))
          (return-from get-vm-arguments-for-opcode result)))))
  (error "Shovel Internal WTF: VM without ~a." opcode))

(defun get-vm-version (vm)
  (get-vm-arguments-for-opcode vm :vm-version))

(defun get-vm-bytecode-md5 (vm)
  (let ((result (get-vm-arguments-for-opcode vm :vm-bytecode-md5)))
    (when (string= "?" result)
      (error "Shovel Internal WTF: VM without bytecode MD5 hash."))
    result))

(defun get-vm-sources-md5 (vm)
  (get-vm-arguments-for-opcode vm :vm-sources-md5))

(defun serialize-vm-state (vm)
  "Serializes the state of this VM to an array of bytes."
  (check-vm-without-error vm)
  (let ((ss (make-serializer-state))
        stack current-environment program-counter)
    (setf stack (serialize (vm-stack vm) ss))
    (setf current-environment (serialize (vm-current-environment vm) ss))
    (setf program-counter (serialize (vm-program-counter vm) ss))
    (let ((serialized-state (list stack current-environment program-counter
                                  (nreverse (serializer-state-array ss))
                                  (get-vm-version vm)
                                  (get-vm-bytecode-md5 vm)
                                  (get-vm-sources-md5 vm)
                                  (vm-executed-ticks vm)
                                  (vm-used-cells vm))))
      (messagepack-encode-with-md5-checksum serialized-state))))

(defun deserialize-vm-state (vm state-bytes)
  (let* ((vm-state (check-md5-checksum-and-messagepack-decode state-bytes))
         (stack-index (aref vm-state 0))
         (current-environment-index (aref vm-state 1))
         (program-counter-index (aref vm-state 2))
         (array (aref vm-state 3))
         (vm-version (aref vm-state 4))
         (vm-bytecode-md5 (aref vm-state 5))
         (vm-executed-ticks (aref vm-state 7))
         (vm-used-cells (aref vm-state 8))
         (ds (make-deserializer-state :array array
                                      :objects (make-array (length array)
                                                           :initial-element nil))))
    (unless (= vm-version (get-vm-version vm))
      (error (make-condition
              'shovel:shovel-vm-match-error
              :message "VM version and serialized VM version do not match.")))
    (unless (string= vm-bytecode-md5 (get-vm-bytecode-md5 vm))
      (error (make-condition
              'shovel:shovel-vm-match-error
              :message
              "VM bytecode MD5 and serialized VM bytecode MD5 do not match.")))
    (setf (vm-executed-ticks vm) vm-executed-ticks)
    (setf (vm-used-cells vm) vm-used-cells)
    (setf (vm-stack vm) (deserialize stack-index ds))
    (setf (vm-current-environment vm) (deserialize current-environment-index ds))
    (setf (vm-program-counter vm) (deserialize program-counter-index ds))))

(defun vm-used-ticks (vm)
  "The number of ticks used by VM so far."
  (vm-executed-ticks vm))

(defun vm-really-used-cells (vm)
  (vm-count-used-cells vm)
  (vm-used-cells vm))
