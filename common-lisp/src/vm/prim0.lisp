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

(in-package #:shovel-vm-prim0)

;; Arithmetic operators:

(defun vm-error (message)
  (funcall shovel-vm::*error-raiser* message))

(defmacro both-numbers-or-strings-or-arrays (op-numbers op-strings op-arrays)
  `(cond ((and (numberp t1) (numberp t2))
          (,op-numbers t1 t2))
         ((and (stringp t1) (stringp t2))
          (,op-strings t1 t2))
         ((and (vectorp t1) (vectorp t2))
          (,op-arrays t1 t2))
         (t
          (vm-error
           "Arguments must have the same type (numbers or strings or arrays)."))))
(defun limit-integer-result (result)
  (if (integerp result)
      (rem result (expt 2 60))
      result))
(defun add (t1 t2)
  (labels ((strcat (t1 t2) (concatenate 'string t1 t2))
           (array-cat (t1 t2) (concatenate 'vector t1 t2)))
    (limit-integer-result
     (both-numbers-or-strings-or-arrays + strcat array-cat))))
(defmacro both-numbers (op)
  `(cond ((and (numberp t1) (numberp t2))
          (,op t1 t2))
         (t (vm-error "Both arguments must be numbers."))))
(defun subtract (t1 t2) (limit-integer-result (both-numbers -)))
(defun unary-minus (t1)
  (if (numberp t1)
      (limit-integer-result (- t1))
      (vm-error "Argument must be number.")))
(defun multiply (t1 t2) (limit-integer-result (both-numbers *)))
(defun shift-left (t1 t2)
  (labels ((shl (t1 t2) (ash t1 t2)))
    (limit-integer-result (both-numbers shl))))
(defun shift-right (t1 t2)
  (labels ((shr (t1 t2) (ash t1 (- t2))))
    (limit-integer-result (both-numbers shr))))
(defun divide (t1 t2)
  (cond ((and (integerp t1) (integerp t2))
         (limit-integer-result (floor (/ t1 t2))))
        ((and (numberp t1) (numberp t2))
         (float (/ t1 t2)))
        (t (vm-error "Both arguments must be numbers."))))
(defmacro both-integers (op)
  `(cond ((and (integerp t1) (integerp t2))
          (,op t1 t2))
         (t (vm-error "Both arguments must be integers."))))
(defun modulo (t1 t2)
  (limit-integer-result (both-integers mod)))
(defun pow (t1 t2)
  (limit-integer-result (both-numbers expt)))

;; Logic operators:
(defun is-bool (var)
  (or (eq var :true)
      (eq var :false)))

(defun make-bool (var)
  (if var :true :false))

(defun is-true (var)
  (declare (optimize speed))
  (eq :true var))

(defmacro both-bools (op)
  `(cond ((and (is-bool t1) (is-bool t2))
          (,op t1 t2))
         (t (vm-error "Both arguments must be booleans."))))

(defun logical-and (t1 t2)
  (labels ((and-op (t1 t2) (and (eq :true t1) (eq :true t2))))
    (make-bool (both-bools and-op))))

(defun logical-or (t1 t2)
  (labels ((or-op (t1 t2) (or (eq :true t1) (eq :true t2))))
    (make-bool (both-bools or-op))))

(defun logical-not (t1)
  (if (is-bool t1)
      (if (eq :true t1) :false :true)
      (vm-error "Argument must be boolean.")))

;; Relational operators:
(defmacro both-numbers-or-strings (op-numbers op-strings)
  `(cond ((and (numberp t1) (numberp t2))
          (,op-numbers t1 t2))
         ((and (stringp t1) (stringp t2))
          (,op-strings t1 t2))
         (t (vm-error "Arguments must have the same type (numbers or strings)."))))
(defun less-than (t1 t2) (make-bool (both-numbers-or-strings < string<)))
(defun less-than-or-equal (t1 t2) (make-bool (both-numbers-or-strings <= string<=)))
(defun greater-than (t1 t2) (make-bool (both-numbers-or-strings > string>)))
(defun greater-than-or-equal (t1 t2)
  (make-bool (both-numbers-or-strings >= string>=)))
(defun are-equal (t1 t2)
  (cond
    ((and (eq :null t1) (eq :null t2)) :true)
    ((or (eq :null t1) (eq :null t2)) :false)
    ((and (numberp t1) (numberp t2)) (make-bool (= t1 t2)))
    ((and (stringp t1) (stringp t2)) (make-bool (string= t1 t2)))
    ((and (is-bool t1) (is-bool t2)) (make-bool (eq t1 t2)))
    ((and (vectorp t1) (vectorp t2)) (make-bool (eq t1 t2)))
    ((and (hash-table-p t1) (hash-table-p t2)) (make-bool (eq t1 t2)))
    (t (vm-error "At least one of the arguments must be null or they must have the same type (numbers, strings, booleans, hashes or arrays)."))))
(defun are-not-equal (t1 t2) (logical-not (are-equal t1 t2)))

;; Bitwise operators:
(defun bitwise-and (t1 t2)
  (limit-integer-result (both-integers logand)))
(defun bitwise-or (t1 t2)
  (limit-integer-result (both-integers logior)))
(defun bitwise-xor (t1 t2)
  (limit-integer-result (both-integers logxor)))

;; Hash constructor:
(defun hash-constructor (&rest args)
  (when (/= 0 (mod (length args) 2))
    (vm-error "Must provide an even number of arguments."))
  (funcall shovel-vm::*cells-increment-herald* (+ 1 (* 2 (length args))))
  (let ((result (make-hash-table :test #'equal)))
    (loop
       while args
       do (let ((key (first args))
                (value (second args)))
            (when (not (stringp key))
              (vm-error "Keys must be strings"))
            (setf (gethash key result) value)
            (setf args (cddr args))))
    (funcall shovel-vm::*cells-incrementer* (+ 1 (* 2 (length args))))
    result))

;; Array constructor:
(defun array-constructor (&rest args)
  (funcall shovel-vm::*cells-increment-herald* (+ 1 (length args)))
  (let* ((vectorized-args (coerce args 'vector))
         (n (length vectorized-args))
         (result (make-array n
                             :initial-element :null
                             :adjustable t
                             :fill-pointer n)))
    (replace result vectorized-args)
    (funcall shovel-vm::*cells-incrementer* (+ 1 (length args)))
    result))

(defun check-vector (array)
  (unless (and (vectorp array) (not (stringp array)))
    (vm-error "First argument must be a vector.")))

(defun array-push (array new-element)
  (check-vector array)
  (funcall shovel-vm::*cells-incrementer* 1)
  (vector-push-extend new-element array))

(defun array-pop (array)
  (check-vector array)
  (unless (> (fill-pointer array) 0)
    (vm-error "Can't pop from an empty array."))
  (let ((result (aref array (1- (fill-pointer array)))))
    (setf (aref array (1- (fill-pointer array))) nil)
    (decf (fill-pointer array))
    result))

(defun array-constructor-n (n)
  (unless (integerp n)
    (vm-error "Argument must be an integer."))
  (funcall shovel-vm::*cells-increment-herald* (+ 1 n))
  (funcall shovel-vm::*cells-incrementer* (+ 1 n))
  (make-array n :initial-element :null :adjustable t :fill-pointer n))

(defun validate-index-access (array index)
  (cond ((< index 0)
         (vm-error "Index less than 0."))
        ((>= index (length array))
         (vm-error
          (format nil
                  "Invalid 0-based index (~d for an array with ~d elements)."
                  index
                  (length array))))))

;; Hash or array access:
(defun array-or-hash-get (array-or-hash index)
  (cond ((vectorp array-or-hash)
         (if (integerp index)
             (progn
               (validate-index-access array-or-hash index)
               (let ((result (aref array-or-hash index)))
                 (if (stringp array-or-hash)
                     (string result)
                     result)))
             (vm-error "Getting an array element requires an integer index.")))
        ((hash-table-p array-or-hash)
         (if (stringp index)
             (gethash index array-or-hash)
             (vm-error
              "Getting a hash table value requires a key that is a string.")))))

(defun hash-get-dot (hash-table key)
  (unless (hash-table-p hash-table)
    (vm-error "First argument must be a hash table."))
  (unless (stringp key)
    (vm-error "Second argument must be a string."))
  (unless (eq :true (has-key hash-table key))
    (vm-error "Key not found in hash table."))
  (gethash key hash-table))

(defun array-or-hash-set (array-or-hash index value)
  (cond ((vectorp array-or-hash)
         (if (numberp index)
             (progn
               (validate-index-access array-or-hash index)
               (cond ((stringp array-or-hash)
                      (when (/= 1 (length value))
                        (vm-error
                         "Must provide a one character string as right hand side for the assignment."))
                      (setf (aref array-or-hash index) (elt value 0)))
                     (t (setf (aref array-or-hash index) value))))
             (vm-error "Getting an array element requires an integer index.")))
        ((hash-table-p array-or-hash)
         (if (stringp index)
             (let ((hash-grows (not (gethash index array-or-hash))))
               (when hash-grows
                 ;; 1 for the key, 1 for the value
                 (funcall shovel-vm::*cells-incrementer* 2))
               (setf (gethash index array-or-hash) value))
             (vm-error
              "Getting a hash table value requires a key that is a string.")))))

;; Length of arrays or strings:

(defun get-length (array-or-string)
  (cond ((stringp array-or-string)
         (length array-or-string))
        ((vectorp array-or-string)
         (length array-or-string))
        (t (vm-error "Argument must be a string or an array."))))

(defun string-upper (string)
  (check-string string)
  (funcall shovel-vm::*cells-incrementer* (length string))
  (string-upcase string))

(defun string-lower (string)
  (check-string string)
  (funcall shovel-vm::*cells-incrementer* (length string))
  (string-downcase string))

;; Keys of a hash as an array:

(defun get-hash-table-keys (hash-table)
  (if (hash-table-p hash-table)
      (let (result)
        (maphash (lambda (k v) (declare (ignore v)) (push k result)) hash-table)
        (funcall shovel-vm::*cells-incrementer* (1+ (length result)))
        (coerce (reverse result) 'vector))
      (vm-error "Argument must be a hash table.")))

;; HasKey:

(defun has-key (hash-table key)
  (unless (hash-table-p hash-table)
    (vm-error "First argument must be a hash table."))
  (unless (stringp key)
    (vm-error "Second argument must be a string."))
  (multiple-value-bind (value has-key)
      (gethash key hash-table)
    (declare (ignore value))
    (make-bool has-key)))

;; Slice of a string or array:

(defun get-slice (array-or-string start end)
  (unless (vectorp array-or-string)
    (vm-error "Argument must be a string or an array."))
  (let* ((length (length array-or-string))
         (real-start (if (< start 0) (+ length start) start))
         (real-end (if (< end 0) (+ length end 1) end)))
    (when (> real-start real-end)
      (vm-error
       (format nil "Starting index (~d) is larger than ending index (~d)."
               real-start real-end)))
    (when (< real-start 0)
      (vm-error
       (format nil "Starting index (~d) is less than 0." real-start)))
    (when (>= real-start length)
      (vm-error
       (format nil
               "Starting index (~d) is equal to or larger than the length of the sequence (~d)."
               real-start length)))
    (when (< real-end 0)
      (vm-error
       (format nil "Ending index (~d) is less than 0." real-end)))
    (when (> real-end length)
      (vm-error
       (format nil
               "Ending index (~d) is larger than the length of the sequence (~d)."
               real-end length)))
    ;; add 1 for the new sequence, add 1 because the length of the new
    ;; sequence is REAL-END - REAL-START + 1
    (funcall shovel-vm::*cells-incrementer* (+ 2 (- real-end real-start)))
    (subseq array-or-string real-start real-end)))

;; Current date/time:

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970))

(defun lisp-to-unix-time (lisp-time)
  (- lisp-time +unix-epoch+))

(defun unix-to-lisp-time (unix-time)
  (+ unix-time +unix-epoch+))

(defun utc-seconds-since-unix-epoch ()
  (lisp-to-unix-time (get-universal-time)))

;; Date/time construction/deconstruction:

(defun decode-time (unix-time)
  (multiple-value-bind (second minute hour date month year day-of-week)
      (decode-universal-time (unix-to-lisp-time unix-time) 0)
    (hash-constructor "second" second
                      "minute" minute
                      "hour" hour
                      "day" date
                      "month" month
                      "year" year
                      "dayOfWeek" (1+ day-of-week))))

(defun encode-time (hash)
  (lisp-to-unix-time
   (encode-universal-time (gethash "second" hash)
                          (gethash "minute" hash)
                          (gethash "hour" hash)
                          (gethash "day" hash)
                          (gethash "month" hash)
                          (gethash "year" hash)
                          0)))

;; Object types:

(defun shovel-is-string (candidate) (make-bool (stringp candidate)))

(defun shovel-is-hash (candidate) (make-bool (hash-table-p candidate)))

(defun shovel-is-bool (candidate) (make-bool (is-bool candidate)))

(defun shovel-is-array (candidate) (make-bool (and (not (stringp candidate))
                                                   (vectorp candidate))))

(defun shovel-is-number (candidate) (make-bool (numberp candidate)))

(defun is-callable (candidate) (shovel-vm::callable-p candidate))

(defun shovel-is-callable (candidate) (make-bool (is-callable candidate)))

(defun unknown-type-error ()
  (vm-error "Object of unknown type."))

(defun shovel-string (var)
  (let ((result (cond ((stringp var) var)
                      ((vectorp var) "[...array...]")
                      ((numberp var) (format nil "~a" var))
                      ((hash-table-p var) "[...hash...]")
                      ((is-callable var) "[...callable...]")
                      ((is-bool var) (string-downcase (symbol-name var)))
                      ((eq :null var) "null")
                      (t (unknown-type-error)))))
    (funcall shovel-vm::*cells-incrementer* (+ 1 (length result)))
    result))

(defun shovel-string-representation (var &optional (visited (make-hash-table :test #'eq) visited-p))
  (let ((result
         (if (gethash var visited)
             "[...loop...]"
             (labels ((write-all-comma-separated (pieces str)
                        ;; Apparently Clozure CL takes some liberties with format
                        ;; strings like "~{~a~^ ~}" (it inserts #n= and #n#
                        ;; strings if the same string is inserted repeatedly a
                        ;; certain number of times), hence the need for this
                        ;; helper function.
                        (let ((is-first t))
                          (dolist (piece pieces)
                            (unless is-first
                              (write-string ", " str))
                            (write-string piece str)
                            (when is-first
                              (setf is-first nil))))))
               (cond ((stringp var) (with-output-to-string (str) (prin1 var str)))
                     ((vectorp var)
                      (setf (gethash var visited) t)
                      (with-output-to-string (str)
                        (write-string "array(" str)
                        (let ((pieces (mapcar (lambda (item)
                                                (shovel-string-representation item visited))
                                              (coerce var 'list))))
                          (write-all-comma-separated pieces str)
                          (write-string ")" str))))
                     ((hash-table-p var)
                      (setf (gethash var visited) t)
                      (with-output-to-string (str)
                        (write-string "hash(" str)
                        (let (pieces)
                          (dolist (key (sort (alexandria:hash-table-keys var) #'string<=))
                            (push (shovel-string-representation key visited) pieces)
                            (push (shovel-string-representation (gethash key var) visited) pieces))
                          (write-all-comma-separated (nreverse pieces) str)
                          (write-string ")" str))))
                     ((or (eq :null var)
                          (numberp var)
                          (is-bool var)
                          (is-callable var))
                      (shovel-string var))
                     (t (unknown-type-error)))))))
    (unless visited-p
      (funcall shovel-vm::*cells-incrementer* (+ 1 (length result))))
    result))

;; Parsing numbers:

(defun check-string (var)
  (unless (stringp var)
    (vm-error "Argument must be a string.")))

(defun parse-int (var)
  (check-string var)
  (limit-integer-result (parse-integer var)))

(defun parse-float (var)
  (check-string var)
  (alexandria:if-let (dotpos (position #\. var))
    (let ((integer-part (parse-integer var :start 0 :end dotpos))
          (fractional-part (parse-integer var :start (1+ dotpos))))
      (+ integer-part
         (/ (float fractional-part) (expt 10 (- (length var) dotpos 1)))))
    (parse-integer var)))

(defun panic (message)
  (check-string message)
  (vm-error message))
