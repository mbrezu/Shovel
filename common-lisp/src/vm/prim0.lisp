
(in-package #:shovel-vm-prim0)

;; Arithmetic operators:

(defun vm-error (message)
  (funcall shovel-vm:*error-raiser* message))

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
(defun add (t1 t2)
  (labels ((strcat (t1 t2) (concatenate 'string t1 t2))
           (array-cat (t1 t2) (concatenate 'vector t1 t2)))
    (both-numbers-or-strings-or-arrays + strcat array-cat)))
(defmacro both-numbers (op)
  `(cond ((and (numberp t1) (numberp t2))
          (,op t1 t2))
         (t (vm-error "Arguments must both be numbers."))))
(defun subtract (t1 t2) (both-numbers -))
(defun unary-minus (t1)
  (if (numberp t1)
      (- t1)
      (vm-error "Argument must be number.")))
(defun multiply (t1 t2) (both-numbers *))
(defun shift-left (t1 t2)
  (labels ((shl (t1 t2) (ash t1 t2)))
    (both-numbers shl)))
(defun shift-right (t1 t2)
  (labels ((shr (t1 t2) (ash t1 (- t2))))
    (both-numbers shr)))
(defun divide (t1 t2)
  (cond ((and (integerp t1) (integerp t2))
         (floor (/ t1 t2)))
        ((and (numberp t1) (numberp t2))
         (float (/ t1 t2)))
        (t (vm-error "Arguments must be both numbers."))))

;; Logic operators:
(defun is-bool (var)
  (member var '(:true :false)))

(defun make-bool (var)
  (if var :true :false))

(defun is-true (var)
  (eq :true var))

(defmacro both-bools (op)
  `(cond ((and (is-bool t1) (is-bool t2))
          (,op t1 t2))
         (t (vm-error "Arguments must both be booleans."))))

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
(defun are-equal (t1 t2) (make-bool (both-numbers-or-strings = string=)))
(defun are-not-equal (t1 t2) (make-bool (both-numbers-or-strings /= string/=)))

;; Bitwise operators:
(defmacro both-integers (op)
  `(cond ((and (integerp t1) (integerp t2))
          (,op t1 t2))
         (t (vm-error "Arguments must both be integers."))))
(defun bitwise-and (t1 t2)
  (both-integers logand))
(defun bitwise-or (t1 t2)
  (both-integers logior))

;; Hash constructor:
(defun hash-constructor (&rest args)
  (when (/= 0 (mod (length args) 2))
    (vm-error "Must provide an even number of arguments."))
  (let ((result (make-hash-table :test #'equal)))
    (loop
       while args
       do (let ((key (first args))
                (value (second args)))
            (when (not (stringp key))
              (vm-error "Keys must be strings"))
            (setf (gethash key result) value)
            (setf args (cddr args))))
    result))

;; Array constructor:
(defun array-constructor (&rest args)
  (coerce args 'vector))

(defun array-constructor-n (n)
  (make-array n))

;; Hash or array access:
(defun array-or-hash-get (array-or-hash index)
  (cond ((vectorp array-or-hash)
         (if (numberp index)
             (aref array-or-hash index)
             (vm-error "Getting an array element requires an integer index.")))
        ((hash-table-p array-or-hash)
         (if (stringp index)
             (gethash index array-or-hash)
             (vm-error
              "Getting a hash table value requires a key that is a string.")))))

(defun array-or-hash-set (array-or-hash index value)
  (cond ((vectorp array-or-hash)
         (if (numberp index)
             (setf (aref array-or-hash index) value)
             (vm-error "Getting an array element requires an integer index.")))
        ((hash-table-p array-or-hash)
         (if (stringp index)
             (setf (gethash index array-or-hash) value)
             (vm-error
              "Getting a hash table value requires a key that is a string.")))))

;; Length of arrays or strings:

(defun get-length (array-or-string)
  (cond ((stringp array-or-string)
         (length array-or-string))
        ((vectorp array-or-string)
         (length array-or-string))
        (t (vm-error "Argument must be a string or an array."))))

;; Keys of a hash as an array:

(defun get-hash-table-keys (hash-table)
  (if (hash-table-p hash-table)
      (let (result)
        (maphash (lambda (k v) (declare (ignore v)) (push k result)) hash-table)
        (coerce (reverse result) 'vector))
      (vm-error "Argument must be a hash table.")))

;; Slice of a string or array:

(defun get-slice (array-or-string start end)
  (unless (vectorp array-or-string)
    (vm-error "Argument must be a string or an array."))
  (let* ((length (length array-or-string))
         (real-start (if (< start 0) (+ length start) start))
         (real-end (if (< end 0) (+ length end) end)))
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
    (when (>= real-end length)
      (vm-error
       (format nil
               "Ending index (~d) is equal to or larger than the length of the sequence (~d)."
               real-end length)))
    (subseq array-or-string real-start (1+ real-end))))

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
    (alexandria:alist-hash-table (list (cons "second" second)
                                       (cons "minute" minute)
                                       (cons "hour" hour)
                                       (cons "day" date)
                                       (cons "month" month)
                                       (cons "year" year)
                                       (cons "dayOfWeek" (1+ day-of-week)))
                                 :test #'equal)))

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
  (cond ((stringp var) var)
        ((vectorp var) "[...array...]")
        ((numberp var) (format nil "~a" var))
        ((hash-table-p var) "[...hash...]")
        ((is-callable var) "[...callable...]")
        ((is-bool var) (string-downcase (symbol-name var)))
        (t (unknown-type-error))))

(defun shovel-string-representation (var)
  (cond ((stringp var) (with-output-to-string (str) (prin1 var str)))
        ((vectorp var) (with-output-to-string (str)
                         (write-string "array(" str)
                         (let ((pieces (mapcar #'shovel-string-representation
                                               (coerce var 'list))))
                           (format str "~{~a~^, ~}" pieces)
                           (write-string ")" str))))
        ((hash-table-p var)
         (with-output-to-string (str)
           (write-string "hash(" str)
           (let (pieces)
             (maphash (lambda (key value)
                        (push (shovel-string-representation key) pieces)
                        (push (shovel-string-representation value) pieces))
                      var)
             (format str "~{~a~^, ~}" (nreverse pieces))
             (write-string ")" str))))
        ((or (numberp var) (is-bool var) (is-callable var)) (shovel-string var))
        (t (unknown-type-error))))

;; Parsing numbers:

(defun check-string (var)
  (unless (stringp var)
    (vm-error "Argument must be a string.")))

(defun parse-int (var)
  (check-string var)
  (parse-integer var))

(defun parse-float (var)
  (check-string var)
  (alexandria:if-let (dotpos (position #\. var))
    (let ((integer-part (parse-integer var :start 0 :end dotpos))
          (fractional-part (parse-integer var :start (1+ dotpos))))
      (+ integer-part
         (/ (float fractional-part) (expt 10 (- (length var) dotpos 1)))))
    (parse-integer var)))
