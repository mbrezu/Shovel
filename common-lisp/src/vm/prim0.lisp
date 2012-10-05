
(in-package #:shovel-vm-prim0)

(defvar *error-raiser*)

(defmacro both-numbers-or-strings (op-numbers op-strings)
  `(cond ((and (numberp t1) (numberp t2))
          (,op-numbers t1 t2))
         ((and (stringp t1) (stringp t2))
          (,op-strings t1 t2))
         (t (funcall *error-raiser*
                     "Arguments must have the same type (numbers or strings)."))))

(defmacro both-numbers (op)
  `(cond ((and (numberp t1) (numberp t2))
          (,op t1 t2))
         (t (funcall *error-raiser* "Arguments must both be numbers."))))

;; Arithmetic operators:
(defun add (t1 t2)
  (labels ((strcat (t1 t2) (concatenate 'string t1 t2)))
    (both-numbers-or-strings + strcat)))
(defun subtract (t1 t2) (both-numbers -))
(defun unary-minus (t1)
  (if (numberp t1)
      (- t1)
      (funcall *error-raiser* "Argument must be number.")))
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
        (t (funcall *error-raiser* "Arguments must be both numbers."))))

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
         (t (funcall *error-raiser* "Arguments must both be booleans."))))

(defun logical-and (t1 t2)
  (labels ((and-op (t1 t2) (and (eq :true t1) (eq :true t2))))
    (make-bool (both-bools and-op))))

(defun logical-or (t1 t2)
  (labels ((or-op (t1 t2) (or (eq :true t1) (eq :true t2))))
    (make-bool (both-bools or-op))))

(defun logical-not (t1)
  (if (is-bool t1)
      (if (eq :true t1) :false :true)
      (funcall *error-raiser* "Argument must be boolean.")))

;; Relational operators:
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
         (t (funcall *error-raiser* "Arguments must both be integers."))))
(defun bitwise-and (t1 t2)
  (both-integers logand))
(defun bitwise-or (t1 t2)
  (both-integers logior))
;; Hash literal:

;; Array literal:

;; Hash or array access:

;; Current date/time:

;; Date/time construction/deconstruction:
