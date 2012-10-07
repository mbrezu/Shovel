
(in-package #:shovel-compiler-tokenizer)

(defstruct tokenizer-state source current-pos (previous-pos (make-pos)))

(defvar *tokenizer-state*)

(declaim (inline make-pos-from-current))
(defun make-pos-from-current ()
  (clone-pos (tokenizer-state-current-pos *tokenizer-state*)))

(declaim (inline make-pos-from-previous))
(defun make-pos-from-previous ()
  (clone-pos (tokenizer-state-previous-pos *tokenizer-state*)))

(declaim (inline extract-content))
(defun extract-content (start-pos end-pos)
  (declare (type pos start-pos end-pos))
  (subseq (the (simple-array character (*))
            (tokenizer-state-source *tokenizer-state*))
          (1- (the fixnum (pos-char start-pos)))
          (pos-char end-pos)))

(declaim (inline current-char))
(defun current-char (&optional forced-current-char)
  (declare (optimize speed (safety 0)))
  (let ((current-source (tokenizer-state-source *tokenizer-state*))
        (current-char
         (or forced-current-char
             (pos-char (tokenizer-state-current-pos *tokenizer-state*)))))
    (declare (type (simple-array character (*)) current-source)
             (type fixnum current-char))
    (cond ((> current-char (length current-source)) nil)
          (t (the character
               (aref (the (vector character) current-source)
                     (1- current-char)))))))

(defun test ()
  (declare (optimize speed))
  (aref (the (simple-array character (*)) (shovel:stdlib)) 0))

(declaim (inline lookahead-char))
(defun lookahead-char (&optional (n 1))
  (let ((lookahead-position
         (+ n (pos-char (tokenizer-state-current-pos *tokenizer-state*)))))
    (current-char lookahead-position)))

(defun copy-pos-slots (source-pos destination-pos)
  (setf (pos-line destination-pos) (pos-line source-pos)
        (pos-column destination-pos) (pos-column source-pos)
        (pos-char destination-pos) (pos-char source-pos)))

(declaim (inline next-char))
(defun next-char ()
  (declare (optimize (safety 0)))
  (let ((pos (tokenizer-state-current-pos *tokenizer-state*)))
    (declare (type pos pos))
    (copy-pos-slots pos (tokenizer-state-previous-pos *tokenizer-state*))
    (let ((ch (current-char)))
      (if (and ch (char= #\newline ch))
          (progn
            (incf (pos-line pos))
            (setf (pos-column pos) 1))
          (incf (pos-column pos))))
    (incf (pos-char pos))))

(declaim (inline is-white-space))
(defun is-white-space (ch)
  (declare (optimize speed))
  (and ch (or (char= #\space ch) (char= #\newline ch) (char= #\tab ch))))

(defun eat-white-space ()
  (declare (optimize speed))
  (loop while (is-white-space (current-char)) do (next-char)))

(defun tokenize (&optional tokens)
  (declare (optimize speed (safety 0)))
  (eat-white-space)
  (let ((ch (current-char)))
    (cond ((not ch) (reverse tokens))
          ((or (char= #\_ ch) (char= #\$ ch) (alpha-char-p ch)
               (char= #\@ ch))
           (tokenize (cons (tokenize-identifier) tokens)))
          ((or (digit-char-p ch))
           (tokenize (cons (tokenize-number) tokens)))
          ((or (char= #\" ch) (char= #\' ch))
           (tokenize (cons (tokenize-literal-string ch) tokens)))
          ((let ((la (lookahead-char)))
             (and (char= #\/ ch) la (char= #\/ la)))
           (eat-comment)
           (tokenize tokens))
          (t (tokenize (cons (tokenize-punctuation) tokens))))))

(defun eat-comment ()
  (next-char)
  (next-char)
  (tokenize-pred :dummy (lambda (ch) (char/= ch #\newline)))
  (next-char))

(defun tokenize-pred (type pred)
  "Forms a token with type TYPE from the characters for which PRED
  holds, starting with the current character."
  (declare (optimize speed)
           (type (function (character) boolean) pred))
  (let ((start-pos (make-pos-from-current)))
    (loop
       for ch of-type character = (current-char) then (current-char)
       while (and ch (funcall pred ch))
       do (next-char))
    (let ((end-pos (make-pos-from-previous)))
      (make-token :type type
                  :content (extract-content start-pos end-pos)
                  :start-pos start-pos
                  :end-pos end-pos))))

(defun tokenize-literal-string (quote)
  (let ((quote-counter 0)
        escaped)
    (prog1
        (tokenize-pred :string
                       (lambda (ch)
                         (prog1
                             (< quote-counter 2)
                           (when (and (char= quote ch) (not escaped))
                             (incf quote-counter))
                           (setf escaped (char= #\\ ch)))))
      (unless (= 2 quote-counter)
        (error (make-condition
                'shovel-error
                :message "Expected an end quote, but reached the end of file."
                :at-eof t))))))

(defmacro on ((var-name object) &body body)
  (check-type var-name symbol)
  `(let ((,var-name ,object))
     ,@body
     ,var-name))

(defun tokenize-identifier ()
  (declare (optimize speed))
  (on (result (tokenize-pred :identifier (lambda (ch)
                                           (or (char= #\_ ch)
                                               (char= #\$ ch)
                                               (char= #\@ ch)
                                               (alpha-char-p ch)
                                               (digit-char-p ch)))))
    (let ((content (token-content result)))
      (declare (type (simple-array character (*)) content))
      (when (char= (aref content 0) #\@)
        (setf (token-type result) :prim
              (token-content result) (subseq content 1)))
      (shovel-utils:when-one-of-strings (token-content result)
        (("pow"
          "array" "arrayN" "length" "slice"
          "hash" "keys" "hasKey"
          "utcSecondsSinceUnixEpoch" "decodeTime" "encodeTime"
          "isString" "isHash" "isBool" "isArray" "isNumber" "isCallable"
          "string" "stringRepresentation"
          "parseInt" "parseFloat"
          "panic")
         (setf (token-is-required-primitive result) t))
        (("var" "if" "fn" "return" "true" "false")
         (setf (token-is-keyword result) t))))))

(defun tokenize-number ()
  (let (after-decimal-dot)
    (tokenize-pred :number
                   (lambda (ch)
                     (if after-decimal-dot
                         (digit-char-p ch)
                         (if (char= #\. ch)
                             (setf after-decimal-dot t)
                             (digit-char-p ch)))))))

(defun make-punctuation-token (length)
  (declare (optimize speed)
           (type fixnum length))
  (let ((start-pos (make-pos-from-current)))
    (loop repeat length do (next-char))
    (let ((end-pos (make-pos-from-previous)))
      (make-token :type :punctuation
                  :content (extract-content start-pos end-pos)
                  :start-pos start-pos
                  :end-pos end-pos))))

(defun tokenize-punctuation ()
  (let ((crt (current-char))
        (la (lookahead-char)))
    (cond ((or (char= crt #\+) (char= crt #\-))
           (on (result (make-punctuation-token 1))
             (setf (token-is-adder-op result) t)))
          ((or (char= crt #\*) (char= crt #\/) (char= crt #\%) (char= crt #\^))
           (on (result (make-punctuation-token 1))
             (setf (token-is-multiplier-op result) t)))
          ((member crt '(#\( #\) #\[ #\] #\{ #\} #\,))
           (make-punctuation-token 1))
          ((char= crt #\=)
           (let ((is-relational (and la (char= la #\=))))
             (on (result (make-punctuation-token (if is-relational 2 1)))
               (when is-relational
                 (setf (token-is-relational-op result) t)))))
          ((char= crt #\!)
           (let* ((is-relational (and la (char= la #\=))))
             (on (result (make-punctuation-token (if is-relational 2 1)))
               (when is-relational
                 (setf (token-is-relational-op result) t)))))
          ((or (char= crt #\<) (char= crt #\>))
           (let* ((is-long-relational (and la (char= la #\=)))
                  (is-multiplier (and la (or (char= la #\<)
                                             (char= la #\>))))
                  (is-relational (or is-long-relational (not is-multiplier))))
             (on (result (make-punctuation-token (if (or is-long-relational
                                                         is-multiplier)
                                                     2 1)))
               (cond (is-relational (setf (token-is-relational-op result) t))
                     (is-multiplier (setf (token-is-multiplier-op result) t))))))
          ((char= crt #\|)
           (let ((is-logical (and la (char= la #\|))))
             (on (result (make-punctuation-token (if is-logical 2 1)))
               (cond (is-logical (setf (token-is-logical-or-op result) t))
                     (t (setf (token-is-adder-op result) t))))))
          ((char= crt #\&)
           (let ((is-logical (and la (char= la #\&))))
             (on (result (make-punctuation-token (if is-logical 2 1)))
               (cond (is-logical (setf (token-is-logical-and-op result) t))
                     (t (setf (token-is-multiplier-op result) t))))))
          ((char= crt #\.)
           (make-punctuation-token 1))
          (t (tokenize-pred :punctuation
                            (lambda (ch) (not (is-white-space ch))))))))

(defun tokenize-string (source)
  (let ((*tokenizer-state* (make-tokenizer-state :source source
                                                 :current-pos (make-pos))))
    (tokenize)))
