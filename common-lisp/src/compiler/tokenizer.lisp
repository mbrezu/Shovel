
(in-package #:shovel-compiler-tokenizer)

(defstruct tokenizer-state source current-pos (previous-pos (make-pos)))

(defvar *tokenizer-state*)

(defun make-pos-from-current ()
  (clone-pos (tokenizer-state-current-pos *tokenizer-state*)))

(defun make-pos-from-previous ()
  (clone-pos (tokenizer-state-previous-pos *tokenizer-state*)))

(declaim (inline extract-content))
(defun extract-content (start-pos end-pos)
  (subseq (tokenizer-state-source *tokenizer-state*)
          (1- (pos-char start-pos))
          (pos-char end-pos)))

(declaim (inline current-char))
(defun current-char (&optional forced-current-char)
  (let ((current-source (tokenizer-state-source *tokenizer-state*))
        (current-char
         (or forced-current-char
             (pos-char (tokenizer-state-current-pos *tokenizer-state*)))))
    (cond ((> current-char (length current-source)) nil)
          (t (elt current-source (1- current-char))))))

(defun lookahead-char (&optional (n 1))
  (let ((lookahead-position
         (+ n (pos-char (tokenizer-state-current-pos *tokenizer-state*)))))
    (current-char lookahead-position)))

(defun copy-pos-slots (source-pos destination-pos)
  (setf (pos-line destination-pos) (pos-line source-pos)
        (pos-column destination-pos) (pos-column source-pos)
        (pos-char destination-pos) (pos-char source-pos)))

(defun next-char ()
  (let ((pos (tokenizer-state-current-pos *tokenizer-state*)))
    (copy-pos-slots pos (tokenizer-state-previous-pos *tokenizer-state*))
    (let ((ch (current-char)))
      (if (and ch (char= #\newline ch))
          (progn
            (incf (pos-line pos))
            (setf (pos-column pos) 1))
          (incf (pos-column pos))))
    (incf (pos-char pos))))

(defun is-white-space (ch)
  (and ch (or (char= #\space ch) (char= #\newline ch) (char= #\tab ch))))

(defun eat-white-space ()
  (loop while (is-white-space (current-char)) do (next-char)))

(defun tokenize (&optional tokens)
  (eat-white-space)
  (let ((ch (current-char)))
    (cond ((not ch) (reverse tokens))
          ((or (char= #\_ ch) (char= #\$ ch) (alpha-char-p ch)
               (char= #\@ ch))
           (tokenize (cons (tokenize-identifier) tokens)))
          ((or (digit-char-p ch))
           (tokenize (cons (tokenize-number) tokens)))
          ((char= #\" ch)
           (tokenize (cons (tokenize-literal-string #\") tokens)))
          ((char= #\' ch)
           (tokenize (cons (tokenize-literal-string #\') tokens)))
          ((and (char= #\/ ch) (char= #\/ (lookahead-char)))
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
  (let ((start-pos (make-pos-from-current)))
    (loop
       for ch = (current-char) then (current-char)
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

(defun tokenize-identifier ()
  (let ((result (tokenize-pred :identifier
                               (lambda (ch)
                                 (or (char= #\_ ch)
                                     (char= #\$ ch)
                                     (char= #\@ ch)
                                     (alpha-char-p ch)
                                     (digit-char-p ch))))))
    (when (char= (elt (token-content result) 0) #\@)
      (setf (token-type result) :prim
            (token-content result) (subseq (token-content result) 1)))
    result))

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
    (cond ((member crt '(#\( #\) #\[ #\] #\+ #\- #\* #\/ #\{ #\} #\,))
           (make-punctuation-token 1))
          ((char= crt #\=)
           (make-punctuation-token (if (and la (char= la #\=)) 2 1)))
          ((char= crt #\!)
           (make-punctuation-token (if (and la (char= la #\=)) 2 1)))
          ((or (char= crt #\<) (char= crt #\>))
           (make-punctuation-token (if (and la (or (char= la #\=)
                                                   (char= la #\<)
                                                   (char= la #\>)))
                                       2 1)))
          ((char= crt #\|)
           (make-punctuation-token (if (and la (char= la #\|)) 2 1)))
          ((char= crt #\&)
           (make-punctuation-token (if (and la (char= la #\&)) 2 1)))
          ((char= crt #\.)
           (make-punctuation-token 1))
          (t (tokenize-pred :punctuation
                            (lambda (ch) (not (is-white-space ch))))))))

(defun tokenize-string (source)
  (let ((*tokenizer-state* (make-tokenizer-state :source source
                                                 :current-pos (make-pos))))
    (tokenize)))
