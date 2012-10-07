
(in-package #:shovel-compiler-parser)

(defstruct parse-state tokens (previous-token nil) (source nil))

(defvar *parse-state* nil)

(declaim (inline current-token))
(defun current-token () (car (parse-state-tokens *parse-state*)))

(declaim (inline previous-token))
(defun previous-token () (parse-state-previous-token *parse-state*))

(declaim (inline next-token))
(defun next-token () (setf (parse-state-previous-token *parse-state*)
                           (pop (parse-state-tokens *parse-state*))))

(defun parse-tokens (tokens &key source)
  (let ((*parse-state* (make-parse-state :tokens tokens :source source))
        result)
    (loop
       while (parse-state-tokens *parse-state*)
       do (push (parse-statement) result))
    (reverse result)))

(defun parse-statement ()
  (cond ((tokenp :identifier "var")
         (parse-var-decl))
        ((tokenp :identifier "return")
         (parse-return))
        (t (let ((expr (parse-expression)))
             (if (tokenp :punctuation "=")
                 (parse-assignment expr)
                 expr)))))

(defun tokenp (expected-type &optional expected-content)
  (declare (optimize speed))
  (let ((token (current-token)))
    (when token
      (let ((type (token-type token))
            (content (token-content token)))
        (declare (type (simple-array character (*)) content))
        (and (eq type expected-type)
             (or (null expected-content)
                 (string= content expected-content)))))))

(defun get-current-start-pos () (token-start-pos (current-token)))

(defun get-previous-end-pos () (token-end-pos (previous-token)))

(defmacro with-new-parse-tree (label &body body)
  "Creates a new parse tree with label LABEL and children returned by
BODY, and with START-POS the start position of the current token
before running BODY and END-POS the end position of the previous token
after running BODY. Makes it easy to write parsing code that records
start and end positions for nodes without actually worrying about the
token positions."
  (let ((start-pos (gensym))
        (children (gensym))
        (end-pos (gensym)))
    `(let ((,start-pos (get-current-start-pos))
           (,children (progn ,@body))
           (,end-pos (get-previous-end-pos)))
       (make-parse-tree :label ,label
                        :start-pos ,start-pos
                        :end-pos ,end-pos
                        :children ,children))))

(defmacro with-new-anchored-parse-tree (start-pos label &body body)
  "Like WITH-NEW-PARSE-TREE, but with a specified START-POS."
  (let ((children (gensym))
        (end-pos (gensym)))
    `(let ((,children (progn ,@body))
           (,end-pos (get-previous-end-pos)))
       (make-parse-tree :label ,label
                        :start-pos ,start-pos
                        :end-pos ,end-pos
                        :children ,children))))

(defun parse-block ()
  (declare (optimize speed))
  (with-new-parse-tree :begin
    (let (statements)
      (consume-token :punctuation "{")
      (loop
         while (and (current-token)
                    (not (tokenp :punctuation "}")))
         do (push (parse-statement) statements))
      (consume-token :punctuation "}")
      (reverse statements))))

(defun consume-token (type content)
  (declare (optimize speed))
  (require-token-1 type content)
  (next-token))

(defun length=1 (list) (and (consp list) (null (rest list))))

(defun require-token-error (possible-tokens token)
  (let* ((expectation
          (let ((possible-contents
                 (remove-if #'null (mapcar #'second possible-tokens)))
                (possible-types (remove-duplicates
                                 (mapcar #'first possible-tokens))))
            (cond
              ((null possible-contents)
               (cond
                 ((null possible-types)
                  (error "Shovel internal WTF."))
                 ((length=1 possible-types)
                  (format nil "Expected a ~(~a~)"
                          (first possible-types)))
                 (t (format nil "Expected a ~{~(~a~)~^ or a ~}"
                            possible-types))))
              ((length=1 possible-contents)
               (format nil "Expected '~a'"
                       (first possible-contents)))
              (t
               (format nil "Expected ~{'~a'~^ or ~}"
                       possible-contents)))))
         (actual-input (if token
                           (format nil "but got '~a'" (token-content token))
                           "but reached the end of the input"))
         (message (format nil "~a, ~a." expectation actual-input)))
    (raise-error message)))

(defun require-token (&rest possible-tokens)
  (declare (optimize speed))
  (let ((token (current-token)))
    (unless (and token (some (lambda (candidate)
                               (apply #'tokenp candidate))
                             possible-tokens))
      (require-token-error possible-tokens token))))

(defun require-token-1 (possible-token-type &optional possible-token-content)
  (declare (optimize speed))
  (let ((token (current-token)))
    (unless (and token (tokenp possible-token-type
                               possible-token-content))
      (require-token-error (list possible-token-type
                                 possible-token-content)
                           token))))

(defun parse-var-decl ()
  (with-new-parse-tree :var
    (consume-token :identifier "var")
    (let ((lhs (parse-name nil)))
      (consume-token :punctuation "=")
      (let ((rhs (parse-expression)))
        (list lhs rhs)))))

(defun parse-return ()
  (with-new-parse-tree :return
    (consume-token :identifier "return")
    (parse-expression)))

(defun parse-assignment (lhs)
  (with-new-anchored-parse-tree (parse-tree-start-pos lhs) :set!
    (list lhs (token-as-parse-tree :prim0) (parse-expression))))

;; Precedence table:
;;
;; ()
;; function call, array access, dot access.
;; unary -
;; * /
;; + -
;; < > <= >= != ==
;; &&
;; ||
;; =
;; fn, if
(defun parse-expression ()
  (cond
    ((tokenp :identifier "fn") ; Handle function literals.
     (parse-lambda))
    ((tokenp :identifier "if") ; Handle branches.
     (parse-if))
    (t (left-assoc #'parse-or-term #'token-is-logical-or-op))))

(defun make-prim0-parse-tree (primitive-name)
  (with-new-parse-tree :prim0
    (prog1
        primitive-name
      (next-token))))

(defun left-assoc (sub-parser operator-pred)
  (declare (optimize speed)
           (type (function (token) boolean) operator-pred)
           (type (function () parse-tree) sub-parser))
  (labels ((iter (start)
             (let ((token (current-token)))
               (if (and token (funcall operator-pred token))
                   (iter (with-new-anchored-parse-tree
                             (parse-tree-start-pos start)
                             :call
                           (list (make-prim0-parse-tree (token-content token))
                                 start
                                 (funcall sub-parser))))
                   start))))
    (iter (funcall sub-parser))))

(defun parse-or-term () (left-assoc #'parse-and-term #'token-is-logical-and-op))

(defun parse-and-term ()
  (left-assoc #'parse-relational-term #'token-is-relational-op))

(defun parse-relational-term ()
  (left-assoc #'parse-addition-term #'token-is-adder-op))

(defun parse-addition-term ()
  (left-assoc #'parse-multiplication-term #'token-is-multiplier-op))

(defun parse-multiplication-term ()
  (cond ((tokenp :punctuation "-") (parse-unary-minus))
        ((tokenp :punctuation "!") (parse-logical-not))
        (t (parse-tight-unary))))

(defun parse-logical-not ()
  (with-new-parse-tree :call
    (require-token-1 :punctuation "!")
    (list (make-prim0-parse-tree "!") (parse-multiplication-term))))

(defun parse-unary-minus ()
  (with-new-parse-tree :call
    (require-token-1 :punctuation "-")
    (list (make-prim0-parse-tree "unary-minus") (parse-multiplication-term))))

(defun parse-tight-unary ()
  (cond ((tokenp :number) (parse-number))
        ((tokenp :string) (parse-literal-string))
        ((or (tokenp :identifier "true")
             (tokenp :identifier "false"))
         (parse-bool))
        ((tokenp :identifier "null")
         (parse-void))
        (t (parse-identifier-or-call-or-ref))))

(defun parse-identifier-or-call-or-ref (&optional forced-start)
  (let ((start (or forced-start (parse-parenthesized-or-name))))
    (cond ((tokenp :punctuation ".") ; Handle hash access by dotting.
           (parse-identifier-or-call-or-ref
            (with-new-anchored-parse-tree
                (parse-tree-start-pos start)
                :call
              (require-token-1 :punctuation ".")
              (list (make-prim0-parse-tree "svm_gref_dot")
                    start
                    (parse-name-as-string)))))
          ((tokenp :punctuation "(") ; Handle function call.
           (parse-identifier-or-call-or-ref
            (with-new-anchored-parse-tree (parse-tree-start-pos start) :call
              (cons start (parse-list #'parse-expression
                                      '(:punctuation "(")
                                      '(:punctuation ",")
                                      '(:punctuation ")"))))))
          ((tokenp :punctuation "[") ; Handle array or hash access.
           (parse-identifier-or-call-or-ref
            (let* (gref-parse-tree
                   gref-start-pos
                   gref-end-pos
                   (result (with-new-anchored-parse-tree
                               (parse-tree-start-pos start)
                               :call
                             (setf gref-start-pos (get-current-start-pos))
                             (require-token-1 :punctuation "[")
                             (prog1
                                 (list (setf gref-parse-tree
                                             (make-prim0-parse-tree "svm_gref"))
                                       start
                                       (parse-expression))
                               (consume-token :punctuation "]")
                               (setf gref-end-pos (get-previous-end-pos))))))
              (setf (parse-tree-start-pos gref-parse-tree) gref-start-pos
                    (parse-tree-end-pos gref-parse-tree) gref-end-pos)
              result)))
          (t start))))

(defun raise-error (message)
  (let* ((token (current-token))
         (pos (if token (token-start-pos token)))
         (line (if token (pos-line pos)))
         (column (if token (pos-column pos)))
         (at-eof (not token)))
    (when pos
      (alexandria:when-let (source (parse-state-source *parse-state*))
        (let* ((start-pos (token-start-pos token))
               (end-pos (token-end-pos token))
               (lines (extract-relevant-source source start-pos end-pos)))
          (setf message
                (format nil "~a~%~a~%~a" message
                        (first lines)
                        (second lines))))))
    (error (make-condition 'shovel-compiler-error
                           :message message
                           :line line
                           :column column
                           :at-eof at-eof))))

(defun parse-parenthesized-or-name ()
  (cond ((tokenp :identifier) (parse-name))
        ((tokenp :punctuation "(")  ; Handle parenthesized expression.
         (consume-token :punctuation "(")
         (prog1
             (parse-expression)
           (consume-token :punctuation ")")))
        ((tokenp :punctuation "{")
         (parse-block))
        ((tokenp :prim) (parse-prim))
        (t (raise-error (if (current-token)
                            (format nil "Unexpected token '~a'."
                                    (token-content (current-token)))
                            "Unexpected end of file.")))))

(defun token-as-parse-tree (label)
  (with-new-parse-tree label
    (prog1
        (token-content (current-token))
      (next-token))))

(defun parse-number () (token-as-parse-tree :number))

(defun parse-bool () (token-as-parse-tree :bool))

(defun parse-void () (token-as-parse-tree :void))

(defun parse-literal-string () (token-as-parse-tree :string))

(defun parse-lambda ()
  (with-new-parse-tree :fn
    (consume-token :identifier "fn")
    (let ((args (parse-lambda-args))
          (body (parse-statement)))
      (list args body))))

(defun parse-lambda-args ()
  (with-new-parse-tree :list
    (if (tokenp :punctuation "(")
        (parse-list (lambda () (parse-name nil))
                    '(:punctuation "(")
                    '(:punctuation ",")
                    '(:punctuation ")"))
        (list (parse-name nil)))))

(defun parse-list (item-parser open-paren separator close-paren)
  (declare (optimize speed)
           (type (function () t) item-parser))
  (let (result)
    (apply #'consume-token open-paren)
    (loop
       (unless (apply #'tokenp close-paren)
         (push (funcall item-parser) result))
       (require-token separator close-paren)
       (when (apply #'tokenp close-paren)
         (return))
       (apply #'consume-token separator))
    (apply #'consume-token close-paren)
    (reverse result)))

(defun parse-name (&optional (can-be-required-primitive t))
  (require-token-1 :identifier)
  (let* ((token (current-token))
         (content (token-content token)))
    (when (token-is-keyword token)
      (raise-error (format nil "'~a' is a reserved keyword." content)))
    (if (token-is-required-primitive token)
        (if can-be-required-primitive
            (token-as-parse-tree :prim0)
            (let* ((pos (token-start-pos (current-token)))
                   (line (pos-line pos))
                   (column (pos-column pos))
                   (message (format nil "Name '~a' is reserved for a primitive."
                                    content)))
              (error (make-condition 'shovel-compiler-error
                                     :message message
                                     :line line
                                     :column column))))
        (token-as-parse-tree :name))))

(defun parse-prim ()
  (require-token-1 :prim)
  (token-as-parse-tree :prim))

(defun parse-name-as-string ()
  (require-token-1 :identifier)
  (let ((result (token-as-parse-tree :string)))
    (setf (parse-tree-children result)
          (format nil "\"~a\"" (parse-tree-children result)))
    result))

(defun parse-if ()
  (with-new-parse-tree :if
    (consume-token :identifier "if")
    (let ((pred (parse-expression))
          (then (parse-statement)))
      (if (tokenp :identifier "else")
          (progn
            (consume-token :identifier "else")
            (list pred then (parse-statement)))
          (list pred then (make-parse-tree
                           :label :void
                           :children "null"
                           :start-pos nil
                           :end-pos nil))))))

(defun simplify-parse-tree (parse-tree)
  (if (parse-tree-p parse-tree)
      (cons (parse-tree-label parse-tree)
            (let ((children (parse-tree-children parse-tree)))
              (if (listp children)
                  (mapcar #'simplify-parse-tree children)
                  (list (simplify-parse-tree children)))))
      parse-tree))
