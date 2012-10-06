
(in-package #:shovel-compiler-parser)

(defstruct parse-state tokens (previous-token nil) (source nil))

(defvar *parse-state* nil)

(defun current-token () (car (parse-state-tokens *parse-state*)))

(defun previous-token () (parse-state-previous-token *parse-state*))

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
  (cond ((tokenp :punctuation "{")
         (parse-block))
        ((tokenp :identifier "var")
         (parse-var-decl))
        ((tokenp :identifier "return")
         (parse-return))
        (t (let ((expr (parse-expression)))
             (if (tokenp :punctuation "=")
                 (parse-assignment expr)
                 expr)))))

(defun tokenp (expected-type &optional expected-content)
  (let ((token (current-token)))
    (when token
      (let ((type (token-type token))
            (content (token-content token)))
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
  (require-token (list type content))
  (next-token))

(defun length=1 (list) (and (consp list) (null (rest list))))

(defun require-token (&rest possible-tokens)
  (let ((token (current-token)))
    (unless (and token (some (lambda (candidate)
                               (apply #'tokenp candidate))
                             possible-tokens))
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
        (raise-error message)))))

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
    (t (left-assoc #'parse-or-term '((:punctuation "||"))))))

(defun make-prim0-parse-tree (primitive-name)
  (with-new-parse-tree :prim0
    (prog1
        primitive-name
      (next-token))))

(defun left-assoc (sub-parser operators)
  (labels ((iter (start)
             (if (some (lambda (operator) (apply #'tokenp operator)) operators)
                 (let ((operator (current-token)))
                   (iter (with-new-anchored-parse-tree
                             (parse-tree-start-pos start)
                             :call
                           (list (make-prim0-parse-tree (token-content operator))
                                 start
                                 (funcall sub-parser)))))
                 start)))
    (iter (funcall sub-parser))))

(defun parse-or-term () (left-assoc #'parse-and-term '((:punctuation "&&"))))

(defun parse-and-term ()
  (left-assoc #'parse-relational-term '((:punctuation "<")
                                        (:punctuation ">")
                                        (:punctuation "<=")
                                        (:punctuation ">=")
                                        (:punctuation "==")
                                        (:punctuation "!="))))

(defun parse-relational-term ()
  (left-assoc #'parse-addition-term '((:punctuation "+")
                                      (:punctuation "-")
                                      (:punctuation "|"))))

(defun parse-addition-term ()
  (left-assoc #'parse-multiplication-term '((:punctuation "*")
                                            (:punctuation "/")
                                            (:punctuation "%")
                                            (:punctuation "&")
                                            (:punctuation "^")
                                            (:punctuation ">>")
                                            (:punctuation "<<"))))

(defun parse-multiplication-term ()
  (cond ((tokenp :punctuation "-") (parse-unary-minus))
        ((tokenp :punctuation "!") (parse-logical-not))
        (t (parse-tight-unary))))

(defun parse-logical-not ()
  (with-new-parse-tree :call
    (require-token (list :punctuation "!"))
    (list (make-prim0-parse-tree "!") (parse-multiplication-term))))

(defun parse-unary-minus ()
  (with-new-parse-tree :call
    (require-token (list :punctuation "-"))
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
              (require-token (list :punctuation "."))
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
                             (require-token (list :punctuation "["))
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
        (setf message
              (format nil "~a~%~a" message (highlight-position source pos)))))
    (setf message (maybe-extend-message pos message))
    (error (make-condition 'shovel-compiler-error
                           :message message
                           :line line
                           :column column
                           :at-eof at-eof))))

(defun parse-parenthesized-or-name ()
  (cond ((tokenp :punctuation "(")  ; Handle parenthesized expression.
         (consume-token :punctuation "(")
         (prog1
             (parse-expression)
           (consume-token :punctuation ")")))
        ((tokenp :identifier) (parse-name))
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

(defparameter *required-primitives*
  '("pow"
    "array" "arrayN" "length" "slice"
    "hash" "keys" "hasKey"
    "utcSecondsSinceUnixEpoch" "decodeTime" "encodeTime"
    "isString" "isHash" "isBool" "isArray" "isNumber" "isCallable"
    "string" "stringRepresentation"
    "parseInt" "parseFloat"))

(defun is-required-primitive-name (str)
  (member str *required-primitives* :test #'string=))

(defparameter *reserved-keywords* '("var" "if" "fn" "return"))

(defun is-reserved-keyword (str)
  (member str *reserved-keywords* :test #'string=))

(defun parse-name (&optional (can-be-required-primitive t))
  (require-token (list :identifier))
  (let ((content (token-content (current-token))))
    (when (is-reserved-keyword content)
      (raise-error (format nil "'~a' is a reserved keyword." content)))
    (if (is-required-primitive-name content)
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
  (require-token (list :prim))
  (token-as-parse-tree :prim))

(defun parse-name-as-string ()
  (require-token (list :identifier))
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
