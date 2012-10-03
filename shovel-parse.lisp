
(in-package #:shovel-parse)

(defun parse-string (source)
  (let ((tokens (shovel-tokenize:tokenize-string source)))
    (parse tokens)))

(defstruct parse-state tokens (previous-token nil))

(defvar *parse-state* nil)

(defun current-token () (car (parse-state-tokens *parse-state*)))

(defun previous-token () (parse-state-previous-token *parse-state*))

(defun next-token () (setf (parse-state-previous-token *parse-state*)
                           (pop (parse-state-tokens *parse-state*))))

(defstruct parse-tree label start-pos end-pos children)

(defun parse (tokens)
  (let ((*parse-state* (make-parse-state :tokens tokens))
        result)
    (loop
       while (parse-state-tokens *parse-state*)
       do (push (parse-statement) result))
    result))

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

(defun tokenp (expected-type expected-content)
  (let ((token (current-token)))
    (when token
      (let ((type (token-type token))
            (content (token-content token)))
        (and (eq type expected-type)
             (or (null expected-content)
                 (string= content expected-content)))))))

(defun get-current-start-pos ()
  (token-start-pos (current-token)))

(defun get-previous-end-pos ()
  (token-end-pos (previous-token)))

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
  (require-token type content)
  (next-token))

(defun require-token (type content)
  (let ((token (current-token)))
    (unless (and token (tokenp type content))
      (if token
          (let ((start-pos (token-start-pos token)))
            (error
             "Shovel parse error at line ~d column ~d: expected '~a' but got '~a'."
             (pos-line start-pos)
             (pos-column start-pos)
             content
             (token-content token)))
          (error
           "Shovel parse error: expected '~a' but reached the end of input."
           content)))))

(defun parse-var-decl ()
  (with-new-parse-tree :var
    (consume-token :identifier "var")
    (let ((lhs (parse-name)))
      (consume-token :punctuation "=")
      (let ((rhs (parse-expression)))
        (list lhs rhs)))))

(defun parse-return ()
  (with-new-parse-tree :return
    (consume-token :identifier "return")
    (parse-expression)))

(defun parse-assignment (lhs)
  (with-new-anchored-parse-tree (parse-tree-start-pos lhs) :set!
    (consume-token :identifier "=")
    (list lhs (parse-expression))))

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
    (t (right-assoc #'parse-assignment-term '((:punctuation "="))))))

(defun right-assoc (sub-parser operators)
  (let ((start (funcall sub-parser)))
    (if (some (lambda (operator) (apply #'tokenp operator)) operators)
        (let ((operator (current-token)))
          (with-new-anchored-parse-tree
              (parse-tree-start-pos start) (list :prim0 (token-content operator))
            (next-token)
            (list start (right-assoc sub-parser operators))))
        start)))

(defun parse-assignment-term ()
  (left-assoc #'parse-or-term '((:punctuation "||"))))

(defun left-assoc (sub-parser operators)
  (labels ((iter (start)
             (if (some (lambda (operator) (apply #'tokenp operator)) operators)
                 (let ((operator (current-token)))
                   (iter (with-new-anchored-parse-tree
                             (parse-tree-start-pos start)
                             (list :prim0 (token-content operator))
                           (next-token)
                           (list start (funcall sub-parser)))))
                 start)))
    (iter (funcall sub-parser))))

(defun parse-or-term ()
  (left-assoc #'parse-and-term '((:punctuation "&&"))))

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
                                            (:punctuation "&")
                                            (:punctuation ">>")
                                            (:punctuation "<<"))))

(defun parse-multiplication-term ()
  (if (tokenp :punctuation "-")
      (parse-unary-minus)
      (parse-unary-minus-term)))

(defun parse-unary-minus-term ()
  (cond ((tokenp :number nil) (parse-number))
        ((tokenp :string nil) (parse-literal-string))
        (t (parse-identifier-or-call-or-ref))))

(defun parse-identifier-or-call-or-ref (&optional forced-start)
  (let ((start (or forced-start (parse-parenthesized-or-name))))
    (cond ((tokenp :punctuation ".") ; Handle hash access by dotting.
           (parse-identifier-or-call-or-ref
            (with-new-anchored-parse-tree
                (parse-tree-start-pos start)
                (list :prim0 "svm_gref")
              (consume-token :punctuation ".")
              (list start (parse-name)))))
          ((tokenp :punctuation "(") ; Handle function call.
           (parse-identifier-or-call-or-ref
            (with-new-anchored-parse-tree (parse-tree-start-pos start) :call
              (consume-token :punctuation "(")
              (prog1
                  (cons start (parse-list #'parse-expression '(:punctuation ",")))
                (consume-token :punctuation ")")))))
          ((tokenp :punctuation "[") ; Handle array or hash access.
           (parse-identifier-or-call-or-ref
            (with-new-anchored-parse-tree 
                (parse-tree-start-pos start)
                (list :prim0 "svm_gref")
              (consume-token :punctuation "[")
              (prog1
                  (list start (parse-expression))
                (consume-token :punctuation "]")))))
          (t start))))

(defun parse-parenthesized-or-name ()
  (cond ((tokenp :punctuation "(") ; Handle parenthesized expression.
         (consume-token :punctuation "(")
         (prog1
             (parse-expression)
           (consume-token :punctuation ")")))
        ((tokenp :identifier nil) (parse-name))
        (t (error "Unexpected token '~a'." (token-content (current-token))))))

(defun parse-number ()
  (require-token :number nil)
  (with-new-parse-tree :number
    (prog1
        (token-content (current-token))
      (next-token))))

(defun parse-literal-string ()
  (require-token :string nil)
  (with-new-parse-tree :string
    (prog1
        (token-content (current-token))
      (next-token))))

(defun parse-unary-minus ()
  (with-new-parse-tree (list "prim0" "-")
    (consume-token :punctuation "-")
    (parse-unary-minus-term)))

(defun parse-lambda ()
  (with-new-parse-tree :fn
    (consume-token :identifier "fn")
    (let ((args (parse-lambda-args))
          (body (parse-statement)))
      (list args body))))

(defun parse-lambda-args ()
  (if (tokenp :punctuation "(")
      (with-new-parse-tree :list
        (consume-token :punctuation "(")
        (prog1
            (parse-list #'parse-name '(:punctuation ","))
          (consume-token :punctuation ")")))
      (parse-name)))

(defun parse-list (item-parser separator)
  (let (result)
    (loop
       (push (funcall item-parser) result)
       (unless (apply #'tokenp separator)
         (return))
       (apply #'consume-token separator))
    (reverse result)))

(defun parse-name ()
  (require-token :identifier nil)
  (with-new-parse-tree :name
    (prog1
        (token-content (current-token))
      (next-token))))

(defun parse-if ()
  (with-new-parse-tree :if
    (consume-token :identifier "if")
    (let ((pred (parse-expression))
          (then (parse-statement)))
      (if (tokenp :identifier "else")
          (progn
            (consume-token :identifier "else")
            (list pred then (parse-statement)))
          (list pred then)))))

(defun simplify-parse-tree (parse-tree)
  (if (parse-tree-p parse-tree)
      (cons (parse-tree-label parse-tree)
            (let ((children (parse-tree-children parse-tree)))
              (if (listp children)
                  (mapcar #'simplify-parse-tree children)
                  (list (simplify-parse-tree children)))))
      parse-tree))
