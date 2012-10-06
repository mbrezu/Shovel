
(in-package #:shovel-compiler-code-generator)

(defstruct env-frame (vars nil))

(defstruct generator-state label-counter source)

(defvar *generator-state*)

(defun gen-label (&optional (symbol 'l))
  (mu-base:mksymb symbol (incf (generator-state-label-counter *generator-state*))))

(defun compile-ast (ast env val? more?)
  (when ast
    (let ((label (parse-tree-label ast)))
      (case label
        (:var (validate-var ast)
              (let ((var-name (name-identifier (var-name ast))))
                (extend-frame env var-name (var-name ast))
                (seq (compile-ast (var-initializer ast) env t t)
                     (compile-set-var var-name env val? more? ast))))
        (:fn (validate-fn ast)
             (if val?
                 (let ((fn (gen-label 'fn))
                       (l (gen-label)))
                   (seq (gen :jump :arguments l)
                        (gen :label :arguments fn :pos ast)
                        (compile-fn-body (fn-args ast) (fn-body ast) env)
                        (gen :label :arguments l)
                        (gen :fn :arguments (list fn (length (fn-args ast))))
                        (unless more? (gen :return))))))
        (:begin (compile-block (parse-tree-children ast) env val? more?))
        (:set! (compile-set ast env val? more?))
        (:if (compile-if ast env val? more?))
        (:name (validate-name ast)
               (let ((var-name (name-identifier ast)))
                 (seq (gen :lget :arguments (find-name var-name env
                                                       (parse-tree-start-pos ast))
                           :pos ast)
                      (unless more? (gen :return))
                      (unless val? (gen :pop)))))
        (:call (compile-funcall ast env val? more?))
        (:prim0 (seq (gen :prim0 :arguments (parse-tree-children ast) :pos ast)
                     (unless more? (gen :return))
                     (unless val? (gen :pop))))
        (:prim (seq (gen :prim :arguments (parse-tree-children ast) :pos ast)
                    (unless more? (gen :return))
                    (unless val? (gen :pop))))
        (:return (seq (compile-ast (parse-tree-children ast) env t more?)
                      (when more? (gen :return))))
        ((:number :string :bool :void) (compile-atom ast env val? more?))
        (t (error "Shovel internal WTF."))))))

(defun compile-set (ast env val? more?)
  "Compiles an assignment to a variable or an element of an array or
hash. For the second case, it rewrites the AST to use the
SVM_SET_INDEXED required primitive."
  (cond ((name-p (first (parse-tree-children ast)))
         (validate-set ast)
         (seq (compile-ast (set-right-side ast) env t t)
              (compile-set-var (name-identifier (set-left-side ast))
                               env val? more? ast)))
        (t (let ((lhs (set-left-side ast)))
             (unless (is-gref-call lhs)
               (raise-error
                (parse-tree-start-pos ast)
                "Assignment only supported for names, arrays and hashes at line ~d, column ~d."))
             (let* ((array-or-hash (gref-call-array-or-hash lhs))
                    (index (gref-call-index lhs))
                    (set-operator (set-operator ast))
                    (primitive-pt (make-parse-tree
                                   :label :prim0
                                   :start-pos (parse-tree-start-pos set-operator)
                                   :end-pos (parse-tree-end-pos set-operator)
                                   :children "svm_set_indexed")))
               (compile-ast (make-parse-tree :label :call
                                             :start-pos (parse-tree-start-pos ast)
                                             :end-pos (parse-tree-end-pos ast)
                                             :children (list primitive-pt
                                                             array-or-hash
                                                             index
                                                             (set-right-side ast)))
                            env val? more?))))))

(defun is-gref-call (ast)
  (and (eq :call (parse-tree-label ast))
       (let ((fn (first (parse-tree-children ast))))
         (and (eq :prim0 (parse-tree-label fn))
              (or (string= "svm_gref" (parse-tree-children fn))
                  (string= "svm_gref_dot" (parse-tree-children fn)))))))

(defun gref-call-array-or-hash (ast) (second (parse-tree-children ast)))

(defun gref-call-index (ast) (third (parse-tree-children ast)))

(defun compile-set-var (name env val? more? ast-for-pos)
  (seq (gen :lset
            :arguments (find-name name env (parse-tree-start-pos ast-for-pos))
            :pos ast-for-pos)
       (unless val? (gen :pop))
       (unless more? (gen :return))))

(defun compile-if (ast env val? more?)
  (validate-if ast)
  (seq (if more?
           (let ((l1 (gen-label))
                 (l2 (gen-label)))
             (seq (compile-ast (if-pred ast) env t t)
                  (gen :fjump :arguments l1)
                  (compile-ast (if-then ast) env val? t)
                  (gen :jump :arguments l2)
                  (gen :label :arguments l1)
                  (compile-ast (if-else ast) env val? t)
                  (gen :label :arguments l2)))
           (let ((l1 (gen-label)))
             (seq (compile-ast (if-pred ast) env t t)
                  (gen :fjump :arguments l1)
                  (compile-ast (if-then ast) env val? nil)
                  (gen :label :arguments l1)
                  (compile-ast (if-else ast) env val? nil))))
       (unless val? (gen :pop))))

(defun compile-funcall (ast env val? more?)
  (let* ((children (parse-tree-children ast))
         (arg-code (mapcan (lambda (ast)
                             (compile-ast ast env t t))
                           (rest children)))
         (function-code (compile-ast (first children) env t t)))
    (if more?
        (seq arg-code
             function-code
             (gen :call :arguments (length (rest children)) :pos ast)
             (unless val? (gen :pop)))
        (seq arg-code
             function-code
             (gen :callj :arguments (length (rest children)) :pos ast)))))

(defun validate-name (name-ast)
  (or (name-p name-ast)
      (error "Internal Shovel WTF.")))

(defun if-pred (if-ast) (first (parse-tree-children if-ast)))

(defun if-then (if-ast) (second (parse-tree-children if-ast)))

(defun if-else (if-ast) (third (parse-tree-children if-ast)))

(defun if-p (if-ast)
  (and (eq :if (parse-tree-label if-ast))
       (let ((children-count (length (parse-tree-children if-ast))))
         (or (= 2 children-count)
             (= 3 children-count)))))

(defun validate-if (if-ast)
  (or (if-p if-ast) (error "Internal Shovel WTF.")))

(defun validate-set (set-ast)
  (or (set-p set-ast) (error "Internal Shovel WTF.")))

(defun set-p (set-ast)
  (and (eq :set! (parse-tree-label set-ast))
       (= 3 (length (parse-tree-children set-ast)))))

(defun set-left-side (set-ast) (first (parse-tree-children set-ast)))

(defun set-operator (set-ast) (second (parse-tree-children set-ast)))

(defun set-right-side (set-ast) (third (parse-tree-children set-ast)))

(defun empty-env () (list (make-env-frame)))

(defun generate-instructions (ast &key source)
  (let ((*generator-state* (make-generator-state :label-counter 0
                                                 :source source)))
    (compile-block ast (empty-env) t t)))

(defun last1 (list) (first (last list)))

(defun compile-block (ast env val? more?)
  (labels ((compile-statements (env drop-values-asts value-ast)
             (seq (mapcan (lambda (ast)
                            (compile-ast ast env nil t))
                          drop-values-asts)
                  (compile-ast value-ast env t more?))))
    (let* ((new-frame (make-env-frame))
           (new-env (cons new-frame env))
           (drop-values-asts (butlast ast))
           (value-ast (last1 ast))
           (compiled-body (compile-statements new-env drop-values-asts value-ast))
           (vars (reverse (mapcar #'first (env-frame-vars new-frame)))))
      (if vars
          (seq (gen :new-frame :arguments vars)
               compiled-body
               (if more? (gen :drop-frame))
               (unless val? (gen :pop)))
          (seq (compile-statements env drop-values-asts value-ast)
               (unless val? (gen :pop)))))))

(defun compile-fn-body (args body env)
  (let* ((new-frame (make-env-frame))
         (new-env (cons new-frame env)))
    (dolist (arg args)
      (extend-frame new-env (name-identifier arg) arg))
    (let ((compiled-body (compile-ast body new-env t nil))
          (vars (reverse (mapcar #'first (env-frame-vars new-frame)))))
      (if vars
          (seq (gen :new-frame :arguments vars)
               (gen :args :arguments (length args))
               compiled-body)
          (compile-ast body env t nil)))))

(defun validate-fn (fn-ast)
  (or (fn-p fn-ast)
      (error "Internal Shovel WTF.")))

(defun fn-p (fn-ast)
  (and (eq :fn (parse-tree-label fn-ast))
       (= 2 (length (parse-tree-children fn-ast)))
       (every #'name-p (fn-args fn-ast))))

(defun fn-args (fn-ast) (parse-tree-children (first (parse-tree-children fn-ast))))

(defun fn-body (fn-ast) (second (parse-tree-children fn-ast)))

(defun var-name (var-ast) (first (parse-tree-children var-ast)))

(defun var-initializer (var-ast) (second (parse-tree-children var-ast)))

(defun var-p (var-ast)
  (and (eq :var (parse-tree-label var-ast))
       (= 2 (length (parse-tree-children var-ast)))
       (name-p (var-name var-ast))))

(defun validate-var (var-ast)
  (or (var-p var-ast)
      (error "Internal Shovel WTF.")))

(defun name-p (name)
  (and (eq :name (parse-tree-label name))
       (stringp (parse-tree-children name))))

(defun name-identifier (name) (parse-tree-children name))

(defun raise-error (pos message)
  (alexandria:when-let (source (generator-state-source *generator-state*))
    (setf message
          (format nil "~a~%~a" message (highlight-position source pos))))
  (error (make-condition 'shovel-error
                         :message message
                         :line (pos-line pos)
                         :column (pos-column pos))))

(defun extend-frame (env name name-ast)
  (let ((top-frame (car env))
        (current-start-pos (parse-tree-start-pos name-ast)))
    (alexandria:when-let
        (var-record (assoc name (env-frame-vars top-frame) :test #'equal))
      (raise-error current-start-pos
                   (format nil
                           "Variable '~a' is already defined in this frame at line ~d, column ~d."
                           name
                           (pos-line (third var-record))
                           (pos-column (third var-record)))))
    (setf (env-frame-vars top-frame)
          (cons (list name (length (env-frame-vars top-frame)) current-start-pos)
                (env-frame-vars top-frame)))))

(defun find-name (name env start-pos &optional (frame-number 0))
  (when (null env)
    (raise-error start-pos (format nil "Undefined variable '~a'." name)))
  (let ((pair (assoc name (env-frame-vars (first env)) :test #'equal)))
    (if pair
        (list frame-number (second pair))
        (find-name name (rest env) start-pos (1+ frame-number)))))

(defun compile-atom (ast env val? more?)
  (declare (ignore env))
  (validate-atom-value ast)
  (if val?
      (seq (gen :const
                :arguments (compile-atom-value (parse-tree-label ast)
                                               (parse-tree-children ast))
                :pos ast)
           (unless more? (gen :return)))))

(defun compile-atom-value (label value)
  (case label
    (:number (read-from-string value))
    (:string (subseq value 1 (1- (length value))))
    (:bool (cond ((string= value "true") :true)
                 ((string= value "false") :false)
                 (t (error "Shovel internal WTF."))))
    (:void :null)))

(defun validate-atom-value (ast)
  (or (and (member (parse-tree-label ast) '(:string :number :bool :void))
           (stringp (parse-tree-children ast)))
      (error "Internal Shovel WTF.")))

(defun gen (opcode &key (arguments nil) (pos nil) (comments nil))
  (list (make-instruction :opcode opcode
                          :arguments arguments
                          :start-pos (if pos (parse-tree-start-pos pos))
                          :end-pos (if pos (parse-tree-end-pos pos))
                          :comments comments)))

(defun seq (&rest subsequences) (apply #'append subsequences))
