
(in-package #:shovel-compiler-code-generator)

(defstruct env-frame (vars nil))

(declaim (optimize speed))
(defstruct generator-state
  (label-counter 0 :type fixnum)
  source
  (instructions nil)
  (file-name nil))
(declaim (optimize (speed 1)))

(defvar *generator-state*)

(defun gen-label (&optional (symbol 'l))
  (declare (optimize speed (safety 0)))
  (alexandria:format-symbol
   nil "~a~a" symbol
   (incf (generator-state-label-counter *generator-state*))))

(defun compile-ast (ast env val? more?)
  (declare (optimize speed))
  (when ast
    (let ((label (parse-tree-label ast)))
      (case label
        (:file-name (setf (generator-state-file-name *generator-state*)
                          (parse-tree-children ast))
                    (gen :file-name :arguments (parse-tree-children ast)))
        (:var (compile-var ast env val? more?))
        (:fn (compile-fn ast env val? more?))
        (:begin (compile-block (parse-tree-children ast) env val? more?))
        (:set! (compile-set ast env val? more?))
        (:if (compile-if ast env val? more?))
        (:name (compile-name ast env val? more?))
        (:call (compile-funcall ast env val? more?))
        (:prim0 (gen :prim0 :arguments (parse-tree-children ast) :pos ast)
                (unless more? (gen :return))
                (unless val? (gen :pop)))
        (:prim (gen :prim :arguments (parse-tree-children ast) :pos ast)
               (unless more? (gen :return))
               (unless val? (gen :pop)))
        ((:number :string :bool :void) (compile-atom ast env val? more?))
        (:named-block (compile-named-block ast env more?))
        (:block-return (compile-block-return ast env))
        (:context (gen :context :pos ast)
                  (unless more? (gen :return))
                  (unless val? (gen :pop)))
        (t (error "Shovel internal WTF."))))))

(defun compile-var (ast env val? more?)
  (declare (optimize speed (safety 0)))
  (validate-var ast)
  (locally (declare (type parse-tree ast))
    (let ((var-name (name-identifier (var-name ast))))
      (extend-frame env var-name (var-name ast))
      (compile-ast (var-initializer ast) env t t)
      (compile-set-var var-name env val? more? ast))))

(defun compile-name (ast env val? more?)
  (declare (optimize speed)
           (type parse-tree ast))
  (validate-name ast)
  (let ((var-name (name-identifier ast)))
    (gen :lget :arguments (find-name var-name env
                                     (parse-tree-start-pos ast)
                                     (parse-tree-end-pos ast))
         :pos ast)
    (unless more? (gen :return))
    (unless val? (gen :pop))))

(defun compile-fn (ast env val? more?)
  (validate-fn ast)
  (if val?
      (let ((fn (gen-label 'fn))
            (l (gen-label)))
        (gen :jump :arguments l)
        (gen :label :arguments fn :pos ast)
        (compile-fn-body (fn-args ast) (fn-body ast) env)
        (gen :label :arguments l)
        (gen :fn :arguments (list fn (length (the list (fn-args ast)))))
        (unless more? (gen :return)))))

(defun compile-block-return (ast env)
  (let ((block-name (first (parse-tree-children ast)))
        (result-expression (second (parse-tree-children ast))))
    (compile-ast block-name env t t)
    (compile-ast result-expression env t t)
    (gen :block-return :pos ast)))

(defun compile-named-block (ast env more?)
  (let ((block-name (first (parse-tree-children ast)))
        (block-contents (second (parse-tree-children ast)))
        (block-end (gen-label 'be)))
    (compile-ast block-name env t t)
    (gen :block :arguments block-end :pos ast)
    (compile-ast block-contents env t t)
    (gen :label :arguments block-end)
    (gen :pop-block :pos ast)
    (unless more? (gen :return))))

(defun compile-set (ast env val? more?)
  "Compiles an assignment to a variable or an element of an array or
hash. For the second case, it rewrites the AST to use the
SVM_SET_INDEXED required primitive."
  (cond ((name-p (first (parse-tree-children ast)))
         (validate-set ast)
         (compile-ast (set-right-side ast) env t t)
         (compile-set-var (name-identifier (set-left-side ast))
                          env val? more? ast))
        (t (let ((lhs (set-left-side ast)))
             (unless (is-gref-call lhs)
               (raise-error
                (parse-tree-start-pos ast)
                (parse-tree-end-pos ast)
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
  (declare (optimize speed (safety 0))
           (type parse-tree ast-for-pos))
  (gen :lset
       :arguments (find-name name env
                             (parse-tree-start-pos ast-for-pos)
                             (parse-tree-end-pos ast-for-pos))
       :pos ast-for-pos)
  (unless val? (gen :pop))
  (unless more? (gen :return)))

(defun compile-if (ast env val? more?)
  (validate-if ast)
  (if more?
      (let ((l1 (gen-label))
            (l2 (gen-label)))
        (compile-ast (if-pred ast) env t t)
        (gen :fjump :arguments l1)
        (compile-ast (if-then ast) env val? t)
        (gen :jump :arguments l2)
        (gen :label :arguments l1)
        (compile-ast (if-else ast) env val? t)
        (gen :label :arguments l2))
      (let ((l1 (gen-label)))
        (compile-ast (if-pred ast) env t t)
        (gen :fjump :arguments l1)
        (compile-ast (if-then ast) env val? nil)
        (gen :label :arguments l1)
        (compile-ast (if-else ast) env val? nil))))

(defun compile-funcall (ast env val? more?)
  (declare (optimize speed (safety 0))
           (type parse-tree ast))
  (let* ((children (parse-tree-children ast)))
    (dolist (arg (rest children))
      (compile-ast arg env t t))
    (compile-ast (first children) env t t)
    (if more?
        (progn
          (gen :call :arguments (length (the list (rest children))) :pos ast)
          (unless val? (gen :pop)))
        (gen :callj :arguments (length (the list (rest children))) :pos ast))))

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
    (gen :vm-version :arguments shovel-vm:*version*)
    (gen :vm-sources-md5 :arguments (shovel-compiler:compute-sources-md5 source))
    (gen :vm-bytecode-md5 :arguments "?")
    (when (and (first ast) (eq :file-name (parse-tree-label (first ast))))
      (compile-ast (first ast) (empty-env) t t)
      (pop ast))
    (compile-block ast (empty-env) t t)
    (reverse (generator-state-instructions *generator-state*))))

(defun last1 (list) (first (last list)))

(defun compile-statements (env drop-values-asts value-ast more?)
  (declare (optimize speed))
  (dolist (ast drop-values-asts)
    (compile-ast ast env nil t))
  (compile-ast value-ast env t more?))

(defun compile-block (ast env val? more?)
  (declare (optimize speed))
  (let* ((new-frame (make-env-frame))
         (new-env (cons new-frame env))
         (drop-values-asts (butlast ast))
         (value-ast (last1 ast))
         (vars (remove-if (lambda (stmt)
                            (not (var-p stmt)))
                          ast))
         (var-names (mapcar (lambda (var)
                              (name-identifier (var-name var)))
                            vars)))
    (cond ((> (length var-names) 0)
           (gen :new-frame
                :arguments var-names
                :start-pos (parse-tree-start-pos (first vars))
                :end-pos (parse-tree-end-pos (last1 vars)))
           (compile-statements new-env drop-values-asts value-ast more?)
           (if more? (gen :drop-frame))
           (unless val? (gen :pop)))
          (t (compile-statements env drop-values-asts value-ast more?)
             (unless val? (gen :pop))))))

(defun compile-fn-body (args body env)
  (declare (optimize speed))
  (let* ((new-frame (make-env-frame))
         (new-env (cons new-frame env)))
    (dolist (arg args)
      (extend-frame new-env (name-identifier arg) arg))
    (let ((var-names (mapcar #'name-identifier args)))
      (cond ((> (length (the list var-names)) 0)
             (gen :new-frame
                  :arguments var-names
                  :start-pos (parse-tree-start-pos (first args))
                  :end-pos (parse-tree-end-pos (last1 args)))
             (gen :args :arguments (length (the list args)))
             (compile-ast body new-env t nil))
            (t (compile-ast body env t nil))))))

(defun validate-fn (fn-ast)
  (or (fn-p fn-ast)
      (error "Internal Shovel WTF.")))

(defun fn-p (fn-ast)
  (and (eq :fn (parse-tree-label fn-ast))
       (= 2 (length (parse-tree-children fn-ast)))
       (every #'name-p (fn-args fn-ast))))

(defun fn-args (fn-ast) (parse-tree-children (first (parse-tree-children fn-ast))))

(defun fn-body (fn-ast) (second (parse-tree-children fn-ast)))

(declaim (inline var-name))
(defun var-name (var-ast)
  (declare (optimize speed (safety 0))
           (type parse-tree var-ast))
  (car (parse-tree-children var-ast)))

(declaim (inline var-name))
(defun var-initializer (var-ast)
  (declare (optimize speed (safety 0))
           (type parse-tree var-ast))
  (cadr (parse-tree-children var-ast)))

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

(declaim (inline name-identifier))
(defun name-identifier (name)
  (declare (optimize speed (safety 0))
           (type parse-tree name))
  (parse-tree-children name))

(defun raise-error (character-start-pos character-end-pos message)
  (let (start-pos end-pos error-file-name line column)
    (alexandria:when-let* ((source (generator-state-source *generator-state*))
                           (file-name (generator-state-file-name *generator-state*))
                           (source-file (find-source source file-name))
                           (content (shovel:source-file-contents source-file)))
      (setf error-file-name file-name)
      (setf start-pos (find-position file-name
                                     content
                                     character-start-pos)
            line (pos-line start-pos)
            column (pos-column start-pos)
            end-pos (find-position file-name
                                   content
                                   character-end-pos))
      (let ((lines (extract-relevant-source source start-pos end-pos)))
        (setf message
              (format nil "~a~%~a~%~a"
                      message (first lines) (second lines)))))
    (error (make-condition 'shovel:shovel-error
                           :message message
                           :file error-file-name
                           :line line
                           :column column))))

(defun extend-frame (env name name-ast)
  (let ((top-frame (car env))
        (current-start-pos (parse-tree-start-pos name-ast)))
    (alexandria:when-let
        (var-record (assoc name (env-frame-vars top-frame) :test #'equal))
      (let* ((file-name (third var-record))
             (character-pos (fourth var-record))
             (source-file (find-source (generator-state-source *generator-state*)
                                       file-name))
             (pos (find-position file-name (shovel:source-file-contents source-file)
                                 character-pos)))
        (raise-error current-start-pos
                     (parse-tree-end-pos name-ast)
                     (format nil
                             "Variable '~a' is already defined in this frame in file '~s', at line ~d, column ~d."
                             name
                             file-name (pos-line pos) (pos-column pos)))))
    (setf (env-frame-vars top-frame)
          (cons (list name
                      (length (env-frame-vars top-frame))
                      (generator-state-file-name *generator-state*)
                      current-start-pos)
                (env-frame-vars top-frame)))))

(defun find-name (name env start-pos end-pos &optional (frame-number 0))
  (declare (optimize speed (safety 0))
           (type fixnum frame-number))
  (when (null env)
    (raise-error start-pos end-pos (format nil "Undefined variable '~a'." name)))
  (let ((pair (assoc name (env-frame-vars (first env)) :test #'equal)))
    (if pair
        (list frame-number (second pair))
        (find-name name (rest env) start-pos end-pos (1+ frame-number)))))

(defun compile-atom (ast env val? more?)
  (declare (ignore env))
  (validate-atom-value ast)
  (when val?
    (gen :const
         :arguments (compile-atom-value (parse-tree-label ast)
                                        (parse-tree-children ast))
         :pos ast)
    (unless more? (gen :return))))

(defun compile-atom-value (label value)
  (case label
    (:number (let ((result (read-from-string value)))
               (if (eq 'single-float (type-of result))
                   (coerce result 'double-float)
                   result)))
    (:string (subseq value 1 (1- (length value))))
    (:bool (cond ((string= value "true") :true)
                 ((string= value "false") :false)
                 (t (error "Shovel internal WTF."))))
    (:void :null)))

(defun validate-atom-value (ast)
  (or (and (member (parse-tree-label ast) '(:string :number :bool :void))
           (stringp (parse-tree-children ast)))
      (error "Internal Shovel WTF.")))

(declaim (inline gen))
(defun gen (opcode &key
                     (arguments nil)
                     (pos nil)
                     (comments nil)
                     (start-pos nil)
                     (end-pos nil))
  (declare (optimize speed (safety 0))
           (type (or null parse-tree) pos))
  (push (make-instruction :opcode opcode
                          :arguments arguments
                          :start-pos (or start-pos
                                         (if pos (parse-tree-start-pos pos)))
                          :end-pos (or end-pos
                                       (if pos (parse-tree-end-pos pos)))
                          :comments comments)
        (generator-state-instructions *generator-state*)))
