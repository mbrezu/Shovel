
(in-package #:shovel-codegen)

(defstruct instruction
  opcode (arguments nil)
  (start-pos nil) (end-pos nil) (comments nil))

(defstruct env-frame (vars nil))

(defvar *label-counter* 0)

(defun gen-label (&optional (symbol 'l))
  (mu-base:mksymb symbol (incf *label-counter*)))

(defun compile-ast (ast env val? more?)
  (when ast
    (let ((label (parse-tree-label ast)))
      (case label
        (:var (validate-var ast)
              (let ((var-name (name-identifier (var-name ast))))
                (extend-frame env var-name)
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
                        (gen :fn :arguments fn)
                        (unless more? (gen :return))))))
        (:begin (compile-block (parse-tree-children ast) env more?))
        (:set! (compile-set ast env val? more?))
        (:if (compile-if ast env val? more?))
        (:name (validate-name ast)
               (let ((var-name (name-identifier ast)))
                 (gen :lget :arguments (find-name var-name env) :pos ast)))
        (:call (compile-funcall ast env more?))
        (:prim0 (gen :prim0 :arguments (parse-tree-children ast)
                     :pos ast))
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
               (error "Assignment only supported for names, arrays and hashes."))
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
              (string= "svm_gref" (parse-tree-children fn))))))

(defun gref-call-array-or-hash (ast) (second (parse-tree-children ast)))

(defun gref-call-index (ast) (third (parse-tree-children ast)))

(defun compile-set-var (name env val? more? ast-for-pos)
  (seq (gen :lset :arguments (find-name name env) :pos ast-for-pos)
       (unless val? (gen :pop))
       (unless more? (gen :return))))

(defun compile-if (ast env val? more?)
  (validate-if ast)
  (if more?
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
             (compile-ast (if-else ast) env val? nil)))))

(defun compile-funcall (ast env more?)
  (let* ((children (parse-tree-children ast))
         (arg-code (mapcan (lambda (ast)
                             (compile-ast ast env t t))
                           (rest children)))
         (function-code (compile-ast (first children) env t t)))
    (if more?
        (seq arg-code
             function-code
             (gen :call :arguments (length (rest children)) :pos ast))
        (seq arg-code
             function-code
             (gen :callj :arguments (length (rest children)) :pos ast)))))

(defun validate-name (name-ast)
  (or (name-p name-ast)
      (error "Invalid name ~a." name-ast)))

(defun if-pred (if-ast) (first (parse-tree-children if-ast)))

(defun if-then (if-ast) (second (parse-tree-children if-ast)))

(defun if-else (if-ast) (third (parse-tree-children if-ast)))

(defun if-p (if-ast)
  (and (eq :if (parse-tree-label if-ast))
       (let ((children-count (length (parse-tree-children if-ast))))
         (or (= 2 children-count)
             (= 3 children-count)))))

(defun validate-if (if-ast)
  (or (if-p if-ast) (error "Invalid IF ~a." if-ast)))

(defun validate-set (set-ast)
  (or (set-p set-ast) (error "Invalid SET! ~a." set-ast)))

(defun set-p (set-ast)
  (and (eq :set! (parse-tree-label set-ast))
       (= 3 (length (parse-tree-children set-ast)))))

(defun set-left-side (set-ast) (first (parse-tree-children set-ast)))

(defun set-operator (set-ast) (second (parse-tree-children set-ast)))

(defun set-right-side (set-ast) (third (parse-tree-children set-ast)))

(defun show-bytecode (source instructions)
  (let ((source-lines (split-sequence:split-sequence #\newline source)))
    (dolist (instruction instructions)
      (let ((opcode (instruction-opcode instruction))
            (args (instruction-arguments instruction)))
        (alexandria:when-let ((start-pos (instruction-start-pos instruction))
                              (end-pos (instruction-end-pos instruction)))
          (print-source-as-comment source-lines start-pos end-pos))
        (cond ((eq :label opcode)
               (format t "~a:" args))
              (t (if (consp args )
                     (format t "    ~a ~{~a~^, ~}" opcode args)
                     (if args
                         (format t "    ~a ~a" opcode args)
                         (format t "    ~a" opcode)))))
        (alexandria:when-let (comments (instruction-comments instruction))
          (format t "~40,8T ; ~a" comments))
        (terpri)))))

(defun print-source-as-comment (source-lines start-pos end-pos)
  (labels ((underline (start end)
             (with-output-to-string (str)
               (loop repeat (1- start) do (write-char #\space str))
               (loop repeat (1+ (- end start)) do (write-char #\^ str))))
           (first-non-blank (line)
             (1+ (or (position-if (lambda (ch)
                                    (and (char/= ch #\space)
                                         (char/= ch #\tab)))
                                  line)
                     0))))
    (let* ((start-line (pos-line start-pos))
           (end-line (pos-line end-pos))
           (add-elipsis (> end-line start-line))
           (first-line (elt source-lines (1- start-line))))
      (format t "    ; line ~5d: ~a" start-line first-line)
      (when add-elipsis
        (format t " [...content snipped...]"))
      (terpri)
      (format t "    ; line ~5d: ~a~%"
              start-line
              (underline (max (pos-column start-pos)
                              (first-non-blank first-line))
                         (min (length first-line)
                              (if add-elipsis
                                  (length first-line)
                                  (pos-column end-pos))))))))

(defun empty-env () (list (make-env-frame)))

(defun comp (source)
  (setf *label-counter* 0)
  (show-bytecode source (compile-block (parse-string source) (empty-env) nil)))

(defun gen-code (ast)
  (assemble (compile-fn-body nil ast (empty-env))))

(defun assemble (instructions)
  (multiple-value-bind (length labels-hash)
      (assemble-pass-1 instructions)
    (assemble-pass-2 instructions length labels-hash)))

(defun assemble-pass-1 (instructions)
  (let ((length 0)
        (labels-hash (make-hash-table)))
    (dolist (instruction instructions)
      (if (eq :label (instruction-opcode instruction))
          (setf (gethash (instruction-arguments instruction) labels-hash) length)
          (incf length)))
    (values length labels-hash)))

(defun assemble-pass-2 (instructions length labels-hash)
  (let ((result (make-array length))
        (current 0))
    (dolist (instruction instructions)
      (let ((opcode (instruction-opcode instruction))
            (args (instruction-arguments instruction)))
        (case opcode
          ((:tjump :fjump :jump :fn)
           (setf (aref result current)
                 (make-instruction :opcode opcode
                                   :arguments (gethash args labels-hash)))
           (incf current))
          (:label)
          (t (setf (aref result current)
                   instruction)
             (incf current)))))
    result))

(defun last1 (list) (first (last list)))

(defun compile-block (ast env more?)
  (let* ((new-env (cons (make-env-frame) env))
         (drop-values-asts (butlast ast))
         (value-ast (last1 ast))
         (compiled-body (seq (mapcan (lambda (ast)
                                       (compile-ast ast new-env nil t))
                                     drop-values-asts)
                             (compile-ast value-ast new-env t more?)))
         (top-frame-count (length (env-frame-vars (car new-env)))))
    (if (> top-frame-count 0)
        (seq (gen :new-frame :arguments top-frame-count)
             compiled-body
             (if more?
                 (gen :drop-frame)))
        (seq compiled-body))))

(defun compile-fn-body (args body env)
  (let ((new-env (cons (make-env-frame) env)))
    (dolist (arg args)
      (extend-frame new-env (name-identifier arg)))
    (let ((compiled-body (compile-ast body new-env t nil))
          (top-frame-count (length (env-frame-vars (car new-env)))))
      (seq (if (> top-frame-count 0)
               (seq (gen :new-frame :arguments top-frame-count)
                    (gen :args :arguments (length args))))
           compiled-body))))

(defun find-name (name env &optional (frame-number 0))
  (when (null env)
    (error "Undefined variable ~a." name))
  (let ((pair (assoc name (env-frame-vars (first env)) :test #'equal)))
    (if pair
        (list frame-number (cdr pair))
        (find-name name (rest env) (1+ frame-number)))))

(defun validate-fn (fn-ast)
  (or (fn-p fn-ast)
      (error "Invalid fn ~a." fn-ast)))

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
      (error "Invalid var declaration ~a." var-ast)))

(defun name-p (name)
  (and (eq :name (parse-tree-label name))
       (stringp (parse-tree-children name))))

(defun name-identifier (name) (parse-tree-children name))

(defun extend-frame (env name)
  (let ((top-frame (car env)))
    (unless (assoc name (env-frame-vars top-frame) :test #'equal)
      (setf (env-frame-vars top-frame)
            (cons (cons name (length (env-frame-vars top-frame)))
                  (env-frame-vars top-frame))))))

(defun compile-atom (ast env val? more?)
  (declare (ignore env))
  (validate-atom-value ast)
  (if val?
      (seq (gen :const :arguments (parse-tree-children ast) :pos ast)
           (unless more? (gen :return)))))

(defun validate-atom-value (ast)
  (or (and (member (parse-tree-label ast) '(:string :number :bool :void))
           (stringp (parse-tree-children ast)))
      (error "Invalid value ~a." ast)))

(defun gen (opcode &key (arguments nil) (pos nil) (comments nil))
  (list (make-instruction :opcode opcode
                          :arguments arguments
                          :start-pos (if pos (parse-tree-start-pos pos))
                          :end-pos (if pos (parse-tree-end-pos pos))
                          :comments comments)))

(defun seq (&rest subsequences) (apply #'append subsequences))
