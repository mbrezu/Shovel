;;;; shovel.lisp

(in-package #:shovel)

(defstruct instruction opcode (arguments nil))

(defstruct retaddr env pc)

(defstruct value type value)

(defstruct env-frame (vars nil))

(defvar *label-counter* 0)

(defun gen-label (&optional (symbol 'l))
  (mu-base:mksymb symbol (incf *label-counter*)))

(defun compile-ast (ast env val? more?)
  (cond ((null ast) nil)
        ((atom ast) (compile-atom ast env val? more?))
        (t (case (first ast)
             (:var (validate-var ast)
                   (extend-frame env (name-identifier (var-name ast)))
                   (seq (compile-ast (var-initializer ast) env t t)
                        (compile-set (name-identifier (var-name ast)) env
                                     t more?)))
             (:fn (validate-fn ast)
                  (if val?
                      (let ((fn (gen-label 'fn))
                            (l (gen-label)))
                        (seq (gen :jump l)
                             (gen :label fn)
                             (compile-fn-body (fn-args ast) (fn-body ast) env)
                             (gen :label l)
                             (gen :fn fn)
                             (unless more? (gen 'return))))))
             (:begin (compile-block (rest ast) env more?))
             (:set! (validate-set ast)
                    ;; To be removed when we support array/hash assignment.
                    (validate-name (set-left-side ast))
                    (seq (compile-ast (set-right-side ast) env t t)
                         (compile-set (name-identifier (set-left-side ast))
                                      env t more?)))
             (:if (compile-if ast env val? more?))
             (:name (validate-name ast)
                    (gen :lget (find-name (name-identifier ast) env)))
             (:prim (gen :prim (second ast)))
             (t ;; Function call.
              (compile-funcall ast env more?))))))

(defun compile-if (ast env val? more?)
  (validate-if ast)
  (if more?
      (let ((l1 (gen-label))
            (l2 (gen-label)))
        (seq (compile-ast (if-pred ast) env t t)
             (gen :fjump l1)
             (compile-ast (if-then ast) env val? t)
             (gen :jump l2)
             (gen :label l1)
             (compile-ast (if-else ast) env val? t)
             (gen :label l2)))
      (let ((l1 (gen-label)))
        (seq (compile-ast (if-pred ast) env t t)
             (gen :fjump l1)
             (compile-ast (if-then ast) env val? nil)
             (gen :label l1)
             (compile-ast (if-else ast) env val? nil)))))

(defun compile-funcall (ast env more?)
  (let ((arg-code (mapcan (lambda (ast)
                            (compile-ast ast env t t))
                          (rest ast)))
        (function-code (compile-ast (first ast) env t t)))
    (if more?
        (let ((k (gen-label 'k)))
          (seq (gen :save k)
               arg-code
               function-code
               (gen :callj (length (rest ast)))
               (gen :label k)))
        (seq arg-code
             function-code
             (gen :callj (length (rest ast)))))))

(defun validate-name (name-ast)
  (or (name-p name-ast)
      (error "We only support assignment to symbols for now.")))

(defun if-pred (if-ast)
  (second if-ast))

(defun if-then (if-ast)
  (third if-ast))

(defun if-else (if-ast)
  (fourth if-ast))

(defun if-p (if-ast)
  (and (or (= 3 (length if-ast))
           (= 2 (length if-ast)))
       (eq :if (first if-ast))))

(defun validate-if (if-ast)
  (or (if-p if-ast)
      (error "Invalid IF ~a." if-ast)))

(defun validate-set (set-ast)
  (or (set-p set-ast)
      (error "Invalid SET! ~a." set-ast)))

(defun set-p (set-ast)
  (and (= 3 (length set-ast))
       (eq :set! (first set-ast))))

(defun set-left-side (set-ast)
  (second set-ast))

(defun set-right-side (set-ast)
  (third set-ast))

(defun show-bytecode (instructions)
  (dolist (instruction instructions)
    (let ((opcode (instruction-opcode instruction))
          (args (instruction-arguments instruction)))
      (cond ((eq :label opcode)
             (format t "~a:~%" args))
            (t (if (consp args )
                   (format t "    ~a ~{~a~^, ~}~%" opcode args)
                   (if args
                       (format t "    ~a ~a~%" opcode args)
                       (format t "    ~a~%" opcode))))))))

(defun empty-env ()
  (list (make-env-frame)))

(defun comp (ast)
  (setf *label-counter* 0)
  (show-bytecode (compile-fn-body nil ast (empty-env))))

(defun last1 (list)
  (first (last list)))

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
        (seq (gen :new-frame top-frame-count)
             compiled-body
             (if more?
                 (gen :drop-frame)))
        (seq compiled-body))))

(defun compile-fn-body (args body env)
  (let* ((new-env (cons (make-env-frame) env))
         (compiled-body (compile-ast body new-env t nil))
         (top-frame-count (length (env-frame-vars (car new-env)))))
    (dolist (arg args)
      (extend-frame new-env (name-identifier arg)))
    (seq (gen 'new-frame top-frame-count)
         (gen 'args (length args))
         compiled-body)))

(defun compile-set (name env val? more?)
  (if val?
      (seq (gen 'lset (find-name name env))
           (unless more? (gen 'return)))))

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
  (and (consp fn-ast)
       (eq :fn (first fn-ast))
       (consp (second fn-ast))
       (= 3 (length fn-ast))
       (every #'name-p (fn-args fn-ast))))

(defun fn-args (fn-ast)
  (second fn-ast))

(defun fn-body (fn-ast)
  (third fn-ast))

(defun var-name (var-ast)
  (second var-ast))

(defun var-initializer (var-ast)
  (third var-ast))

(defun var-p (var-ast)
  (and (= 3 (length var-ast))
       (eq :var (first var-ast))
       (name-p (var-name var-ast))))

(defun validate-var (var-ast)
  (or (var-p var-ast)
      (error "Invalid var declaration ~a." var-ast)))

(defun name-p (name)
  (and (consp name)
       (eq :name (car name))
       (stringp (second name))))

(defun name-identifier (name)
  (second name))

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
      (seq (gen 'const ast)
           (unless more? (gen 'return)))))

(defun validate-atom-value (ast)
  (or (and (atom ast)
           (or (stringp ast)
               (numberp ast)
               (and (symbolp ast)
                    (or (eq ast 'true)
                        (eq ast 'false)))))
      (error "Invalid value ~a." ast)))

(defun gen (opcode &optional arguments)
  (list (make-instruction :opcode opcode :arguments arguments)))

(defun seq (&rest subsequences)
  (apply #'append subsequences))
