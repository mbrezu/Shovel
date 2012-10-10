
(in-package #:shovel-compiler)

(defun compile-sources-to-instructions (sources)
  (let* ((all-tokens (mapcar (lambda (shript-file)
                               (shovel-compiler-tokenizer:tokenize-source-file
                                shript-file))
                             sources))
         (parse-tree (mapcan (lambda (file-tokens)
                               (shovel-compiler-parser:parse-tokens
                                file-tokens :source sources))
                             all-tokens))
         (instructions
          (shovel-compiler-code-generator:generate-instructions
           parse-tree :source sources)))
    instructions))

(defun assemble-instructions (instructions)
  (labels ((assemble-pass-1 (instructions)
             (let ((length 0)
                   (labels-hash (make-hash-table)))
               (dolist (instruction instructions)
                 (if (eq :label (instruction-opcode instruction))
                     (setf (gethash (instruction-arguments instruction)
                                    labels-hash) length)
                     (incf length)))
               (values length labels-hash)))
           (assemble-pass-2 (instructions length labels-hash)
             (let ((result (make-array length))
                   (current 0))
               (dolist (instruction instructions)
                 (let ((opcode (instruction-opcode instruction))
                       (args (instruction-arguments instruction)))
                   (unless (eq :label opcode)
                     (case opcode
                       ((:tjump :fjump :jump)
                        (setf (instruction-arguments instruction)
                              (gethash args labels-hash)))
                       (:fn
                        (setf (first (instruction-arguments instruction))
                              (gethash (first args) labels-hash))))
                     (setf (aref result current) instruction)
                     (incf current))))
               result)))
    (multiple-value-bind (length labels-hash)
        (assemble-pass-1 instructions)
      (assemble-pass-2 instructions length labels-hash))))

(defun show-instructions (sources instructions)
  (include-relevant-source-as-comments sources instructions)
  (dolist (instruction instructions)
    (let ((opcode (instruction-opcode instruction))
          (args (instruction-arguments instruction)))
      (dolist (comment (instruction-comments instruction))
        (format t "~a~%" comment))
      (cond ((eq :label opcode)
             (format t "~a:" args))
            (t (if (consp args)
                   (format t "    ~a ~{~a~^, ~}" opcode args)
                   (if args
                       (format t "    ~a ~a" opcode args)
                       (format t "    ~a" opcode)))))
      (terpri))))

(defun include-relevant-source-as-comments (sources instructions)
  (let (source-lines last-file-name)
    (dolist (instruction instructions)
      (when (and (instruction-start-pos instruction)
                 (or (not last-file-name)
                     (string/= last-file-name
                               (pos-file-name
                                (instruction-start-pos instruction)))))
        (setf last-file-name (pos-file-name (instruction-start-pos instruction)))
        (let ((source (shovel-utils:find-source sources last-file-name)))
          (setf source-lines (split-sequence:split-sequence
                              #\newline
                              (shovel-types:shript-file-contents source)))))
      (alexandria:when-let ((start-pos (instruction-start-pos instruction))
                            (end-pos (instruction-end-pos instruction)))
        (setf (instruction-comments instruction)
              (append (instruction-comments instruction)
                      (extract-relevant-source sources start-pos end-pos
                                               :line-prefix "    ; "
                                               :source-lines source-lines))))))
  instructions)
