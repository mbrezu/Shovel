
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
  (let (source-lines last-file-name source)
    (dolist (instruction instructions)
      (when (eq :file-name (instruction-opcode instruction))
        (let ((file-name (instruction-arguments instruction)))
          (when (or (not last-file-name) (string/= last-file-name file-name))
            (setf last-file-name file-name
                  source (shovel-types:shript-file-contents
                          (shovel-utils:find-source sources last-file-name))
                  source-lines (split-sequence:split-sequence
                                #\newline
                                source)))))
      (when last-file-name
        (alexandria:when-let*
            ((character-start-pos (instruction-start-pos instruction))
             (character-end-pos (instruction-end-pos instruction))
             (start-pos (find-position last-file-name source character-start-pos))
             (end-pos (find-position last-file-name source character-end-pos)))
          (setf (instruction-comments instruction)
                (append (instruction-comments instruction)
                        (extract-relevant-source sources start-pos end-pos
                                                 :line-prefix "    ; "
                                                 :source-lines source-lines)))))))
  instructions)
