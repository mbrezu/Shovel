;; Copyright (c) 2012, Miron Brezuleanu
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:shovel-compiler)

(defun compile-sources-to-instructions (sources)
  (setf sources (prepare-sources sources))
  (let* ((all-tokens (mapcar (lambda (source-file)
                               (tokenize-source-file source-file))
                             sources))
         (parse-tree (mapcan (lambda (file-tokens)
                               (parse-tokens file-tokens :source sources))
                             all-tokens))
         (instructions
          (generate-instructions parse-tree :source sources)))
    instructions))

(defun compute-instructions-md5 (instructions)
  (let ((digester (ironclad:make-digest :md5)))
    (labels ((update-with-arguments (args)
               (cond ((null args))
                     ((stringp args)
                      (ironclad:update-digest digester
                                              (babel:string-to-octets args)))
                     ((or (eq :true args) (eq :false args) (eq :null args))
                      (ironclad:update-digest digester
                                              (babel:string-to-octets
                                               (symbol-name args))))
                     ((numberp args)
                      (ironclad:update-digest digester
                                              (babel:string-to-octets
                                               (format nil "~a" args))))
                     ((consp args)
                      (dolist (arg args)
                        (update-with-arguments arg)))
                     (t (error "Shovel internal WTF: ~
don't know how to compute MD5 hash.")))))
      (dotimes (i (length instructions))
        (let ((instruction (aref instructions i)))
          (ironclad:update-digest digester (babel:string-to-octets
                                            (symbol-name
                                             (instruction-opcode instruction))))
          (update-with-arguments (instruction-arguments instruction)))))
    (produce-digest-as-string digester)))

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
                       ((:tjump :fjump :jump :block)
                        (setf (instruction-arguments instruction)
                              (gethash args labels-hash)))
                       (:fn
                        (setf (first (instruction-arguments instruction))
                              (gethash (first args) labels-hash))))
                     (setf (instruction-opcode-num instruction)
                           (get-numeric-opcode opcode))
                     (setf (aref result current) instruction)
                     (incf current))))
               result)))
    (multiple-value-bind (length labels-hash)
        (assemble-pass-1 instructions)
      (assemble-pass-2 instructions length labels-hash))))

(defun get-numeric-opcode (opcode)
  (case opcode
    (:jump 0)
    (:const 1)
    (:prim0 2)
    (:prim 3)
    (:call 4)
    (:callj 5)
    (:fjump 6)
    (:lset 7)
    (:pop 8)
    (:lget 9)
    (:fn 10)
    (:new-frame 11)
    (:drop-frame 12)
    (:args 13)
    (:return 14)
    (:block 15)
    (:pop-block 16)
    (:block-return 17)
    (:context 18)
    (:tjump 19)
    (:file-name 20)
    (:vm-version 20)
    (:vm-sources-md5 20)
    (:vm-bytecode-md5 20)))

(defun show-instructions (sources instructions)
  (setf sources (prepare-sources sources))
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
      (unless (and (eq :label opcode) (not (instruction-comments instruction)))
        (terpri))
      (terpri))))

(defun include-relevant-source-as-comments (sources instructions)
  (let (source-lines last-file-name source)
    (dolist (instruction instructions)
      (when (eq :file-name (instruction-opcode instruction))
        (let ((file-name (instruction-arguments instruction)))
          (when (or (not last-file-name) (string/= last-file-name file-name))
            (setf last-file-name file-name
                  source (shovel:source-file-contents
                          (find-source sources last-file-name))
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
