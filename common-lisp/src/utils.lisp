
(in-package #:shovel-utils)

(defun underline (start end)
  (with-output-to-string (str)
    (loop repeat (1- start) do (write-char #\space str))
    (loop repeat (1+ (- end start)) do (write-char #\^ str))))

(defun first-non-blank (line)
  (1+ (or (position-if (lambda (ch) (and (char/= ch #\space) (char/= ch #\tab)))
                       line)
          0)))

(defun highlight-position (source pos)
  (let* ((source-lines (split-sequence:split-sequence #\newline source))
         (relevant-line (elt source-lines (1- (pos-line pos)))))
    (format nil "~a~%~a" 
            relevant-line
            (underline (pos-column pos) (pos-column pos)))))

