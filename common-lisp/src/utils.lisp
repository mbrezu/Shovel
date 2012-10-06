
(in-package #:shovel-utils)

(defun underline (start end)
  (with-output-to-string (str)
    (loop repeat (1- start) do (write-char #\space str))
    (loop repeat (1+ (- end start)) do (write-char #\^ str))))

(defun first-non-blank (line)
  (1+ (or (position-if (lambda (ch) (and (char/= ch #\space) (char/= ch #\tab)))
                       line)
          0)))

(defun extract-relevant-source (source-lines start-pos end-pos
                                &key (line-prefix ""))
  (when (stringp source-lines)
    (setf source-lines (split-sequence:split-sequence #\newline source-lines)))
  (let* ((start-line (pos-line start-pos))
         (end-line (pos-line end-pos))
         (add-elipsis (> end-line start-line))
         (first-line (elt source-lines (1- start-line))))
    (list (with-output-to-string (str)
            (format str "~aline ~5d: ~a" line-prefix start-line first-line)
            (when add-elipsis
              (format str " [...content snipped...]")))
          (format nil "~aline ~5d: ~a"
                  line-prefix
                  start-line
                  (underline (max (pos-column start-pos)
                                  (first-non-blank first-line))
                             (min (length first-line)
                                  (if add-elipsis
                                      (length first-line)
                                      (pos-column end-pos))))))))
