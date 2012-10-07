
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

(defun group-by (list key-pred)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (item list)
      (setf (gethash (funcall key-pred item) hash)
            (cons item (gethash (funcall key-pred item) hash))))
    (let (result)
      (maphash (lambda (key value)
                 (push (cons key value) result))
               hash)
      result)))

(defun chop-first-from-string-of-alternatives (alternatives)
  (mapcar (lambda (alternative)
            (setf (rest alternative)
                  (mapcar (lambda (string-and-bodies)
                            (setf (first string-and-bodies)
                                  (subseq (first string-and-bodies) 1))
                            string-and-bodies)
                          (rest alternative)))
            alternative)
          alternatives))

(defun gen-code (string string-length alternatives &optional (index 0))
  (when alternatives
    (labels ((alternative-string-is-empty (alternative)
               (= 0 (length (first alternative)))))
      (alexandria:if-let
          (position (position-if #'alternative-string-is-empty alternatives))
        (let ((body (rest (elt alternatives position))))
          `(cond ((= ,string-length ,index)
                  ,@body)
                 (t
                  ,(gen-code string string-length
                             (remove-if #'alternative-string-is-empty alternatives)
                             index))))
        `(when (< ,index ,string-length)
           ,(if (= 1 (length alternatives))
                (let* ((alternative (first alternatives))
                       (body (rest alternative))
                       (alt-string (first alternative)))
                  `(when (string= (subseq ,string ,index) ,alt-string)
                     ,@body))
                (let ((grouped-and-chopped-alternatives
                       (chop-first-from-string-of-alternatives
                        (group-by alternatives
                                  (lambda (item) (aref (first item) 0))))))
                  `(case (aref ,string ,index)
                     ,@(mapcar (lambda (option)
                                 (let ((char (first option))
                                       (alternatives (rest option)))
                                   `(,char
                                     ,(gen-code string string-length
                                                alternatives (1+ index)))))
                               grouped-and-chopped-alternatives)))))))))

(defun prepare-alternatives (alternatives)
  (mapcan (lambda (alternative)
            (cond ((stringp (first alternative)) (list alternative))
                  ((listp (first alternative))
                   (mapcar (lambda (string)
                             (cons string (rest alternative)))
                           (first alternative)))
                  (t (error "Broken alternative."))))
          alternatives))

(defmacro when-one-of-strings (str &body alternatives)
  (setf alternatives (prepare-alternatives alternatives))
  (let ((g-str (gensym))
        (g-str-len (gensym)))
    `(let* ((,g-str ,str)
            (,g-str-len (length ,g-str)))
       (declare (type (simple-array character (*)) ,g-str)
                (type fixnum ,g-str-len))
       ,(gen-code g-str g-str-len alternatives))))
