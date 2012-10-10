
(in-package #:shovel-utils)

(defun underline (start end)
  (with-output-to-string (str)
    (loop repeat (1- start) do (write-char #\space str))
    (loop repeat (1+ (- end start)) do (write-char #\^ str))))

(defun first-non-blank (line)
  (1+ (or (position-if (lambda (ch) (and (char/= ch #\space) (char/= ch #\tab)))
                       line)
          0)))

(defun prepare-sources (sources)
  (let ((counter 0))
    (mapcar (lambda (source)
              (cond ((stringp source)
                     (shovel-types:make-shript-file
                      :contents source
                      :name (format nil "<unspecified-~d>" (incf counter))))
                    (t source)))
            sources)))

(defun find-source (sources file-name)
  (dolist (source sources)
    (when (string= file-name (shovel-types:shript-file-name source))
      (return-from find-source source)))
  (error (make-condition 'shovel-error
                         :message (format nil "File '~a' not found." file-name))))

(defun find-position (file-name content char-position)
  (declare (optimize speed (safety 0))
           (type (simple-array character (*)) file-name content)
           (type fixnum char-position))
  (let ((result (make-pos :file-name file-name)))
    (when (<= (length content) char-position)
      (error
       "The character position specified is not inside the provided content."))
    (loop
       for i from 0 to char-position
       when (char= (aref content i) #\newline) 
       do 
         (incf (pos-line result))
         (setf (pos-column result) 1)
       when (char/= (aref content i) #\newline)
       do
         (incf (pos-column result)))
    (decf (pos-column result))
    result))

(defun extract-relevant-source (source-files start-pos end-pos
                                &key
                                  (line-prefix "")
                                  source-lines)
  (unless source-lines
    (let* ((file-name (shovel-types:pos-file-name start-pos))
           (source (find-source source-files file-name)))
      (setf source-lines
            (split-sequence:split-sequence
             #\newline
             (shovel-types:shript-file-contents source)))))
  (let* ((file-name (pos-file-name start-pos))
         (start-line (pos-line start-pos))
         (end-line (pos-line end-pos))
         (add-elipsis (> end-line start-line))
         (first-line (elt source-lines (1- start-line))))
    (list (with-output-to-string (str)
            (format str "~afile '~a' line ~d: ~a"
                    line-prefix file-name start-line first-line)
            (when add-elipsis
              (format str " [...content snipped...]")))
          (format nil "~afile '~a' line ~d: ~a"
                  line-prefix
                  file-name
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

(defmacro defbits (struct-name accessor &rest bitnames)
  (check-type accessor symbol)
  (dolist (bitname bitnames)
    (check-type bitname symbol))
  `(locally (declare (optimize speed))
     ,@(loop
          for i from 0 to (1- (length bitnames))
          collect `(declaim
                    (inline ,(alexandria:symbolicate
                              struct-name '- (elt bitnames i))))
          collect `(defun ,(alexandria:symbolicate
                            struct-name '- (elt bitnames i)) (struct)
                     (/= 0 (ldb (byte 1 ,i) (the fixnum (,accessor struct)))))
          collect `(defun ,(alexandria:symbolicate
                            'set- struct-name '- (elt bitnames i))
                       (struct new-value)
                     (setf (ldb (byte 1 ,i) (,accessor struct))
                           (if new-value 1 0))
                     new-value)
          collect `(defsetf ,(alexandria:symbolicate
                              struct-name '- (elt bitnames i))
                       ,(alexandria:symbolicate
                         'set- struct-name '- (elt bitnames i))))))
