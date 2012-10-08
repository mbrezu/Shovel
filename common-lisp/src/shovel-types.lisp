
(in-package #:shovel-types)

(declaim (optimize speed))

(defstruct instruction
  opcode (arguments nil)
  (start-pos nil) (end-pos nil) (comments nil))

(declaim (inline make-pos clone-pos copy-pos-slots))
(defstruct (pos (:copier clone-pos))
  (line 1 :type fixnum)
  (column 1 :type fixnum))

(define-condition shovel-error (error)
  ((file :initform nil :accessor error-file :initarg :file)
   (line :initform nil :accessor error-line :initarg :line)
   (column :initform nil :accessor error-column :initarg :column)
   (message :initform nil :accessor error-message :initarg :message)))

(defmethod print-object ((object shovel-error) stream)
  (declare (optimize (speed 1)))
  (format stream "Shovel error")
  (alexandria:when-let (file (error-file object))
    (format stream " in file '~a'" file))
  (alexandria:when-let ((line (error-line object))
                        (column (error-column object)))
    (format stream " at line ~d, column ~d" line column))
  (format stream ": ~a~%" (error-message object)))
