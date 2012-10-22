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

(in-package #:shovel)

(defstruct source-file name contents)

(define-condition shovel-error (error)
  ((file :initform nil :accessor error-file :initarg :file)
   (line :initform nil :accessor error-line :initarg :line)
   (column :initform nil :accessor error-column :initarg :column)
   (message :initform nil :accessor error-message :initarg :message)
   (at-eof :initform nil :accessor error-at-eof :initarg :at-eof)))

(defmethod print-object ((object shovel-error) stream)
  (declare (optimize (speed 1)))
  (format stream "Shovel error")
  (alexandria:when-let (file (error-file object))
    (format stream " in file '~a'" file))
  (alexandria:when-let ((line (error-line object))
                        (column (error-column object)))
    (format stream " at line ~d, column ~d" line column))
  (when (error-at-eof object)
    (format stream " at end of file"))
  (format stream ": ~a~%" (error-message object)))

(define-condition shovel-vm-match-error (error)
  ((message :initform nil :accessor error-message :initarg :message)))

(defmethod print-object ((object shovel-vm-match-error) stream)
  (declare (optimize (speed 1)))
  (format stream "Shovel VM match error error")
  (format stream ": ~a~%" (error-message object)))

(define-condition shovel-broken-checksum (error) ())

(define-condition shovel-version-too-large (error) ())

(define-condition shovel-total-ticks-quota-exceeded (error) ())

(define-condition shovel-cell-quota-exceeded (error) ())
