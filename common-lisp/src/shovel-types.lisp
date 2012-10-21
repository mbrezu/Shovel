
(in-package #:shovel-types)

(declaim (optimize speed))

(defstruct instruction
  opcode (arguments nil)
  (start-pos nil) (end-pos nil) (comments nil)
  (cache nil)
  (opcode-num 255 :type (unsigned-byte 8)))

(declaim (inline make-pos clone-pos copy-pos-slots))
(defstruct (pos (:copier clone-pos))
  (file-name "<unspecified>")
  (line 1 :type fixnum)
  (column 1 :type fixnum))

