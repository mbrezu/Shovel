
(in-package #:shovel-types)

(defstruct instruction
  opcode (arguments nil)
  (start-pos nil) (end-pos nil) (comments nil))
