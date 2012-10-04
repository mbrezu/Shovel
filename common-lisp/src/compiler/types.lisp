
(in-package #:shovel-compiler-types)

(defstruct (pos (:copier clone-pos)) (line 1) (column 1) (char 1))

(defstruct token type content start-pos end-pos)

(defstruct parse-tree label start-pos end-pos children)

(defstruct instruction
  opcode (arguments nil)
  (start-pos nil) (end-pos nil) (comments nil))
