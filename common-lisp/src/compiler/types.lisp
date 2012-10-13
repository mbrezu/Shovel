
(in-package #:shovel-compiler-types)

(declaim (optimize speed))
(declaim (inline make-token))
(defstruct token type content start-pos end-pos
           (bits 0 :type fixnum))

(shovel-utils:defbits token token-bits
  is-relational-op
  is-adder-op
  is-multiplier-op
  is-logical-and-op
  is-logical-or-op
  is-required-primitive)

(defstruct parse-tree label start-pos end-pos children)


