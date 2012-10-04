;;;; package.lisp

(defpackage #:shovel
  (:use #:cl))

(defpackage #:shovel-tokenize
  (:use #:cl)
  (:export
   :tokenize-string
   :pos
   :pos-line
   :pos-column
   :pos-char
   :token
   :token-type
   :token-content
   :token-start-pos
   :token-end-pos))

(defpackage #:shovel-parse
  (:use #:cl #:shovel-tokenize)
  (:export
   :parse-string
   :parse-tree
   :make-parse-tree
   :parse-tree-label
   :parse-tree-start-pos
   :parse-tree-end-pos
   :parse-tree-children))

(defpackage #:shovel-codegen
  (:use #:cl #:shovel-parse)
  (:export
   :gen-code
   :comp
   :show-bytecode))

