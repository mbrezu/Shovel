;;;; package.lisp

(defpackage #:shovel
  (:use #:cl))

(defpackage #:shovel-compiler-types
  (:use #:cl)
  (:export
   
   :pos
   :make-pos
   :pos-p
   :clone-pos
   :pos-line
   :pos-column
   :pos-char
   
   :token
   :make-token
   :token-p
   :token-type
   :token-content
   :token-start-pos
   :token-end-pos

   :parse-tree
   :make-parse-tree
   :parse-tree-p
   :parse-tree-label
   :parse-tree-start-pos
   :parse-tree-end-pos
   :parse-tree-children
   
   :instruction
   :make-instruction
   :instruction-p
   :instruction-opcode
   :instruction-arguments
   :instruction-start-pos
   :instruction-end-pos
   :instruction-comments))

(defpackage #:shovel-compiler-tokenizer
  (:use #:cl #:shovel-compiler-types)
  (:export :tokenize-string))

(defpackage #:shovel-compiler-parser
  (:use #:cl #:shovel-compiler-types)
  (:export :parse-tokens))

(defpackage #:shovel-compiler-code-generator
  (:use #:cl #:shovel-compiler-types)
  (:export :generate-instructions))

(defpackage #:shovel-compiler
  (:use #:cl #:shovel-compiler-types)
  (:export
   :compile-string-to-instructions
   :assemble-instructions
   :show-instructions))

