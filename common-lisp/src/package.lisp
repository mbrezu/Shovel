;;;; package.lisp

(defpackage #:shovel-types
  (:use #:cl)
  (:export

   :instruction
   :make-instruction
   :instruction-p
   :instruction-opcode
   :instruction-arguments
   :instruction-start-pos
   :instruction-end-pos
   :instruction-comments

   :pos
   :make-pos
   :pos-p
   :clone-pos
   :pos-line
   :pos-column
   :pos-char

   :shovel-error
   :error-file
   :error-line
   :error-column
   :error-message))

(defpackage #:shovel-utils
  (:use #:cl #:shovel-types)
  (:export
   :first-non-blank
   :underline
   :highlight-position
   :extract-relevant-source))

(defpackage #:shovel-compiler-types
  (:use #:cl #:shovel-types)
  (:export

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

   :shovel-compiler-error))

(defpackage #:shovel-compiler-tokenizer
  (:use #:cl #:shovel-compiler-types #:shovel-types)
  (:export :tokenize-string))

(defpackage #:shovel-compiler-parser
  (:use #:cl #:shovel-compiler-types #:shovel-types #:shovel-utils)
  (:export :parse-tokens))

(defpackage #:shovel-compiler-code-generator
  (:use #:cl #:shovel-compiler-types #:shovel-types #:shovel-utils)
  (:export :generate-instructions))

(defpackage #:shovel-compiler
  (:use #:cl #:shovel-compiler-types #:shovel-types #:shovel-utils)
  (:export
   :compile-string-to-instructions
   :assemble-instructions
   :show-instructions))

(defpackage #:shovel-vm-prim0
  (:use #:cl #:shovel-types)
  (:export 
   :add
   :subtract
   :multiply
   :divide
   :shift-left
   :shift-right
   :less-than 
   :greater-than
   :less-than-or-equal
   :greater-than-or-equal 
   :unary-minus
   :bitwise-and
   :bitwise-or
   :are-equal
   :are-not-equal
   :logical-and
   :logical-or
   :logical-not
   :is-true
   :is-bool
   :hash-constructor
   :array-constructor
   :array-or-hash-get
   :array-or-hash-set
   :get-length
   :get-hash-table-keys
   :array-constructor-n
   :for-each
   :for-each-with-index
   :get-slice
   :utc-seconds-since-unix-epoch
   :decode-time
   :encode-time
   :shovel-is-string
   :shovel-is-hash
   :shovel-is-bool
   :shovel-is-array
   :shovel-is-number
   :shovel-is-callable
   :shovel-string
   :shovel-string-representation
   :parse-int
   :parse-float
   :modulo
   :bitwise-xor
   :pow
   :has-key
   :hash-get-dot))

(defpackage #:shovel-vm
  (:use #:cl #:shovel-types #:shovel-utils)
  (:export
   :run-vm 
   :*error-raiser*))

(defpackage #:shovel
  (:use #:cl)
  (:export
   :print-code
   :run-code
   :stdlib))
