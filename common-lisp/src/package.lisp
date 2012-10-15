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
   :pos-file-name
   
   :shript-file
   :make-shript-file
   :shript-file-name
   :shript-file-contents

   :shovel-error
   :error-file
   :error-line
   :error-column
   :error-message
   :shovel-vm-match-error
   :shovel-broken-checksum
   :shovel-version-too-large))

(defpackage #:shovel-utils
  (:use #:cl #:shovel-types)
  (:export
   :first-non-blank
   :underline 
   :extract-relevant-source
   :when-one-of-strings
   :defbits
   :prepare-sources
   :find-source
   :find-position 
   :messagepack-encode-with-md5-checksum
   :check-md5-checksum-and-messagepack-decode))

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
   :token-is-relational-op
   :token-is-adder-op
   :token-is-multiplier-op
   :token-is-logical-and-op
   :token-is-logical-or-op
   :token-is-required-primitive
   :token-bits
   
   :parse-tree
   :make-parse-tree
   :parse-tree-p
   :parse-tree-label
   :parse-tree-start-pos
   :parse-tree-end-pos
   :parse-tree-children))

(defpackage #:shovel-compiler-tokenizer
  (:use #:cl #:shovel-compiler-types #:shovel-types)
  (:export 
           :tokenize-source-file))

(defpackage #:shovel-compiler-parser
  (:use #:cl #:shovel-compiler-types #:shovel-types #:shovel-utils)
  (:export :parse-tokens))

(defpackage #:shovel-compiler-code-generator
  (:use #:cl #:shovel-compiler-types #:shovel-types #:shovel-utils)
  (:export :generate-instructions))

(defpackage #:shovel-compiler
  (:use #:cl #:shovel-compiler-types #:shovel-types #:shovel-utils)
  (:export 
   :assemble-instructions
   :show-instructions
   :compile-sources-to-instructions
   :compute-instructions-md5
   :compute-sources-md5))

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
   :hash-get-dot
   :panic
   :array-pop
   :array-push
   :string-upper
   :string-lower))

(defpackage #:shovel-vm
  (:use #:cl #:shovel-types #:shovel-utils)
  (:export
   :run-vm 
   :step-vm
   :serialize-vm-state 
   :*error-raiser*
   :*version*
   :vm-user-primitive-error
   :wake-up-vm
   :get-vm-stack
   :get-vm-environment
   :get-vm-programming-error
   :get-vm-user-defined-primitive-error))

(defpackage #:shovel
  (:use #:cl)
  (:export
   :print-code
   :run-code
   :stdlib
   :naked-run-code
   :serialize-bytecode
   :deserialize-bytecode
   :get-bytecode
   :run-vm
   :get-vm-stack
   :get-vm-environment
   :wake-up-vm
   :get-vm-programming-error
   :get-vm-user-defined-primitive-error))
