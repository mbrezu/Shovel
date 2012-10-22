;;;; package.lisp

(defpackage #:shovel-types
  (:use #:cl))

(defmacro privately-export (package-name &body symbols)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,(alexandria:format-symbol *package*
                                       "IMPORT-FROM-~a"
                                       (symbol-name package-name)) ()
       (list :import-from
             ,package-name
             ,@(mapcar (lambda (to-intern)
                         `',(intern (symbol-name to-intern) package-name))
                       symbols)))))

(privately-export :shovel-types
  :instruction
  :make-instruction
  :instruction-p
  :instruction-opcode
  :instruction-arguments
  :instruction-start-pos
  :instruction-end-pos
  :instruction-comments
  :instruction-cache
  :instruction-opcode-num

  :pos
  :make-pos
  :pos-p
  :clone-pos
  :pos-line
  :pos-column
  :pos-file-name)

(defpackage #:shovel-utils
  (:use #:cl)
  #. (import-from-shovel-types))

(privately-export :shovel-utils
  :first-non-blank
  :underline
  :extract-relevant-source
  :when-one-of-strings
  :defbits
  :prepare-sources
  :find-source
  :find-position
  :messagepack-encode-with-md5-checksum
  :check-md5-checksum-and-messagepack-decode
  :produce-digest-as-string)

(defpackage #:shovel-compiler-types
  (:use #:cl)
  #. (import-from-shovel-types)
  #. (import-from-shovel-utils))

(privately-export :shovel-compiler-types
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
  :parse-tree-children)

(defpackage #:shovel-compiler-tokenizer
  (:use #:cl)
  #. (import-from-shovel-types)
  #. (import-from-shovel-utils)
  #. (import-from-shovel-compiler-types))

(privately-export :shovel-compiler-tokenizer
  :tokenize-source-file)

(defpackage #:shovel-compiler-parser
  (:use #:cl #:shovel-compiler-types)
  #. (import-from-shovel-types)
  #. (import-from-shovel-utils)
  #. (import-from-shovel-compiler-types))

(privately-export :shovel-compiler-parser
  :parse-tokens)

(defpackage #:shovel-compiler-code-generator
  (:use #:cl #:shovel-compiler-types )
  #. (import-from-shovel-types)
  #. (import-from-shovel-utils)
  #. (import-from-shovel-compiler-types)
  (:export :generate-instructions))

(defpackage #:shovel-compiler
  (:use #:cl #:shovel-compiler-types)
  #. (import-from-shovel-types)
  #. (import-from-shovel-utils)
  #. (import-from-shovel-compiler-tokenizer)
  #. (import-from-shovel-compiler-parser))

(privately-export :shovel-compiler
  :assemble-instructions
  :show-instructions
  :compile-sources-to-instructions
  :compute-instructions-md5
  :compute-sources-md5)

(defpackage #:shovel-vm-prim0
  (:use #:cl)
  #. (import-from-shovel-types))

(privately-export :shovel-vm-prim0
  :add :subtract :multiply :divide
  :shift-left :shift-right :less-than :greater-than
  :less-than-or-equal :greater-than-or-equal :unary-minus
  :bitwise-and :bitwise-or :are-equal :are-not-equal :logical-and
  :logical-or :logical-not :is-true :is-bool :hash-constructor
  :array-constructor :array-or-hash-get :array-or-hash-set
  :get-length :get-hash-table-keys :array-constructor-n :for-each
  :for-each-with-index :get-slice :utc-seconds-since-unix-epoch :decode-time
  :encode-time :shovel-is-string :shovel-is-hash :shovel-is-bool
  :shovel-is-array :shovel-is-number :shovel-is-callable :shovel-string
  :shovel-string-representation :parse-int :parse-float :modulo
  :bitwise-xor :pow :has-key :hash-get-dot :panic :array-pop
  :array-push :string-upper :string-lower)

(defpackage #:shovel-vm
  (:use #:cl )
  #. (import-from-shovel-types)
  #. (import-from-shovel-utils))

(privately-export :shovel-vm
  :run-vm :step-vm :serialize-vm-state
  :*error-raiser* :*version* :vm-user-primitive-error
  :wake-up-vm :get-vm-stack :get-vm-environment
  :get-vm-programming-error :get-vm-user-defined-primitive-error
  :*ticks-incrementer* :*cells-incrementer* :*cells-increment-herald*
  :vm-used-ticks :vm-really-used-cells :get-vm-bytecode-md5
  :get-vm-version :get-vm-sources-md5 :vm-execution-complete)

(defpackage #:shovel
  (:use #:cl)
  #. (import-from-shovel-types)
  #. (import-from-shovel-utils)
  #. (import-from-shovel-compiler)
  #. (import-from-shovel-vm)
  (:export
   
   :print-code :run-code :naked-run-code
   :stdlib
   
   :get-bytecode :serialize-bytecode :deserialize-bytecode
   
   :run-vm :get-vm-stack :get-vm-environment :wake-up-vm

   :get-vm-programming-error
   :get-vm-user-defined-primitive-error
   
   :increment-ticks :increment-cells
   
   :vm-used-cells :vm-used-ticks

   :source-file
   :make-source-file
   :source-file-name
   :source-file-contents

   :shovel-error
   :error-file
   :error-line
   :error-column
   :error-message
   
   :shovel-vm-match-error
   :shovel-broken-checksum
   :shovel-version-too-large
   :shovel-total-ticks-quota-exceeded
   :shovel-cell-quota-exceeded
   
   :vm-version
   :vm-bytecode-md5
   :vm-sources-md5
   
   :serialize-vm-state :vm-execution-complete
   
   :*version*))

(defvar shovel:*version* 1)

