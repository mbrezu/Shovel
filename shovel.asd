;;;; shovel.asd

(asdf:defsystem #:shovel
  :depends-on (#:alexandria
               #:split-sequence)
  :components
  ((:module
    "common-lisp"
    :components
    ((:module
      "src"
      :components
      ((:file "package")
       (:module "compiler"
                :depends-on ("package")
                :components ((:file "types")
                             (:file "tokenizer" :depends-on ("types"))
                             (:file "parser" :depends-on ("types"))
                             (:file "code-generator" :depends-on ("types"))
                             (:file "compiler")))
       (:file "shovel" :depends-on ("package" "compiler"))))))))
