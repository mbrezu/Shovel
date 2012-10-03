;;;; shovel.asd

(asdf:defsystem #:shovel
  :depends-on (#:alexandria #:mbrezu-utils-base)
  :components ((:file "package")
               (:file "shovel-codegen" :depends-on ("package"))
               (:file "shovel-tokenize" :depends-on ("package"))
               (:file "shovel-parse" :depends-on ("package"
                                                  "shovel-tokenize"))
               (:file "shovel" :depends-on ("package"
                                            "shovel-codegen"
                                            "shovel-parse"))))

