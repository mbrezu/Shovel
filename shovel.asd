;;;; shovel.asd

(asdf:defsystem #:shovel
  :depends-on (#:alexandria #:mbrezu-utils-base)
  :components ((:file "package")
               (:file "shovel" :depends-on ("package"))))

