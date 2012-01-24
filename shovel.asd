;;;; shovel.asd

(asdf:defsystem #:shovel
  :serial t
  :depends-on (#:alexandria #:mbrezu-utils)
  :components ((:file "package")
               (:file "shovel")))

