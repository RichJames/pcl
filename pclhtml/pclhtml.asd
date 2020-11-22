;;;; pclhtml.asd

(asdf:defsystem #:pclhtml
  :description "Describe pclhtml here"
  :author "Rich James"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:macro-utils)
  :components ((:file "package")
               (:file "pclhtml")))
