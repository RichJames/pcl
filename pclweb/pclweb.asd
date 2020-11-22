;;;; pclweb.asd

(asdf:defsystem #:pclweb
  :description "Describe pclweb here"
  :author "Rich James"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:aserve
	       #:pclhtml
	       #:macro-utils)
  :components ((:file "package")
               (:file "pclweb")))
