;;;; bin-data.asd

(asdf:defsystem #:bin-data
  :description "Describe bin-data here"
  :author "Rich James"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "utilities")
               (:file "bin-data")))
