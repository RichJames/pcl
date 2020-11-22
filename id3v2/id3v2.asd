;;;; id3v2.asd

(asdf:defsystem #:id3v2
  :description "Describe id3v2 here"
  :author "Rich James"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:bin-data
	       #:pathnames)
  :components ((:file "package")
               (:file "id3v2")))
