;;;; pcl-database.asd

(asdf:defsystem #:pcl-database
  :description "Describe pcl-database here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:pathnames
	       :macro-utils
	       :id3v2)
  :components ((:file "package")
               (:file "pcl-database")))
