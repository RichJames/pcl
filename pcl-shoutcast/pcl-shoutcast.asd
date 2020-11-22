;;;; pcl-shoutcast.asd

(asdf:defsystem #:pcl-shoutcast
  :description "Describe pcl-shoutcast here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:aserve
	       #:id3v2)
  :components ((:file "package")
               (:file "pcl-shoutcast")))
