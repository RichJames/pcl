;;;; pcl-mp3-browser.asd

(asdf:defsystem #:pcl-mp3-browser
  :description "Describe pcl-mp3-browser here"
  :author "Rich James <richjamespub1@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:aserve
	       #:id3v2
	       #:pcl-database
	       #:pcl-shoutcast
	       #:pclweb
	       #:pclhtml)
  :components ((:file "package")
	       (:file "pcl-playlist")
               (:file "pcl-mp3-browser")
	       (:file "run-app")))
