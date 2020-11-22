;;;; package.lisp

(defpackage #:pcl-mp3-browser
  (:use #:cl
	#:net.aserve
	#:pclhtml
	#:pcl-shoutcast
	#:pclweb
	#:pcl-database
	#:id3v2)
  (:import-from :acl-socket
		:ipaddr-to-dotted
		:remote-host)
  (:import-from #+allegro :multiprocessing
		#-allegro :acl-compat.mp
		:make-process-lock
		:with-process-lock)
  (:export :start-mp3-browser))

