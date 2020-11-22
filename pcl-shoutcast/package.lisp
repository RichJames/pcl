;;;; package.lisp

(defpackage #:pcl-shoutcast
  (:use #:cl
	#:net.aserve
	#:id3v2)
  (:export :song
	   :file
	   :title
	   :id3-size
	   :find-song-source
	   :current-song
	   :still-current-p
	   :maybe-move-to-next-song
	   :*song-source-type*))
