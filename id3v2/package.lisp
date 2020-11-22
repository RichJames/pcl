;;;; package.lisp

(defpackage #:id3v2
  (:use #:cl
	#:bin-data
	#:pathnames)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))
