(in-package #:pcl-mp3-browser)
;;; *** Running the App ***

;; Define the root directory of the mp3 collection
(defparameter *mp3-dir* "/home/rich/Music/mp3s/v2.3")

;; Define the css stylesheet to use
(defparameter *mp3-css* "/home/rich/quicklisp/local-projects/rich/pcl-mp3-browser/mp3-browser.css")

;; This function will do the required initializing of the mp3 database and will
;; start AllegroServe.

(defun start-mp3-browser ()
  (load-database *mp3-dir* *mp3s*)
  (publish-file :path "/mp3-browser-css" :file *mp3-css* :content-type "text/css")
  (setf *song-source-type* 'playlist)
  (net.aserve::debug-on :notrap)
  (net.aserve:start :port 2001))

;;(start-mp3-browser)


(defun stop-mp3-browser ()
  (net.aserve:shutdown))

(defun reset-mp3-database ()
  (setf *mp3s* (make-instance 'table :schema *mp3-schema*)))
