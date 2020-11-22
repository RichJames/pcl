;;;; package.lisp

(defpackage #:pclweb
  (:use #:cl
	#:net.aserve
	#:pclhtml
	#:macro-utils)
  (:export :define-url-function
	   :string->type))
