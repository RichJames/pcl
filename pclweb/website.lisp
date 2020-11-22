(in-package #:pclweb)

;; *** Start the webserver ***
(start :port 8085)

;; *** Publish base directory ***
(publish-directory :prefix "/" :destination "/home/rich/web/pclweb/html/")

;; *** Web page functions ***

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (net.aserve:with-http-body (request entity)
      (format
       (request-reply-stream request)
       "<html>~@
        <head><title>Random</title></head>~@
        <body>~@
        <p>Random number: ~d</p>~@
        </body>~@
        </html>~@
       "
       (random 1000)))))

;; You can publish the above function to respond to a uri request like this:
;;  (publish :path "/random-number" :function 'random-number)

(defun random-numbers (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
	(html
	  (:head (:title "Random Numbers"))
	  (:body
	   (:h1 "Random Numbers")
	   (:p (loop :repeat 10 :do (html (:print (random 1000)) (:br))))))))))


(define-html-macro :standard-page ((&key title) &body body)
  `(html
     (:head (:title ,title))
     (:body
      (:h1 ,title)
      ,@body)))

(defun show-query-params (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
	(html
	  (:standard-page
	   (:title "Query Parameters")
	   (if (request-query request)
	       (html
		 (:table :border 1
			 (loop :for (k . v) :in (request-query request)
			    :do (html (:tr (:td k) (:td v))))))
	       (html (:p "No query parameters.")))))))))

(defun simple-form (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (let ((*html-output* (request-reply-stream request)))
	(html
	  (:html
	   (:head (:title "Simple Form"))
	   (:body
	    (:form :method "POST" :action "/show-query-params"
		   (:table
		    (:tr (:td "Foo")
			 (:td (:input :name "foo" :size 20)))
		    (:tr (:td "Password")
			 (:td (:input :name "password" :type "password" :size 20)))
		    (:p (:input :name "submit" :type "submit" :value "Okay")
			(:input ::type "reset" :value "Reset")))))))))))

;; *** A revised random-number function that accepts a query parameter that will limit
;;     the size of the random number.
(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (let* ((*html-output* (request-reply-stream request))
	     (limit-string (or (request-query-value "limit" request) ""))
	     (limit (or (parse-integer limit-string :junk-allowed t) 1000)))
	(html
	  (:html
	   (:head (:title "Random"))
	   (:body
	    (:p "Random number: " (:print (random limit))))))))))

;; *** Cookies ***

(defun show-cookies (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
	(html
	  (:standard-page
	   (:title "Cookies")
	   (if (null (get-cookie-values request))
	       (html (:p "No cookies"))
	       (html
		 (:table
		  (loop :for (key . value) :in (get-cookie-values request)
		     :do (html (:tr (:td key) (:td value)))))))))))))

(defun set-cookie (request entity)
  (with-http-response (request entity :content-type "text/html")
    (set-cookie-header request :name "MyCookie" :value "A cookie value")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
	(html
	  (:standard-page
	   (:title "Set Cookie")
	   (:p "Cookie set.")
	   (:p (:a :href "/show-cookies" "Look at cookie jar."))))))))

;; *** Publish various pages ***

(publish :path "/random-number" :function 'random-number)
(publish :path "/random-numbers" :function 'random-numbers)
(publish :path "/show-query-params" :function 'show-query-params)
(publish :path "/simple-form" :function 'simple-form)
(publish :path "/show-cookies" :function 'show-cookies)
(publish :path "/set-cookie" :function 'set-cookie)
