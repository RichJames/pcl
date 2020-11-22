(in-package #:pclhtml)

;; *** test usage ***

(with-open-file (out "~/web/pclweb/html/foo.html" :direction :output)
  (with-html-output (out :pretty t)
    (emit-html '(:html
		 (:head (:title "Foo"))
		 (:body (:h1 "Hello World! This is Foo!"))))))

(with-open-file (out "~/web/pclweb/html/my-html.html"
		     :direction :output
		     :if-exists :overwrite)
  (with-html-output (out :pretty t)
    (emit-html
     '(:html
       (:head
	:title "My HTML")
       (:body
	(:h1 "My HTML from FOO")
	(:p)
	(:b "Here's an ordered list:")
	(:ol
	 (:li "Item 1")
	 (:li "Item 2")
	 (:li "Item 3"))
	(:p "After this is a horizontal rule")
	(:hl)
	(:p)
	(:h3 "That's all for now!"))))))
