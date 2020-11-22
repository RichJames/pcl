;;; examples.lisp

(in-package #:bin-data)

;;; This is example code that is not part of the library itself.


;; This is an example call using the macro defined above
(define-binary-class id3-tag ()
	      ((file-identifier (iso-8859-1-string :length 3))
	       (major-version u1)
	       (revision u1)
	       (flags u1)
	       (size id3-tag-size)
	       (frames (id3-frames :tag-size size))))

;; The above macro call will generate this code:
(DEFCLASS ID3-TAG NIL
          ((FILE-IDENTIFIER :INITARG :FILE-IDENTIFIER :ACCESSOR FILE-IDENTIFIER)
           (MAJOR-VERSION :INITARG :MAJOR-VERSION :ACCESSOR MAJOR-VERSION)
           (REVISION :INITARG :REVISION :ACCESSOR REVISION)
           (FLAGS :INITARG :FLAGS :ACCESSOR FLAGS)
           (SIZE :INITARG :SIZE :ACCESSOR SIZE)
           (FRAMES :INITARG :FRAMES :ACCESSOR FRAMES)))


(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop :for char = (read-byte in nil)
       :until (or (equal char (char-code +null+)) (equal char nil))
       :do (write-char (code-char char) s))))

(defun write-null-terminated-ascii (string out)
  (loop :for char :across string
     :do (write-byte (char-code char) out))
  (write-byte (char-code +null+) out))

(defvar testfile "/home/rich/quicklisp/local-projects/rich/bin-data/testfile.txt")
(defvar message "Hello, World. It's Rich! How goes it?")

(defun write-message-to-file (file message)
  (with-open-file (stream file
			  :element-type '(unsigned-byte 8)
			  :direction :output
			  :if-exists :append
			  :if-does-not-exist :create)
    (write-null-terminated-ascii message stream)))

(defun read-and-print-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (do ((string (read-null-terminated-ascii stream) (read-null-terminated-ascii stream)))
	((= (length string) 0) )
      (format t "~s~%" string))))

;; *** example of a binary type declaration ***

(define-binary-type iso-8859-1-string (length)
  (:reader (in)
	   (let ((string (make-string length)))
	     (dotimes (i length)
	       (setf (char string i) (code-char (read-byte in))))
	     string))
  (:writer (out string)
	   (dotimes (i length)
	     (write-byte (char-code (char string i)) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1))
