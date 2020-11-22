;;;; bin-data.lisp

(in-package #:bin-data)

(defconstant +null+ (code-char 0))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as a given type to the stream."))

  
(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))



;; ****************************************************************************
;; From this point forward, we begin to add inheritance and handling of tagged
;; structures.
;; ****************************************************************************

;; Generic methods to read and write already instantiated objects:
;; (Note: write-object is not strictly needed, but we have it to
;;  have some degree of code symmetry.

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

;; Redefine read-value and write-value as they no longer need to be specialized,
;; given read- and write-object will do the specialized code.

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))

;; Now we need code to enable a subclass to refer to the slots it inherits from its
;; superclasses. There are cases where this is needed and, to make it possible, we
;; will store slot information about classes in the attribute list of the class name.

(defun direct-slots (name)
  "Return the slots directly defined by the named class."
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  "Get inherited slots from super classes name was derived from."
  (loop :for super :in (get name 'superclasses)
     nconc (direct-slots super)
     nconc (inherited-slots super)))

(defun all-slots (name)
  "Get all slots, both direct and inherited, for the given class."
  (nconc (direct-slots name) (inherited-slots name)))

;; Helper function to get all slots during macro expansion
(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))


;; *** Tagged Structures ***

;; Define a new, helper macro that generates the common parts of our revised define-binary-class
;; macro and our new define-tagged-binary-class.
(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',name 'slots) ',(mapcar #'first slots))
	 (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
	 ,(mapcar #'slot->defclass-slot slots))

       ,read-method

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
		  (declare (ignorable ,streamvar))
		  (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
		    ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

;; 6th version of this class, now built by using our define-generic-binary-class helper macro.
(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
				  (defmethod read-object progn ((,objectvar ,name) ,streamvar)
					     (declare (ignorable ,streamvar))
					     (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
					       ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

;; And our new define-tagged-binary-class, also using define-generic-binary-class
(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
				  (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
				    (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
				      (let ((,objectvar
					     (make-instance
					      ,@(or (cdr (assoc :dispatch options))
						    (error "Must supply :dispatch form."))
					      ,@(mapcan #'slot->keyword-arg slots))))
					(read-object ,objectvar ,streamvar)
					,objectvar))))))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

;; *** Primitive binary types

;; 2nd version of define-binary-type.  This one enables defining types using either a
;; short form or long form definition.  That is useful for defining one type in terms
;; of another already defined type.  The difference in the two forms is based on the
;; length of spec.

(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
	 `(progn
	    (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
	      (read-value ',derived-from ,stream ,@derived-args))
	    (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
	      (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
	  ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
	     `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
		,@body))
	  ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
	     `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
		,@body)))))))

;; *** Adding a current object stack ***

;; This code provides a way to get at the binary objects being read or written while reading or writing.
;; When reading or writing nested composite objects, it's usefule to get at any of the objects currently
;; being read or written (I'll take his word for it).

;; This variable will hold a stack of objects currently being read or written

(defvar *in-progress-objects* nil)

;; Now we define :around methods on read-object and write-object that push the object being read or
;; written onto that stack before actually reading or writing them.

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

;; Convenience functions for getting at specific objects in the in-progress stack:

(defun current-binary-object () (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))
