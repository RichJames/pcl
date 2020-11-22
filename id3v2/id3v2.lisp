;;;; id3v2.lisp

(in-package #:id3v2)

;; Define integer types

(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
	   (loop :with value = 0
	      :for low-bit :downfrom (* bits-per-byte (1- bytes)) :to 0 :by bits-per-byte :do
	      (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
	      :finally (return value)))
  (:writer (out value)
	   (loop :for low-bit :downfrom (* bits-per-byte (1- bytes)) :to 0 :by bits-per-byte :do
	      (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type id3-tag-size () (unsigned-integer :bytes 4 :bits-per-byte 7))

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))

;; String types

;; Generic string of a known length
(define-binary-type generic-string (length character-type)
  (:reader (in)
	   (let ((string (make-string length)))
	     (dotimes (i length)
	       (setf (char string i) (read-value character-type in)))
	     string))
  (:writer (out string)
	   (dotimes (i length)
	     (write-value character-type out (char string i)))))

;; Generic string with a known terminator character
(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
	   (with-output-to-string (s)
	     (loop :for char = (read-value character-type in)
		:until (char= char terminator) :do (write-char char s))))
  (:writer (out string)
	   (loop :for char :across string
	      :do (write-value character-type out char)
	      :finally (write-value character-type out terminator))))

;; *** ISO-8859-1 characters and strings ***

;; Define the iso-8859-1 character type
(define-binary-type iso-8859-1-char ()
  (:reader (in)
	   (let ((code (read-byte in)))
	     (or (code-char code)
		 (error "Character code ~d not supported." code))))
  (:writer (out char)
	   (let ((code (char-code char)))
	     (if (<= 0 code #xff)
		 (write-byte code out)
		 (error "Illegal character for iso-8859-1 encoding: character: ~c with code: ~d"
			char code)))))

;; Define iso-8859-1 string of known length
(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

;; Define iso-8859-1 string with known terminator character
(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :character-type 'iso-8859-1-char))

;; *** UCS-2 characters and strings ***

;; Define parameterized UCS-2 characater type (for use in later defining big-endian
;; and little-endian UCS-2 characters)

(define-binary-type ucs-2-char (swap)
  (:reader (in)
	   (let ((code (read-value 'u2 in)))
	     (when swap (setf code (swap-bytes code)))
	     (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
	   (let ((code (char-code char)))
	     (unless (<= 0 code #xffff)
	       (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d"
		      char code))
	     (when swap (setf code (swap-bytes code)))
	     (write-value 'u2 out code))))

(defun swap-bytes (code)
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
  code)

;; Define the two UCS-2 character types:
(define-binary-type ucs-2-char-big-endian () (ucs-2-char :swap nil))
(define-binary-type ucs-2-char-little-endian () (ucs-2-char :swap t))

;; Define function to determine type of UCS-2 character to use, based on the
;; value of the byte-order mark:
(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (#xfeff 'ucs-2-char-big-endian)
    (#xfffe 'ucs-2-char-little-endian)))

;; Define UCS-2 string of a known length (in bytes, NOT characters!)
;; *** Why does this give me a style warning that "the variable LENGTH
;;     is defined but never used."?
(define-binary-type ucs-2-string (length)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in))
		 (characters (1- (/ length 2))))
	     (read-value 'generic-string in
			 :length characters
			 :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
	   (declare (ignore length))
	   (write-value 'u2 out #xfeff)
	   (write-value 'generic-string out string
			:length (length string)
			:character-type (ucs-2-char-type #xfeff))))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in)))
	     (read-value 'generic-terminated-string in
			 :terminator terminator
			 :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
	   (write-value 'u2 out #xfeff)
	   (write-value 'generic-terminated-string out string
			:terminator terminator
			:character-type (ucs-2-char-type #xfeff))))


;; ***************   Some test routines   *************************

;; Read the id3 tag info in file, assuming the id3 info is at the beginning of the file
(defun read-id3 (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'id3-tag in)))

;; Show id3 tag header information for the given file
(defun show-tag-header (file)
  (with-slots (identifier major-version revision flags size) (read-id3 file)
    (format t "~a ~d.~d ~8,'0b ~d bytes -- ~a~%"
	    identifier major-version revision flags size (enough-namestring file))))

;; Test if the given file is an mp3 type file
(defun mp3-p (file)
  (and
   (not (directory-pathname-p file))
   (string-equal "mp3" (pathname-type file))))

;; Display id3 tag headers of mp3 files in the given directory
(defun show-tag-headers (dir)
  (walk-directory dir #'show-tag-header :test #'mp3-p))

;; Count how many ID3 tags of each version (of 2, 3 & 4) are in the files within a given directory
(defun count-versions (dir)
  (let ((versions (mapcar #'(lambda (x) (cons x 0)) '(2 3 4))))
    (flet ((count-version (file)
	     (incf (cdr (assoc (major-version (read-id3 file)) versions)))))
      (walk-directory dir #'count-version :test #'mp3-p))
    versions))

;; Test if a file actually starts with an ID3 tag
(defun id3-p (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (string= "ID3" (read-value 'iso-8859-1-string in :length 3))))

;; Function to write an id3-tag object to a file
(defun write-id3 (id3-object file)
  (with-open-file (stream file
			  :element-type '(unsigned-byte 8)
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :overwrite)
    (write-value 'id3-tag stream id3-object)))

;; Function to create a test id3 object, then write it to a file
(defun test-writing-id3 ()
  (let ((id3-object (make-instance 'id3-tag
				   :identifier "ID3"
				   :major-version 2
				   :revision 0
				   :flags 0
				   :size 8))
	(file "/home/rich/quicklisp/local-projects/rich/id3v2/test-id3v2.mp3"))
    (write-id3 id3-object file)))

;; ***************************************************************************************

;; ID3 Frames

;; v2 frames

(define-tagged-binary-class id3-frame ()
  ((id (iso-8859-1-string :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

(define-binary-class generic-frame (id3-frame)
  ((data (raw-bytes :size size))))

(define-binary-type raw-bytes (size)
  (:reader (in)
	   (let ((buf (make-array size :element-type '(unsigned-byte 8))))
	     (read-sequence buf in)
	     buf))
  (:writer (out buf)
	   (declare (ignore size))
	   (write-sequence buf out)))


;; We need to implement read-frame, which while leverage the condition system to
;; orchestrate "peeking" one byte ahead to determine if we are looking at padding
;; or actual frame data:
(define-condition in-padding () ())

(define-binary-type frame-id (length)
  (:reader (in)
	   (let ((first-byte (read-byte in)))
	     (when (= first-byte 0) (signal 'in-padding))
	     (let ((rest (read-value 'iso-8859-1-string in :length (1- length))))
	       (concatenate
		'string (string (code-char first-byte)) rest))))
  (:writer (out id)
	   (write-value 'iso-8859-1-string out id :length length)))

;; Now we redefine id3-frame to make slot "id" be a frame-id type:
(define-tagged-binary-class id3-frame ()
  ((id (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

;; *** Supporting Multiple versions of ID3

;; This is the 3rd version of id3-tag.  We need to redefine our id3-tag definition to allow for
;; dispatch based on major version:
(define-tagged-binary-class id3-tag ()
  ((identifier          (iso-8859-1-string :length 3))
   (major-version       u1)
   (revision            u1)
   (flags               u1)
   (size                id3-tag-size))
  (:dispatch
   (ecase major-version
     (2 'id3v2.2-tag)
     (3 'id3v2.3-tag))))

;; Now we define a class for id3 v2.2 tags:
(define-binary-class id3v2.2-tag (id3-tag)
  ((frames (id3-frames :tag-size size :frame-type 'id3v2.2-frame))))

;; Next, we need to define an id3 v2.3 tag:
(define-binary-type optional (type if)
  (:reader (in)
	   (when if (read-value type in)))
  (:writer (out value)
	   (when if (write-value type out value))))

;; 1st definition of id3v2.3-tag:
(define-binary-class id3v2.3-tag (id3-tag)
  ((extended-header-size       (optional :type 'u4 :if (extended-p flags)))
   (extra-flags                (optional :type 'u2 :if (extended-p flags)))
   (padding-size               (optional :type 'u4 :if (extended-p flags)))
   (crc                        (optional :type 'u4 :if (crc-p flags extra-flags)))
   (frames                     (id3-frames :tag-size size :frame-type 'id3v2.3-frame))))

(defun extended-p (flags) (logbitp 6 flags))
(defun crc-p (flags extra-flags)
  (and (extended-p flags) (logbitp 15 extra-flags)))

;; New versions of id3-frames and read-frame to support the xtra frame-type parameter:
(define-binary-type id3-frames (tag-size frame-type)
  (:reader (in)
	   (loop :with to-read = tag-size
	      :while (plusp to-read)
	      :for frame = (read-frame frame-type in)
	      :while frame
	      :do (decf to-read (+ (frame-header-size frame) (size frame)))
	      :collect frame
	      :finally (loop :repeat (1- to-read) :do (read-byte in))))
  (:writer (out frames)
	   (loop :with to-write = tag-size
	      :for frame :in frames
	      :do (write-value frame-type out frame)
	      (decf to-write (+ (frame-header-size frame) (size frame)))
	      :finally (loop :repeat to-write :do (write-byte 0 out)))))

(defun read-frame (frame-type in)
  (handler-case (read-value frame-type in)
    (in-padding () nil)))

(defgeneric frame-header-size (frame))

;; Now we need to define versioned frame base classes
(define-tagged-binary-class id3v2.2-frame ()
  ((id (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

;; Here's the v2.3 frame base class:
(define-tagged-binary-class id3v2.3-frame ()
  ((id                   (frame-id :length 4))
   (size                 u4)
   (flags                u2)
   (decompressed-size    (optional :type 'u4 :if (frame-compressed-p flags)))
   (encryption-scheme    (optional :type 'u1 :if (frame-encrypted-p flags)))
   (grouping-identity    (optional :type 'u1 :if (frame-grouped-p flags))))
  (:dispatch (find-frame-class id)))

(defun frame-compressed-p (flags) (logbitp 7 flags))
(defun frame-encrypted-p (flags) (logbitp 6 flags))
(defun frame-grouped-p (flags) (logbitp 5 flags))

(defmethod frame-header-size ((frame id3v2.2-frame)) 6)
(defmethod frame-header-size ((frame id3v2.3-frame)) 10)

;; Redefinition of generic-frame, taking into account our two new version-specific
;; id3 base classes:
(define-binary-class generic-frame ()
  ((data (raw-bytes :size (data-bytes (current-binary-object))))))

(defgeneric data-bytes (frame))

(defmethod data-bytes ((frame id3v2.2-frame))
  (size frame))

(defmethod data-bytes ((frame id3v2.3-frame))
  (let ((flags (flags frame)))
    (- (size frame)
       (if (frame-compressed-p flags) 4 0)
       (if (frame-encrypted-p flags) 1 0)
       (if (frame-grouped-p flags) 1 0))))

;; These clases are mixins of the specific base class and the generic frame class:
(define-binary-class generic-frame-v2.2 (id3v2.2-frame generic-frame) ())
(define-binary-class generic-frame-v2.3 (id3v2.3-frame generic-frame) ())



;; ***** More test code ****************************************************

(defun frame-types (file)
  (delete-duplicates (mapcar #'id (frames (read-id3 file))) :test #'string=))

(defun frame-types-in-dir (dir)
  (let ((ids ()))
    (flet ((collect (file)
	     (setf ids (nunion ids (frame-types file) :test #'string=))))
      (walk-directory dir #'collect :test #'mp3-p))
    ids))

;; ************************************************************************

;; Text Information Frames

(defun non-terminated-type (encoding)
  (ecase encoding
    (0 'iso-8859-1-string)
    (1 'ucs-2-string)))

(defun terminated-type (encoding)
  (ecase encoding
    (0 'iso-8859-1-terminated-string)
    (1 'ucs-2-terminated-string)))

(defun string-args (encoding length terminator)
  (cond
    (length (values (non-terminated-type encoding) :length length))
    (terminator (values (terminated-type encoding) :terminator terminator))))

(define-binary-type id3-encoded-string (encoding length terminator)
  (:reader (in)
	   (multiple-value-bind (type keyword arg)
	       (string-args encoding length terminator)
	     (read-value type in keyword arg)))
  (:writer (out string)
	   (multiple-value-bind (type keyword arg)
	       (string-args encoding length terminator)
	     (write-value type out string keyword arg))))

(define-binary-class text-info-frame ()
  ((encoding u1)
   (information (id3-encoded-string :encoding encoding :length (bytes-left 1)))))

(defun bytes-left (bytes-read)
  (- (size (current-binary-object)) bytes-read))

(define-binary-class text-info-frame-v2.2 (id3v2.2-frame text-info-frame) ())
(define-binary-class text-info-frame-v2.3 (id3v2.3-frame text-info-frame) ())

;; **** Comment Frames ****
(define-binary-class comment-frame ()
  ((encoding u1)
   (language (iso-8859-1-string :length 3))
   (description (id3-encoded-string :encoding encoding :terminator +null+))
   (text (id3-encoded-string
	  :encoding encoding
	  :length (bytes-left
		   (+ 1 ; encoding
		      3 ; language
		      (encoded-string-length description encoding t)))))))

(defun encoded-string-length (string encoding terminated)
  (let ((characters (+ (length string) (if terminated 1 0))))
    (* characters (ecase encoding (0 1) (1 2)))))

(define-binary-class comment-frame-v2.2 (id3v2.2-frame comment-frame) ())
(define-binary-class comment-frame-v2.3 (id3v2.3-frame comment-frame) ())

(defun find-frame-class (name)
  (cond
    ((and (char= (char name 0) #\T)
	  (not (member name '("TXX" "TXXX") :test #'string=)))
     (ecase (length name)
       (3 'text-info-frame-v2.2)
       (4 'text-info-frame-v2.3)))
    ((string= name "COM") 'comment-frame-v2.2)
    ((string= name "COMM") 'comment-frame-v2.3)
    (t
     (ecase (length name)
       (3 'generic-frame-v2.2)
       (4 'generic-frame-v2.3)))))

;; *** Utility functions to query id3 tags

(defun upto-null (string)
  (subseq string 0 (position +null+ string)))

(defun find-frame (id3 ids)
  (find-if #'(lambda (x) (find (id x) ids :test #'string=)) (frames id3)))

(defun get-text-info (id3 &rest ids)
  (let ((frame (find-frame id3 ids)))
    (when frame (upto-null (information frame)))))

(defun song (id3) (get-text-info id3 "TT2" "TIT2"))
(defun album (id3) (get-text-info id3 "TAL" "TALB"))
(defun artist (id3) (get-text-info id3 "TP1" "TPE1"))
(defun track (id3) (get-text-info id3 "TRK" "TRCK"))
(defun year (id3) (get-text-info id3 "TYE" "TYER" "TDRC"))
(defun genre (id3) (get-text-info id3 "TCO" "TCON"))

;; *** support for v1 genre tags ***

(defun translated-genre (id3)
  (let ((genre (genre id3)))
    (if (and genre (char= #\( (char genre 0)))
	(translate-v1-genre genre)
	genre)))

(defun translate-v1-genre (genre)
  (aref *id3-v1-genres* (parse-integer genre :start 1 :junk-allowed t)))

(defparameter *id3-v1-genres*
  #(
    ;; These are the official ID3v1 genres:
    "Blues" "Classic Rock" "Country" "Dance" "Disco" "Funk" "Grunge"
    "Hip-Hop" "Jazz" "Metal" "New Age" "Oldies" "Other" "Pop" "R&B" "Rap"
    "Reggae" "Rock" "Techno" "Industrial" "Alternative" "Ska"
    "Death Metal" "Pranks" "Soundtrack" "Euro-Techno" "Ambient"
    "Trip-Hop" "Vocal" "Jazz+Funk" "Fusion" "Trance" "Classical"
    "Instumental" "Acid" "House" "Game" "Sound Clip" "Gospel" "Noise"
    "AlternRock" "Bass" "Soul" "Punk" "Space" "Meditative"
    "Instrumental Pop" "Instrumental Rock" "Ethnic" "Gothic" "Darkwave"
    "Techno-Industrial" "Electronic" "Pop-Folk" "Eurodance" "Dream"
    "Southern Rock" "Comedy" "Cult" "Gangsta" "Top 40" "Christian Rap"
    "Pop/Funk" "Jungle" "Native American" "Cabaret" "New Wave"
    "Psychedelic" "Rave" "Showtunes" "Trailer" "Lo-Fi" "Tribal"
    "Acid Punk" "Acid Jazz" "Polka" "Retro" "Musical" "Rock & Roll"
    "Hard Rock"

    ;; These were made up by the authors of Winamp but backported into
    ;; the ID3 spec:
    "Folk" "Folk-Rock" "National Folk" "Swing" "Fast Fusion"
    "Bebob" "Latin" "Revival" "Celtic" "Bluegrass" "Avantgarde"
    "Gothic Rock" "Progressive Rock" "Psychedelic Rock" "Symphonic Rock"
    "Slow Rock" "Big Band" "Chorus" "Easy Listening" "Acoustic" "Humor"
    "Speech" "Chanson" "Opera" "Chamber Music" "Sonata" "Symphony"
    "Booty Bass" "Primus" "Porn Groove" "Satire" "Slow Jam" "Club"
    "Tango" "Samba" "Folklore" "Ballad" "Power Ballad" "Rhythmic Soul"
    "Freestyle" "Duet" "Punk Rock" "Drum Solo" "A capella" "Euro-House"
    "Dance Hall"

    ;; These were also invented by the Winamp folks, but ignored by the
    ;; ID3 authors:
    "Goa" "Drum & Bass" "Club-House" "Hardcore" "Terror" "Indie"
    "BritPop" "Negerpunk" "Polsk Punk" "Beat" "Christian Gansta Rap"
    "Heavy Metal" "Black Metal" "Crossover" "Contemporary Christian"
    "Christian Rock" "Merengue" "Salsa" "Thrash Metal" "Anime" "Jpop"
    "Synthpop"))

