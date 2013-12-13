(defpackage :com.cvberry.file-index
  (:nicknames :file-index)
  (:use :common-lisp :alexandria :com.cvberry.util)
  (:import-from :split-sequence :split-sequence)
  (:import-from :com.cvberry.wordstat :bootstrap-image *total-stat-store*)
  (:import-from :com.cvberry.stringops :split-and-strip :get-leading-text)
  (:export :wordentry-numpositions
	   :read-file-index-from-disk
	   :read-file-index-from-file
	   :store-file-index-to-disk
	   :create-file-index
	   :create-file-index-from-plain-file
	   :pathescape
	   :wordentry))

(in-package :com.cvberry.file-index)

;;;MAIN STRUCT
(defstruct file-index 
  (url ())
  (title ())
  (keywords ())
  (description "")
  (leadingtext "")
  (timeindexed ())
  (keywords-freq-hash (make-hash-table :test #'equalp))
  (nwords-in-kfh 0)
  (outgoinglinks ())
  (totnumwords 0)
  (position-hash (make-hash-table :test #'equalp)))
;;position-hash values are wordentry objects

;;;PUBLIC METHODS
(defun read-file-index-from-disk (identifier directory &key (id-is-url t))
  (if id-is-url
      (read-file-index-from-file (concatenate 'string directory (pathescape identifier)))
      (read-file-index-from-file (concatenate 'string directory identifier))))

(defun store-file-index-to-disk (fileindex directory)
  "directory of form indexes/"
  (write-file-index-to-file fileindex (concatenate 'string directory (pathescape (slot-value fileindex 'url)))))

(defun create-file-index (string-input url &optional (title "") (description nil) (keywords nil) (outgoinglinks nil) (tiptext description))
  (let ((mhash (make-hash-table :test #'equalp))
	(wnw (multiple-value-list (split-and-strip string-input)))
	(timeindexed (get-universal-time))
	)
    (add-to-freq-table (nth 0 wnw) mhash 0)
    (multiple-value-bind (keywords-hash nwords-in-kfh) 
	(create-keywords-freq-hash title description keywords)
      (make-file-index :url url 
		       :title title 
		       :keywords keywords 
		       :description description 
		       :leadingtext tiptext
		       :timeindexed timeindexed
		       :keywords-freq-hash keywords-hash
		       :nwords-in-kfh nwords-in-kfh
		       :outgoinglinks outgoinglinks 
		       :totnumwords (nth 1 wnw) 
		       :position-hash mhash))))

(defun create-file-index-from-plain-file (filename)
  "this one's for if for some reason you are indexing local files"
  (with-open-file (stream filename)
    (create-file-index (slurp-stream4 stream) filename)))

(defun pathescape (str)
  (substitute #\! #\* (substitute #\_ #\/ str)))

(defun read-file-index-from-file (filename)
  (with-open-file (stream filename)
    (let ((ioraw ()))
      (with-standard-io-syntax
	(setf ioraw (read stream)))
	     (make-file-index
	      :url (getf ioraw :url)
	      :title (getf ioraw :title)
	      :keywords (getf ioraw :keywords)
	      :description (getf ioraw :description)
	      :leadingtext (getf ioraw :leadingtext)
	      :timeindexed (getf ioraw :timeindexed)
	      :keywords-freq-hash (alist-hash-table (getf ioraw :keywords-freq-plist) :test #'equalp)
	      :nwords-in-kfh (getf ioraw :nwords-in-kfh)
	      :outgoinglinks (getf ioraw :outgoinglinks)
	      :totnumwords (getf ioraw :totnumwords)
	      :position-hash (alist-hash-table (getf ioraw :position-plist) :test #'equalp)))))


;;;WORDENTRY STUFF
(defstruct wordentry
  (numpositions 0) ;keep this calculated for ease
  (positions ()))

(defun add-to-wordentry (wordentry position)
  (with-slots (numpositions positions) wordentry
	(setf numpositions (1+ numpositions))
	(push position positions))
  wordentry)

(defun reverse-wordentry (wordentry)
  (with-slots (positions) wordentry
  (setf positions (reverse positions)))
  wordentry)

(defmethod make-load-form ((w wordentry) &optional env)
  "aids in reading in wordentrys from file"
  (declare (ignore env))
  (make-load-form-saving-slots w))


;;;STUFF THAT CAN PROBABLY STAY PRIVATE
(defun write-file-index-to-file (fileindex filename)
  "returns filename"
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (write-file-index-to-stream fileindex out)))
  filename)

(defun write-file-index-to-stream (fileindex stream)
  "takes in list of file-stat objects, a filename, and header text
   writes the filestats data and header in readable form to the file specified by filename"
  (print (file-index-to-string fileindex) stream))

(defun file-index-to-string (fileindex)
  (with-slots (url title keywords description leadingtext timeindexed keywords-freq-hash nwords-in-kfh outgoinglinks totnumwords position-hash) fileindex
    (list 
     :url url
     :title title
     :keywords keywords
     :description description
     :leadingtext leadingtext
     :timeindexed timeindexed
     :keywords-freq-plist (hash-table-alist keywords-freq-hash)
     :nwords-in-kfh nwords-in-kfh
     :outgoinglinks outgoinglinks
     :totnumwords totnumwords
     :position-plist (hash-table-alist position-hash) 
     ))) 


(defun create-keywords-freq-hash (title description keywords-list)
  "creates a freq-hash of the header content of a page, for inclusion
   in file-index"
  (let* ((out (make-hash-table :test #'equalp))
	(wlist (nconc (split-and-strip title) 
		      (split-and-strip description)
		      keywords-list))
	(nwords (add-to-freq-table wlist out 0)))
    (values out nwords)
    ))

(defun add-to-freq-table (wordlist freq-hash position)
  "adds the entries in wordlist to freq-hash, incrementing position
   as it goes.  returns the final position."
  (let ((pos position))
    (loop for word in wordlist do
	 (multiple-value-bind (v exists) (gethash word freq-hash)
	   (if exists
	       (add-to-wordentry v pos)
	       ;;^then we shall increment the word count for this word!
	       (setf (gethash word freq-hash) (make-wordentry :numpositions 1 :positions (list pos)))
	       ;;^else we are adding the first occurrence of this word.
	       )
	   (setf pos (1+ pos))))
    pos))

;;;EXPORT ALL FUNCTION SYMBOLS FROM THIS PACKAGE!
(let ((pack (find-package :com.cvberry.file-index)))
  (do-all-symbols (sym pack) 
    (when (and (eql (symbol-package sym) pack)
	       (handler-case (symbol-function sym)
		 (error (text) (declare (ignore text)) nil)))
      ;;(format t "exporting ~a from test-file! ~%" sym)
      (export sym))))
