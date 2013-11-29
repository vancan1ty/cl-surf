(ql:quickload :split-sequence)
(ql:quickload :alexandria)
(ql:quickload :cl-heap)
(ql:quickload :cl-html5-parser)
(load "berryutils.lisp")
(load "otest.lisp")

;;;the code in this file transforms a stream of characters in which any xml/html/other supported formatting code
;;;has been stripped away into a file-index object.
;;;this object may then be written to disk or aggregated for searching purposes.

(defpackage :com.cvberry.search
  (:use :common-lisp :alexandria :com.cvberry.util :cl-heap :html5-parser :cl-ppcre)
  (:import-from :split-sequence :split-sequence)
  (:import-from :com.cvberry.wordstat :bootstrap-image *total-stat-store*))

(load "index-word-structs.lisp")

(in-package :com.cvberry.search)

(defun split-and-strip (string)
  "takes in a string and a delimiter predicate of one variable,
   returns a list of words in the string and number of words found."
  ;;first clean out punctuation
  (let* ((nstring 
	  (substitute-if-not 
	   #\Space 
	   #'(lambda (char) 
	       (let ((charcode (char-code char))) 
		 (if (and
		      (not (eql charcode (char-code #\-))) ;leave dashes in place
		      (or (< charcode (char-code #\A))
			  (> charcode (char-code #\z))
			  (and
			   (> charcode (char-code #\Z))
			   (< charcode (char-code #\a)))))
		     nil
		     char))) 
	   string))
	 (words 
	  (remove-if (lambda (v) (equal v "")) (loop for word in (split-sequence #\Space nstring  :remove-empty-subseqs t) collecting
	       (strip-word word)))) ;remove null entries after strip-word.
	 (numwords (length words)))
				;now do some light/simple stemming of words, and split them up
     (values words numwords)))

(defun strip-word (word)
  "this will need help before doing serious work w/ it."
  (if (equalp word "s") ;get rid of the solo s's caused by stripping possesives
      (setf word ""))
  (string-downcase word)) ;if we get this far...

(defun create-file-index (string-input url &optional (title "") (description ()) (keywords ()) (outgoinglinks ()))
  (let ((mhash (make-hash-table :test #'equal))
	(wnw (multiple-value-list (split-and-strip string-input))))
    (add-to-freq-table (nth 0 wnw) mhash 0)
    (make-file-index :url url :title title :keywords keywords :description description :outgoinglinks outgoinglinks :totnumwords (nth 1 wnw) :position-hash mhash)))

(defun create-file-index-from-plain-file (filename)
  (with-open-file (stream filename)
    (create-file-index (slurp-stream4 stream) filename)))

(defun add-to-freq-table (wordlist freq-hash position)
  (loop for word in wordlist 
     with pos = position do
       (multiple-value-bind (v exists) (gethash word freq-hash)
	 (if exists
	     (add-to-wordentry v pos)
					;^then we shall increment the word count for this word!
	     (setf (gethash word freq-hash) (make-wordentry :numpositions 1 :positions (list pos)))
					;^else we are adding the first occurrence of this word.
	     )
	(setf pos (1+ pos)))))
