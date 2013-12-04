(defpackage :com.cvberry.stringops
  (:use :common-lisp :alexandria :com.cvberry.util)
  (:import-from :split-sequence :split-sequence)
  (:export  :split-and-strip))

(in-package :com.cvberry.stringops)

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

