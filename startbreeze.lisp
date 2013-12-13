(load "breeze.asd")
(asdf:load-system :breeze)

(defpackage startbreeze
  (:use :common-lisp :com.cvberry.util))

(in-package :startbreeze)

(defun startbreeze ()
  (let* ((currentsettings controller::*currentsite*)
	 (numinindex 
	  (length
	   (directory 
	    (concatenate 'string 
			 (getf currentsettings :directory) 
			 "*.*")))))
    (format t "Here are your current settings~%") 
    (pprint-plist currentsettings)
    (format t "there are ~a files in ~a~%" numinindex (getf currentsettings :directory))
    (format t "What would you like to do?~%
             (0) restore from disk~%
             (1) index this site again~%
             (2) index a different site~%")
    (let ((choice (parse-integer (prompt-read ""))))
      (cond ((eql choice 0) 
	     (format t "restoring search for ~a~%" (getf currentsettings :siteroot))
	     (com.cvberry.controller::restore-search))
	    ((eql choice 1)
	     (format t "reindexing ~a~%" (getf currentsettings :siteroot))
	     (com.cvberry.controller::setup-search-wrapper))
	    ((eql choice 3)
	     (format t "to scan a different site, open controller.lisp and follow the instructions there.~%"))
	    (t 
	     (format t "unsupported option!  try again~%")
	     (startbreeze))))))

;;AUTOMATICALLY START THINGS UP...
(startbreeze)
