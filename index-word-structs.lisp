(ql:quickload :split-sequence)
(ql:quickload :alexandria)
;;;require "berryutils.lisp"

;;this file contains the file-index and wordentry structures
;;and some utility methods which operate on them.

(in-package :com.cvberry.search)

(defstruct file-index 
  (url ())
  (title ())
  (keywords ())
  (description "")
  (timeindexed ())
  (keywords-freq-hash (make-hash-table :test #'equalp))
  (nwords-in-kfh 0)
  (outgoinglinks ())
  (totnumwords 0)
  (position-hash (make-hash-table :test #'equalp)))
;;position-hash values are wordentry objects

(defun write-file-index-to-file (fileindex filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (write-file-index-to-stream fileindex out ))))


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
	      :timeindexed (getf ioraw :timeindexed)
	      :keywords-freq-hash (alist-hash-table (getf ioraw :keywords-freq-plist) :test #'equalp)
	      :outgoinglinks (getf ioraw :outgoinglinks)
	      :totnumwords (getf ioraw :totnumwords)
	      :position-hash (alist-hash-table (getf ioraw :position-plist) :test #'equalp)))))

(defun file-index-to-string (fileindex)
  (with-slots (url title keywords description outgoinglinks totnumwords position-hash) fileindex
    (list 
     :url url
     :title title
     :keywords keywords
     :description description
     :timeindexed timeindexed
     :keywords-freq-plist (hash-table-alist position-hash)
     :outgoinglinks outgoinglinks
     :totnumwords totnumwords
     :position-plist (hash-table-alist position-hash) 
     ))) 

(defun write-file-index-to-stream (fileindex stream)
  "takes in list of file-stat objects, a filename, and header text
   writes the filestats data and header in readable form to the file specified by filename"
  (print (file-index-to-string fileindex) stream))

(defun pathescape (str)
  (substitute #\! #\* (substitute #\_ #\/ str)))

(defun store-file-index-to-disk (fileindex directory)
  "directory of form indexes/"
  (write-file-index-to-file fileindex (concatenate 'string directory (pathescape (slot-value fileindex 'url)))))

(defun store-search-cache (fileindex-ht directory)
  (maphash 
   (lambda (key doc)
       (store-file-index-to-disk doc directory))
   fileindex-ht))

(defun load-search-cache (fileindex-ht directory)
  "directory of form ./indexes/*.*"
  (loop for file in directory do
       (let* ((fileindex (read-file-index-from-file file))
	      (url (slot-value fileindex 'url)))
	 (setf (gethash url fileindex-ht) fileindex))))

(defstruct wordentry
  (numpositions 0) ;keep this calculated for ease
  (positions ()))

(defmethod make-load-form ((w wordentry) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots w))

(defun add-to-wordentry (wordentry position)
  (with-slots (numpositions positions) wordentry
	(setf numpositions (1+ numpositions))
	(push position positions))
  wordentry)

(defun reverse-wordentry (wordentry)
  (with-slots (positions) wordentry
  (setf positions (reverse positions)))
  wordentry)

