(defpackage :com.cvberry.mem-cache-handler
  (:nicknames :mchandler)
  (:use :common-lisp :alexandria :com.cvberry.util)
  (:import-from :split-sequence :split-sequence)
  (:import-from :com.cvberry.wordstat :bootstrap-image *total-stat-store*)
  (:import-from :com.cvberry.stringops :split-and-strip)
  (:import-from :com.cvberry.searcher :word-in-fileindex-p)
  (:import-from :com.cvberry.file-index 
		:read-file-index-from-disk
		:read-file-index-from-file)
  
  (:export :init-memcache
	   :update-memcache))

;;;This package encapsulates the double hash table (word -> filehash) which search-handler uses to keep track
;;;of whether a file contains word or not

;(update-word->docs-hash (make-hash-table :test #'equalp) "indexes4/")

(in-package :com.cvberry.mem-cache-handler)

(defun init-memcache (directory-name)
  (update-memcache (make-hash-table :test #'equalp) (directory directory-name)))

(defun update-memcache (memcache directory)
  "the first time around memcache (which is a word->docs-hashtable) can be a freshly created
   hash table.  it will be of the form word->{doc1->t doc2->t}"
  (loop for file in directory do
       (let* ((fileindex (file-index:read-file-index-from-file file))
	      (url (file-index:file-index-url fileindex)))
	 ;;loop for each word in file's position-hash and see if the words-hash properly links it
	 (maphash (lambda (word wordentry) 
		    (if (eql nil (gethash word memcache))
			(setf (gethash word memcache) 
			      (make-hash-table :test #'equalp))) 
		    (setf (gethash url (gethash word memcache)) t)
		    )
		  (file-index:file-index-position-hash fileindex))
	 ;;now do the same thing for keywords-freq-hash
	 (maphash (lambda (word wordentry) 
		    (if (eql nil (gethash word memcache))
			(setf (gethash word memcache) 
			      (make-hash-table :test #'equalp))) 
		    (setf (gethash url (gethash word memcache)) t)
		    )
		  (file-index:file-index-keywords-freq-hash fileindex))
     ;;now remove documents from the wordhash who no longer have words they used to
	 (maphash (lambda (word file-hash) 
		    (if (and (gethash url file-hash)
			     (not (word-in-fileindex-p word fileindex)))
			(break-transparent (remhash url file-hash)))
		    )
		  memcache)))
  memcache)

