;; (defpackage :com.cvberry.sscorer
;;   (:use :common-lisp :alexandria :com.cvberry.util :html5-parser :cl-ppcre)
;;   (:import-from :split-sequence :split-sequence)
;;   (:import-from :com.cvberry.wordstat *total-stat-store*))


;; (in-package :com.cvberry.sscorer)

(defun calc-word-weights (querylist bighash num-words-in-bighash)
  "returns weighted alist of standout words from query.
   querylist should be in a list of words from the query.
 algorithm:  40 (((100*smallratio - 100*bigratio))/(1 + sqrt(bigratio))) "
  (let ((orderedset (remove-dup-list-items querylist)))
    (loop for word in orderedset collect
	 (let* ((bigvalue (multiple-value-bind (v exists) (gethash word bighash)
			    (if v v 0)))
		(bigratio (* (/ bigvalue num-words-in-bighash) 100)))
	   (cons word (sqrt (if (eql bigratio 0)
				600
				(/ 1 bigratio))))))))

(defun remove-dup-list-items (mlist &key (test #'equal))
  (let ((dahash (make-hash-table :test test)))
    (loop for word in mlist nconc
	 (multiple-value-bind (v exists) (gethash word dahash)
	   (if exists
	       ()
	       (progn (setf (gethash word dahash) 1)
		      (list word)))))))

(defun word-freq-in-file-index (querylist fileindex keywords-weight)
  "returns alist with word frequency ratio for each word
   in fileindex.  now includes, and weights, keywords"
  (let ((orderedset (remove-dup-list-items querylist)))
  (with-slots (url totnumwords position-hash keywords-freq-hash nwords-in-kfh) fileindex
    (loop for word in orderedset collecting
	 (multiple-value-bind (v exists) (gethash word position-hash)
	   (multiple-value-bind (vk existsk) (gethash word keywords-freq-hash)
	     (cons word (+ 
			 (if exists
			     (/ (slot-value v 'numpositions) totnumwords)
			     0)
			 (if existsk
			     (* keywords-weight (/ (slot-value vk 'numpositions) nwords-in-kfh))
			     0)))))))))
  
(defun calc-doc-score (stringquery fileindex tothash nwordsin-tothash)
  (let* ((querylist (split-and-strip stringquery))
	 (freq-results (word-freq-in-file-index querylist fileindex 1))
	 (weight-results (calc-word-weights querylist tothash nwordsin-tothash)))
    (loop for (w1 . f) in freq-results
	  for (w2 . w) in weight-results summing
	 (* (sqrt f) w 5))))

(defun scoredocs (stringquery file-index-list tothash nwords-in-tothash)
  "returns an alist of file-index ptrs and their associated score"
  (loop for doc in file-index-list collecting
       (cons doc (calc-doc-score stringquery doc tothash nwords-in-tothash))))

(defun scoredocs-ht (stringquery visitedhash tothash nwords-in-tothash)
  "returns an alist of file-index ptrs and their associated score"
  (let ((out ()))
  (maphash (lambda (dockey doc) 
	     (setf out (cons (cons doc (calc-doc-score stringquery doc tothash nwords-in-tothash)) out))) 
	   findex-hashtable)
  out))

(defun update-word->docs-hash (word-docs-hash directory)
  "the first time around word-docs-hash can be a freshly created
   hash table.  it will be of the form word->{doc1->t doc2->t}"
  (loop for file in directory do
       (let* ((fileindex (read-file-index-from-file file))
	      (url (slot-value fileindex 'url)))
	 ;;loop for each word in file's position-hash and see if the words-hash properly links it
	 (maphash (lambda (word wordentry) 
		    (if (eql nil (gethash word word-docs-hash))
			(setf (gethash word word-docs-hash) 
			      (make-hash-table :test #'equalp))) 
		    (setf (gethash url (gethash word word-docs-hash)) t))
		  (slot-value fileindex 'position-hash))
	 ;;now do the same thing for keywords-freq-hash
	 (maphash (lambda (word wordentry) 
		    (if (eql nil (gethash word word-docs-hash))
			(setf (gethash word word-docs-hash) 
			      (make-hash-table :test #'equalp))) 
		    (setf (gethash url (gethash word word-docs-hash)) t))
		  (slot-value fileindex 'keywords-freq-hash))
     ;;now remove documents from the wordhash who no longer have words they used to
	 (maphash (lambda (word file-hash) 
		    (if (and (gethash url file-hash)
			     (not (word-in-fileindex-p word fileindex)))
			(remhash url file-hash)))
		  word-docs-hash))))

(update-word->docs-hash (make-hash-table :test #'equalp) "indexes4/")


(defun word-in-fileindex-p (word fileindex)
  (let ((wordinkeys (gethash word (slot-value fileindex 'keywords-freq-hash)))
	(wordinbody (gethash word (slot-value fileindex 'position-hash))))
    (if (or wordinkeys wordinbody)
	t
	nil)))


(defun get-top-matches (doc-score-alist start end)
  (let ((len (length doc-score-alist)))
    (if (> start len)
	(return-from get-top-matches ())
	(if (> end len)
	    (setf end len)))
    (subseq (sort doc-score-alist #'> :key #'cdr) start end)))

(defun get-header-word-list (fileindex)
  (let ((out ()))
    (with-slots (url title keywords description)
	(let* ((urlbase (get-url-base url))
	       (titlelist (split-and-strip title)))))))

(defun get-search-results (file-index-list stringquery start end)
  (get-top-matches (scoredocs stringquery file-index-list *tothash* *totnum*) start end))

(defun get-search-results-ht (visited-hash stringquery start end)
  (get-top-matches (scoredocs-ht stringquery visited-hash *tothash* *totnum*) start end))

(defun intersect-word->docs-hash (word-docshash words-list)
  "returns list of urls which contain all words in words-list.
   w/ exception that if a word is not found anywhere it is ignored..."  
  ;;this first step tells us the word with the fewest number of matching docs
  (let ((wordslist (loop for word in words-list collect
       (let ((wordhash (gethash word word-docshash)))
	 (if (or
	      (not wordhash) ;then no documents contain that word!
	      (eql 0 (hash-table-count wordhash)))
	     nil
	     (cons word (hash-table-count wordhash)))))))
    (let* ((sortedwlist (sort wordslist #'< :key #'cdr))
	   (minword (car (elt sortedwlist 0)))
	   (fileupperlist ()))
      (if (not minword)
	  (return-from intersect-word->docs-hash nil))
      (maphash (lambda (url inword) (if inword
					(setf fileupperlist (cons url fileupperlist))))
	       (gethash minword word-docshash))
      (loop for (nword . num) in sortedwlist do
	   (setf fileupperlist (remove nil (loop for url in fileupperlist collect
		(if (eql t (gethash url (gethash nword word-docshash)))
		    url
		    nil)))))
      fileupperlist)))

(defun sort-intersect-results (filelist words-list directory)
  (let ((fileindex-list (loop for url in filelist collect
			     (read-file-index-from-file (concatenate 'string directory (pathescape url))))))
    fileindex-list))

(defun disp-search-results (file-index-alist)
  "takes in sorted alist of form '((fileindex1 . score) (fileindex2 . score) ...)"
  (loop for (findex . score) in file-index-alist do
       (with-slots (url title description) findex
	 (format t "~7a=== ~a~%~a~%~a~%~%" score title description url))))


;;; Must design an algorithm to add a sequential factor to the doc score^
;; (defun calc-sequential-factor (querylist fileindex)
;;   (if (not querylist)
;;       0
;;       (loop for i from 0 to (length querylist) sum
;; 	   (multiple-value-bind (v exists) (slot-value fileindex 'position-hash)
;; 	     (let ((positions (slot-value v positions)))
;; 	       )))))


(defparameter *bootstrap-complete* nil)

(defun interactive-suggestions ()
  (setf *wsj-iran* (create-file-index-from-plain-file "/home/vancan1ty/shared/1332Project/wsjiranarticle.txt"))
  (setf *wsj-financial* (create-file-index-from-plain-file "/home/vancan1ty/shared/1332Project/wsjfinancialarticle.txt"))
  (setf *nytimes-iran* (create-file-index-from-plain-file "/home/vancan1ty/shared/1332Project/NYTimesIranArticle.txt"))
  (setf *wp-iran* (create-file-index-from-plain-file "/home/vancan1ty/shared/1332Project/washingtonpost_old_iran_article.txt"))
  (setf *tothash* (slot-value *total-stat-store* :wordhash))
  (setf *totnum* (slot-value *total-stat-store* :numwords)))


;(write-file-index-to-file *wsj-iran* (concatenate 'string (slot-value *wsj-iran* (concatenate 'string "indexes/" 'url))))

;example!!
;(disp-search-results (get-search-results (sort-intersect-results (intersect-word->docs-hash *tword->docshash* '("programming" "clojure")) '("programming" "clojure") "indexes4/") "programming clojure" 0 5))
;(update-word->docs-hash *tword->docshash* (directory "indexes4/*.*"))
