(defpackage :com.cvberry.searcher
  (:nicknames :searcher)
  (:use :common-lisp :alexandria :com.cvberry.util :file-index)
  (:import-from :split-sequence :split-sequence)
  (:import-from :com.cvberry.wordstat :bootstrap-image *total-stat-store* *tothash* *totnum*)
  (:import-from :com.cvberry.stringops :split-and-strip)
  (:export :disp-search-results 
	   :word-in-fileindex-p
	   :run-text-search))

(in-package :com.cvberry.searcher)

;example!!
;; (disp-search-results 
;;  (get-search-results 
;;   (sort-intersect-results 
;;    (intersect-word->docs-hash *tword->docshash* '("programming" "clojure")) 
;;    '("programming" "clojure") "indexes4/") 
;;   "programming clojure" 0 5))
;; ;

;(update-word->docs-hash *tword->docshash* (directory "indexes4/*.*"))

(defun run-text-search (memcache directory querystring start end)
  (let ((querylist (remove-dup-list-items (split-and-strip querystring) :test #'equalp)))
  (disp-search-results 
   (get-search-results 
    (sort-intersect-results 
     (intersect-memcache memcache 
			 querylist)
     querylist directory)
    querystring start end))))

(defun disp-search-results (file-index-alist)
  "takes in sorted alist of form '((fileindex1 . score) (fileindex2 . score) ...)"
  (loop for (findex . score) in file-index-alist do
       (with-accessors ((url file-index-url) 
			(title file-index-title) 
			(description file-index-description)) findex
	 (format t "~7a=== ~a~%~a~%~a~%~%" score title description url))))

(defun get-search-results (file-index-list stringquery start end)
  (get-top-matches (scoredocs stringquery file-index-list wordstat:*tothash* wordstat:*totnum*) start end))

(defun scoredocs (stringquery file-index-list tothash nwords-in-tothash)
  "returns an alist of file-index ptrs and their associated score"
  (loop for doc in file-index-list collecting
       (cons doc (calc-doc-score stringquery doc tothash nwords-in-tothash))))

(defun calc-doc-score (stringquery fileindex tothash nwordsin-tothash)
  (let* ((querylist (split-and-strip stringquery))
	 (freq-results (word-freq-in-file-index querylist fileindex 1))
	 (weight-results (calc-word-weights querylist tothash nwordsin-tothash)))
    (loop for (w1 . f) in freq-results
	  for (w2 . w) in weight-results summing
	 (* (sqrt f) w 5))))

(defun word-in-fileindex-p (word fileindex)
  (let ((wordinkeys (gethash word (file-index-keywords-freq-hash fileindex)))
	(wordinbody (gethash word (file-index-position-hash  fileindex))))
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

(defun sort-intersect-results (filelist words-list directory)
  (let ((fileindex-list (loop for url in filelist collect
			     (read-file-index-from-file (concatenate 'string directory (pathescape url))))))
    fileindex-list))

(defun intersect-memcache (memcache words-list)
  "returns list of urls which contain all words in words-list.
   w/ exception that if a word is not found anywhere it is ignored..."  
  ;;this first step tells us the word with the fewest number of matching docs
  (let ((wordslist (loop for word in words-list collect
			(let ((wordhash (gethash word memcache)))
			  (if (or
			       (not wordhash) ;then no documents contain that word!
			       (eql 0 (hash-table-count wordhash)))
			      nil
			      (cons word (hash-table-count wordhash)))))))
    (let* ((sortedwlist (sort wordslist #'< :key #'cdr))
	   (minword (car (elt sortedwlist 0)))
	   (fileupperlist ()))
      (if (not minword)
	  (return-from intersect-memcache nil))
      (maphash (lambda (url inword) (if inword
					(setf fileupperlist (cons url fileupperlist))))
	       (gethash minword memcache))
      (loop for (nword . num) in sortedwlist do
	   (setf fileupperlist (remove nil (loop for url in fileupperlist collect
						(if (eql t (gethash url (gethash nword memcache)))
						    url
						    nil)))))
      fileupperlist)))


;;; Must design an algorithm to add a sequential factor to the doc score^
;; (defun calc-sequential-factor (querylist fileindex)
;;   (if (not querylist)
;;       0
;;       (loop for i from 0 to (length querylist) sum
;; 	   (multiple-value-bind (v exists) (slot-value fileindex 'position-hash)
;; 	     (let ((positions (slot-value v positions)))
;; 	       )))))


(defparameter *bootstrap-complete* nil)

;; (defun interactive-suggestions ()
;;   (setf *wsj-iran* (create-file-index-from-plain-file "/home/vancan1ty/shared/1332Project/wsjiranarticle.txt"))
;;   (setf *wsj-financial* (create-file-index-from-plain-file "/home/vancan1ty/shared/1332Project/wsjfinancialarticle.txt"))
;;   (setf *nytimes-iran* (create-file-index-from-plain-file "/home/vancan1ty/shared/1332Project/NYTimesIranArticle.txt"))
;;   (setf *wp-iran* (create-file-index-from-plain-file "/home/vancan1ty/shared/1332Project/washingtonpost_old_iran_article.txt"))
;;   (setf *tothash* (slot-value *total-stat-store* :wordhash))
;;   (setf *totnum* (slot-value *total-stat-store* :numwords)))


;(write-file-index-to-file *wsj-iran* (concatenate 'string (slot-value *wsj-iran* (concatenate 'string "indexes/" 'url))))

;example!!
;(disp-search-results (get-search-results (sort-intersect-results (intersect-word->docs-hash *tword->docshash* '("programming" "clojure")) '("programming" "clojure") "indexes4/") "programming clojure" 0 5))
;(update-word->docs-hash *tword->docshash* (directory "indexes4/*.*"))

;;;PRIVATE STUFF BELOW!
(defun word-freq-in-file-index (querylist fileindex keywords-weight)
  "returns alist with word frequency ratio for each word
   in fileindex.  now includes, and weights, keywords (so not really plain frequency anymore)"
  (let ((orderedset (remove-dup-list-items querylist)))
  (with-accessors ((url file-index-url) 
		   (file file-index-file)
		   (totnumwords file-index-totnumwords)
		   (position-hash file-index-position-hash)
		   (keywords-freq-hash file-index-keywords-freq-hash)
		   (nwords-in-kfh file-index-nwords-in-kfh)) fileindex
    (loop for word in orderedset collecting
	 (multiple-value-bind (v exists) (gethash word position-hash)
	   (multiple-value-bind (vk existsk) (gethash word keywords-freq-hash)
	     (cons word (+ 
			 (if exists
			     (/ (wordentry-numpositions v) totnumwords)
			     0)
			 (if existsk
			     (* keywords-weight (/ (wordentry-numpositions vk) nwords-in-kfh))
			     0)))))))))

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
