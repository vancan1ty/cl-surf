(in-package :com.cvberry.search)
(ql:quickload :closure-html)

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

(defun word-freq-in-file-index (querylist fileindex)
  "returns alist with word frequency ratio for each word
   in fileindex"
  (let ((orderedset (remove-dup-list-items querylist)))
  (with-slots (url totnumwords position-hash) fileindex
    (loop for word in orderedset collecting
	 (multiple-value-bind (v exists) (gethash word position-hash)
	   (cons word (if exists
			  (/ (slot-value v 'numpositions) totnumwords)
			  0)))))))
			  
(defun calc-doc-score (stringquery fileindex tothash nwordsin-tothash)
  (let* ((querylist (split-and-strip stringquery))
	 (freq-results (word-freq-in-file-index querylist fileindex))
	 (weight-results (calc-word-weights querylist tothash nwordsin-tothash)))
    (loop for (w1 . f) in freq-results
	  for (w2 . w) in weight-results summing
	 (* (sqrt f) w 5))))

(defun scoredocs (stringquery file-index-list tothash nwords-in-tothash)
  "returns an alist of file-index ptrs and their associated score"
  (loop for doc in file-index-list collecting
       (cons doc (calc-doc-score stringquery doc tothash nwords-in-tothash))))

(defun scoredocs-ht (stringquery findex-hashtable tothash nwords-in-tothash)
  "returns an alist of file-index ptrs and their associated score"
  (let ((out ()))
  (maphash (lambda (dockey doc) 
	     (setf out (cons (cons doc (calc-doc-score stringquery doc tothash nwords-in-tothash)) out))) findex-hashtable)
  out))

(defun get-top-matches (doc-score-alist start end)
  (let ((len (length doc-score-alist)))
    (if (> start len)
	(return-from get-top-matches ())
	(if (> end len)
	    (setf end len)))
    (subseq (sort doc-score-alist #'> :key #'cdr) start end)))

(defun get-search-results (file-index-list stringquery start end)
  (get-top-matches (scoredocs stringquery file-index-list *tothash* *totnum*) start end))

(defun get-search-results-ht (file-index-ht stringquery start end)
  (get-top-matches (scoredocs-ht stringquery file-index-ht *tothash* *totnum*) start end))


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

(defun clean-html (string)
    (chtml:parse string (chtml:make-string-sink)))

