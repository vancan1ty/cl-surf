(defpackage :com.cvberry.crawler
  (:nicknames :crawler)
  (:use :common-lisp :alexandria :com.cvberry.util :html5-parser)
  (:import-from :com.cvberry.file-index 
		:store-file-index-to-disk 
		:create-file-index
		:file-index-url
		:file-index-outgoinglinks)
  (:import-from :split-sequence :split-sequence)
  (:import-from :com.cvberry.stringops :split-and-strip :is-prefix :get-leading-text)
  (:import-from :mchandler :update-memcache-single-file)
  (:import-from :cl-ppcre :scan)
  (:export :index-sites-wrapper
	   :create-standard-index-site-p
	   :index-single-web-page
	   ))

(in-package :com.cvberry.crawler)

(defun index-sites-wrapper (starturls maxdepth directory index-site-p &key (stay-on-sites nil) (directories-to-avoid nil))
  (let ((visited-hash (make-hash-table :test #'equalp)) ;use the urls as keys, the full object as values
	(baseurls (if stay-on-sites
		      (mapcar (lambda (url) (get-site-root url)) starturls)
		      nil)))
    (recursive-index-sites starturls visited-hash index-site-p 0 maxdepth directory :baseurls baseurls :directories-to-avoid directories-to-avoid)
    visited-hash
    ))

(defmacro create-standard-index-site-p (time-between)
  "creates a lambda which returns true if we should visit site again, false if not.
   visited-hash is of form {url => timelastvisited}.  time-between is time between visits to page!"
  `(lambda (url visited-hash directory)
     (multiple-value-bind (v exists) (gethash url visited-hash) 
       (if exists
	   (if (> (- (get-universal-time) v) ,time-between)
	       t)
       t))))

(defun flex-dom-map2 (recurse-p fn node)
  "fn is applied to each visited nodee
   recurse-p controls whether to visit the children of node"
  (if node
      (progn
	(funcall fn node) ;apply the function to the node
	(if (funcall recurse-p node)
	    (html5-parser:element-map-children (lambda (n-node) (flex-dom-map2 recurse-p fn n-node)) node)))))

(defun gettag (top-node tag) 
  (flex-dom-map2 
   #'standard-recurse-p
   (lambda (node) (if (equalp (html5-parser:node-name node) tag)
		      (return-from gettag node)))
   top-node))

(defun scrapetext2 (top-node)
  (remove-newlines (with-output-to-string (s) 
    (flex-dom-map2 
     #'standard-recurse-p
     (lambda (node) (if (equal (html5-parser:node-type node) :TEXT)
			(format s " ~a " (html5-parser:node-value node))))
     top-node))))


(defun remove-newlines (str)
  (remove-if (lambda (ch) (or (eql ch #\return)
			      (eql ch #\linefeed))) str))

(defun remove-excess-newlines (text)
  (coerce (remove nil 
		  (loop for char across text 
		     with innewline = nil collect
		       (if innewline
			   (if (not (or (eql char #\newline)
					(eql char #\linefeed))) ;then back to normal node
			       (progn
				 (setf innewline nil)
				 char))
			   (if (or (eql char #\newline)
				   (eql char #\linefeed))
			       (progn
				 (setf innewline t)
				 #\Space)
			       char)))) 
	  'string))



(defun get-title (dom-node)
  (flex-dom-map2 
   #'standard-recurse-p
   (lambda (node) (if (equalp (node-name node) "title")
		      (return-from get-title (node-value (node-first-child node)))))
   dom-node))

(defun get-meta-content-list (dom-node meta-name)
  (flex-dom-map2 
   #'standard-recurse-p
   (lambda (node) (if (equalp (node-name node) "meta")
		      (if (equalp (element-attribute node "name") meta-name)
			  (let ((content (element-attribute node "content")))
				(return-from get-meta-content-list (split-and-strip content))))))
   dom-node))

(defun get-description (dom-node)
  (flex-dom-map2 
   #'standard-recurse-p
   (lambda (node) (if (equalp (node-name node) "meta")
		      (if (equalp (element-attribute node "name") "description")
			  (let ((content (element-attribute node "content")))
				(return-from get-description content)))))
   dom-node))


(defun get-keywords (dom-node)
  (get-meta-content-list dom-node "keywords"))

(defun get-robots (dom-node)
  (get-meta-content-list dom-node "robots"))

(defun get-links (dom-node current-url)
  (let ((out ()))
    (flex-dom-map2 
     #'standard-recurse-p
     (lambda (node) 
       (if (equalp (node-name node) "a")
	   (if (element-attribute node "href") 
	       (let ((prospective-link 
		      (process-site-link (element-attribute node "href") current-url))) 
		 (if prospective-link
		     (push prospective-link out))))))
     dom-node)
    out))

(defun process-site-link (link currenturl)
  ;;first check if this is even an http link
  (handler-case 
      (progn
  (if (or (eql (length link) 0)
	  (eql (elt link 0) #\#))
      (return-from process-site-link nil))
  (if (ppcre:scan "^([a-zA-Z])+:.*$" link) ;then we do have a uri in front of link!
      (if (not (ppcre:scan "^http:.*$" link)) ;then we don't support the protocol!
	  (return-from process-site-link nil)))
  (if (ppcre:scan "#" link) ;don't support in page links
      (return-from process-site-link nil))
  (if (and (>= (length link) 5)
	   (equalp (subseq link 0 5) "http:"))  ;if it's an absolute link return it unharmed
      link 
      (with-output-to-string (collector) (puri:RENDER-URI (puri:merge-uris (puri:parse-uri link) (puri:parse-uri currenturl)) collector))))
    (error (text) (progn (format t "error in process-link ~a" text) nil))))
;;else let puri work its magic
      ;; (if (equalp (elt link 0) #\/) ;if starts with slash return root+link
      ;; 	  (let ((rooturl (get-site-root currenturl))) 
      ;; 	    (if rooturl
      ;; 		(concatenate 'string rooturl link)))
      ;; 	  (let ((fixedurl (if (equal #\/ (last-elt currenturl))  ;else this is relative link
      ;; 			      currenturl
      ;; 			      (if (ppcre:scan "^http://.*/.$*" currenturl) ;then I can strip down w/ slash
      ;; 				  (multiple-value-bind (start end) (ppcre:scan ".*/" currenturl)
      ;; 				    (subseq currenturl start end))
      ;; 				  (concatenate 'string currenturl "/")))))
      ;; 	    (concatenate 'string fixedurl link)))


(defun get-site-root (url)
  (let ((regexresults (multiple-value-list (ppcre:scan "(^http://[^/]*).*$" url))))
    (if (not (equal regexresults '(nil)))
	(let* ((rootstart 0)
	       (rootend (elt (elt regexresults 3) 0) ))
	  (subseq url rootstart rootend)))))


(defvar *webpages* ())

(defun index-web-page (url)
  "returns nil if the contents of url are not of mime type text/html or text/plain"
  (multiple-value-bind (body status headers uri) (drakma:http-request url :connection-timeout 5)
    (if (eql status 200)
	(let* ((content-type (cdr (assoc :CONTENT-TYPE headers))))
	  (if (ppcre:scan "text/html" content-type)
	      (return-from index-web-page (index-html-file body url)))
	  (if (ppcre:scan "text/plain" content-type)
	      (return-from index-web-page (index-text-file body url)))))))

(defun get-tip-text (top-dom-node description fulltext)
  (let ((leadingtextpossible (scrapetext2 (gettag (gettag top-dom-node "body") "p"))))
    (if (< (length leadingtextpossible) 15)
	(setf leadingtextpossible description))
    (if (< (length leadingtextpossible) 15) ;if it's STILL < 15
	(setf leadingtextpossible fulltext))
    (get-leading-text leadingtextpossible 200)))

(defun index-single-web-page (memcache url visited-hash directory)
  (let ((wpageindex (index-web-page url)))
    (if wpageindex
	(progn 
	  (let* ((filename (file-index:store-file-index-to-disk wpageindex directory)))
	  (setf (gethash (file-index-url wpageindex) visited-hash) (get-universal-time))
	  (update-memcache-single-file memcache filename))))))

(defun index-html-file (html url)
  (let ((dom-model (parse-html5 html)))
    (when dom-model
      (let* ((title (get-title dom-model))
	     (description (get-description dom-model))
	     (keywords (get-keywords dom-model))
	     (links (get-links dom-model url))
	     ;this is a hack below.  set the leading text of the site
	     ;to the contents of the first "p" in "body"
	     (textcontent (scrapetext2 (gettag dom-model "body")))
	     (tiptext (get-tip-text dom-model description textcontent))) 
	(create-file-index textcontent url title description keywords links tiptext)))))

(defun index-text-file (text url)
  (create-file-index text url))

(defun recursive-index-sites (starturls visited-hash index-site-p current-depth maxdepth directory &key (baseurls nil) (directories-to-avoid nil))
  "visited-hash is a reference to a hash table to which sites are added
   across recursive runs...
   if baseurls is specified, the crawler never leaves those top level domains"
					;(break-transparent current-depth)
  (let ((collectedlinks 
	 (loop for url in starturls nconc
	      (block loopblock
		(loop for baddir in directories-to-avoid do
		     (cons baddir url)
		     (if (is-prefix baddir url)
			 (return-from loopblock nil)))
		(if (funcall index-site-p url visited-hash directory)
		    (progn  
		      (if baseurls
			  (if (not (find (get-site-root url) baseurls :test #'equalp))
			      (return-from loopblock nil))) ;this implements the "stay 
		      (format t "indexing ~a~%" url)
		      (let ((wpageindex (handler-case (index-web-page url)
					  (error (text) (format t "error indexing ~a! ~a~%" url text)))
			      ))
			(if wpageindex
			    (progn 
			      (file-index:store-file-index-to-disk wpageindex directory)
			      (setf (gethash (file-index-url wpageindex) visited-hash) (get-universal-time))
			      (file-index-outgoinglinks wpageindex)
			      )
			    (format t "~a was not indexed.  Likely incorrect MIME type ~%" url)))))))))
    (if (not (equal current-depth maxdepth))
	(recursive-index-sites collectedlinks visited-hash index-site-p (1+ current-depth) maxdepth directory :baseurls baseurls :directories-to-avoid directories-to-avoid))))


(defun standard-recurse-p (node)
  "returns true only if you aren't trying to recurse into a script,
  style, or noscript tag."
  (not (or (equalp (node-name node) "script")
	   (equalp (node-name node) "style")
	   (equalp (node-name node) "noscript"))))



;;;INTERACTIVE SUGGESTIONS
;; (defun crawler-interactive-suggestions ()
;;   (setf *visited-hash2* (index-sites-wrapper '("http://weitz.de/drakma/") 1))
;;   (store-search-cache *visited-hash2*)
;;   (defparameter *visited-hash3* (make-hash-table :test #'equalp))
;;   (load-search-cache *visited-hash3*))

;; (defun xml-interactive-suggestions ()
;;   (setf *cvberrydom2* (parse-html5 (drakma:http-request "http://cvberry.com")))
;;   (setf *nytimes-chess* (parse-html5 (drakma:http-request "http://www.nytimes.com/2013/11/24/business/for-chess-a-would-be-white-knight.html?hp&_r=0" ))))

;; (defun scraperdemo (node nodeurl)
;;   (com.cvberry.wordstat:standout-words-print 
;;    (subseq (com.cvberry.wordstat:calc-standout-words 
;;     (slot-value (com.cvberry.wordstat:MAKE-FILE-STAT (scrapetext2 node) nodeurl) 'com.cvberry.wordstat::frequency-hash) 
;;     *tothash* 
;;     *totnum* ) 0 10)))
