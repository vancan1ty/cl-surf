(defpackage :com.cvberry.controller
  (:nicknames :controller)
  (:use :common-lisp :alexandria :com.cvberry.util)
  (:import-from :com.cvberry.file-index :pathescape)
  (:import-from :com.cvberry.wordstat :bootstrap-image :*tothash* :*totnum*)
  (:import-from :com.cvberry.crawler :index-sites-wrapper :create-standard-index-site-p)
  (:import-from :com.cvberry.searcher :run-text-search)
  (:import-from :com.cvberry.mem-cache-handler :init-memcache :update-memcache)
  (:export :setup-search
	   :run-search
	   :*tinfo*
	   :*currentsite*
	   :init-totnum-tothash
	   :site-index-info-visited-hash
	   :site-index-info-memcache
	   :site-index-info-directory
	   :run-html-search
	   ))

(in-package :com.cvberry.controller)

(defstruct site-index-info
  (visited-hash ())
  (memcache ())
  (directory ()))

(defvar *tinfo* ())
(defvar *totnum* ())
(defvar *tothash* ())

(defparameter cvberrysite
  (list :siteroot "http://www.cvberry.com"
	:stayonsite t
	:depth 5
	:directory "index_cvberry/"
	:directories-to-avoid nil
))

(defparameter sossite 
  (list :siteroot "http://www.sosmath.com"
	:stayonsite t
	:depth 4
	:directory "index_sosmath/"
	:directories-to-avoid '("http://www.sosmath.com/CBB" "http://www.sosmath.com/memberlist")
))

(defparameter clikisite
  (list :siteroot "http://www.cliki.net"
	:stayonsite t
	:depth 6
	:directory "index_cliki/"
	:directories-to-avoid '("http://www.cliki.net/site")))

;; (defparameter dailywtfsite
;;   (list :siteroot "http://www.thedailywtf.com"
;; 	:stayonsite t
;; 	:depth 4
;; 	:directory "thedailywtfindex/"
;; 	:directories-to-avoid (make-directories-to-avoid-list "http://www.thedailywtf.com" '(
;; 								"/Resources/"
;; 								"/Admin/"
;; 								"/Comments/"))))

(defparameter greystonesite
  (list :siteroot "http://www.campgreystone.com"
	:stayonsite t
	:depth 5
	:directory "index_greystone/"
	:directories-to-avoid '("http://www.campgreystone.com/live" "http://www.campgreystone.com/news/photos")))


(defparameter *currentsite* cvberrysite)

(defun make-directories-to-avoid-list (root dirs)
  (loop for dir in dirs collect
       (concatenate 'string root dir)))

(defun setup-search-wrapper ()
  (setf *tinfo* (setup-search 
			      (getf *currentsite* :siteroot) 
			      (getf *currentsite* :directory) 
			      (getf *currentsite* :stayonsite) 
			      (getf *currentsite* :depth)
			      (getf *currentsite* :directories-to-avoid))))

(defun setup-search (baseurl directory stayonsite depth &optional (directories-to-avoid nil))
  (if (not (boundp '*tothash*))
      (com.cvberry.wordstat:bootstrap-image))
  (let* (;(mdirectory (concatenate 'string "index_" (pathescape baseurl) "/"))
	 (mdirectory directory)
	 (mvisited-hash
	  (crawler:index-sites-wrapper (list baseurl) depth mdirectory (crawler:create-standard-index-site-p 100000000) :stay-on-sites stayonsite :directories-to-avoid directories-to-avoid))
	 (mmemcache (mchandler:init-memcache (concatenate 'string mdirectory "*.*"))))
    (make-site-index-info :visited-hash mvisited-hash :memcache mmemcache :directory mdirectory)))

(defun restore-search ()
  (let ((visitedhash
	(alist-hash-table 
	 (loop for file in 
	      (directory (concatenate 'string (getf *currentsite* :directory) "*.*")) collect
	      (let ((filecontents (with-open-file (stream file) (with-standard-io-syntax (read stream)))))
		(cons (getf filecontents :url) (getf filecontents :timeindexed))))))
  (memcache (mchandler:init-memcache (concatenate 'string (getf *currentsite* :directory) "*.*")))
  (directory (getf *currentsite* :directory)))
  (setf *tinfo* (make-site-index-info :visited-hash visitedhash :memcache memcache :directory directory))))

(defun index-single-web-page-wrapper (url)
  (crawler:index-single-web-page (site-index-info-memcache *tinfo*) url (site-index-info-visited-hash *tinfo*) (site-index-info-directory *tinfo*)))

(defun run-search (siteindexinfo querytext start end)
  (with-slots (visited-hash memcache directory) siteindexinfo
    (searcher:run-text-search memcache directory querytext start end)))

(defun run-html-search (siteindexinfo querytext start end)
  (with-slots (visited-hash memcache directory) siteindexinfo
    (searcher:run-html-search memcache directory querytext start end)))

(defun init-totnum-tothash ()
  (let ((totdata (with-open-file (stream "totdata.lisp") (read stream))))
    (setf *totnum* (getf totdata :totnum))
    (setf *tothash* (alist-hash-table (getf totdata :tothash))))
  ())
