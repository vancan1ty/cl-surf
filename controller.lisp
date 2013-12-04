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
	   ))

(in-package :com.cvberry.controller)

(defstruct site-index-info
  (visited-hash ())
  (memcache ())
  (directory ()))

(defun setup-search (vartoset baseurl stayonsite depth)
  (if (not (boundp '*tothash*))
      (com.cvberry.wordstat:bootstrap-image))
  (let* (;(mdirectory (concatenate 'string "index_" (pathescape baseurl) "/"))
	 (mdirectory "tdir2/")
	 (mvisited-hash
	  (crawler:index-sites-wrapper (list baseurl) depth mdirectory (crawler:create-standard-index-site-p 1000) :stay-on-sites t))
	 (mmemcache (mchandler:init-memcache (concatenate 'string mdirectory "*.*"))))
    (setf vartoset (make-site-index-info :visited-hash mvisited-hash :memcache mmemcache :directory mdirectory))))

(defun run-search (siteindexinfo querytext start end)
  (with-slots (visited-hash memcache directory) siteindexinfo
    (searcher:run-text-search memcache directory querytext start end)))
