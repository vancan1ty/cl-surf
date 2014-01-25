(ql:quickload :cl-ppcre)
(ql:quickload :puri)
;(load "berryutils.lisp")
(ql:quickload :alexandria)
(ql:quickload :drakma)

(defpackage :com.cvberry.robotser
  (:use :common-lisp :alexandria :com.cvberry.util :cl-ppcre)
  (:import-from :cl-user))

(in-package :com.cvberry.robotser)

(defun process-site-robots (rooturl)
  (let ((robotstext (drakma:http-request (easy-uri-merge rooturl "robots.txt"))))
    (if robotstext
	(let* ((breezedirs (get-forbidden-dirs-for-agent rooturl robotstext "cl-breeze"))
	       (stardirs (get-forbidden-dirs-for-agent rooturl robotstext "*")))
	  (if stardirs
	      stardirs
	      breezedirs)))))

		     

(defun get-forbidden-dirs-for-agent (rooturl robotstext agent)
  (extract-disallow-dirs rooturl 
			 (select-agent agent 
				       (splitbyuseragent 
					(normalize-line-endings
					 (blank-out-comments
					  robotstext))))))

(defun select-agent (agent agentdata-alist)
  (cadr (assoc agent agentdata-alist :test #'equalp)))

(defun extract-disallow-dirs (rooturl robotslinelist)
  (declare (optimize (debug 3)))
  (let* ((disallowscanner (create-scanner "Disallow:(.*)" :case-insensitive-mode t))
	 (relativepaths (loop for line in robotslinelist
			     when (scan disallowscanner line)
			     collect
			     (register-groups-bind (path) (disallowscanner line)
						   (string-trim " " path)))))
    (mapcar (lambda (relpath) 
	      (easy-uri-merge rooturl relpath)) 
	      relativepaths)))

(defun splitbyuseragent (robotstext)
  (declare (optimize (debug 3)))
  (let ((uagentsearch (cl-ppcre:create-scanner "user-agent:" :case-insensitive-mode t :single-line-mode t)))
    (loop for str in 
	 (subseq (cl-ppcre:split uagentsearch robotstext) 1)
       collect
	 (let* ((after-split (cl-ppcre:split "\\n" str))
		(title (string-trim " " (elt after-split 0)))
		(others (subseq after-split 1)))
	   (cons title (list others))))))

(defun blank-out-comments (robotstext)
  (cl-ppcre:regex-replace-all "#.*" robotstext ""))

(defun normalize-line-endings (text)
  "replaces all sorts of weird line endings with the standard cl line ending #\newline"
  (cl-ppcre:regex-replace-all "(\\r|\\n)+" text (string #\newline)))
