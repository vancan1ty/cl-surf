(defpackage :com.cvberry.util
  (:use :common-lisp)
  (:export :break-transparent
	   :range
	   :prompt-read
	   :zip-plist
	   :enumerate-hash-table
	   :slurp-stream4
	   :simplify-line-endings
	   :easy-uri-merge
	   :slurp-file))

(in-package :com.cvberry.util)


;;;////////////////////////////////UTILITY CODE
(defmacro break-transparent (exp)
  `(let ((x ,exp)) (break "argument to break: ~:S" x) x))

(defun range (&key (min 0) (max 0) (step 1))
  "returns range from min to max inclusive of min
   exclusive of max"
   (loop for n from min below max by step
      collect n))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (read-line *query-io*))

(defun zip-plist (keys values)
  "creates a plist from a list of keys and a list of values."
  (loop for k in keys
        for v in values nconc
       (list k v)))
(defun enumerate-hash-table (hasht)
  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) hasht))

(defun slurp-stream4 (stream)
 (let ((seq (make-string (file-length stream))))
  (read-sequence seq stream)
  seq))

(defun pprint-plist (plist)
    (format t "~{~a:~10t~a~%~}~%" plist))

(defun simplify-line-endings (text)
  "replaces all sorts of weird line endings with the standard cl line ending #\newline"
  (cl-ppcre:REGEX-REPLACE-ALL "(\\r|\\n)+" text (string #\newline)))

(defun easy-uri-merge (rooturl relpath)
  "merges the relpath onto the root url, returns the result as a string"
  (with-output-to-string (ostring) 
    (puri:render-uri (puri:merge-uris relpath rooturl) 
		     ostring)))

(defun slurp-file (filename)
  (with-open-file (stream filename)
    (slurp-stream4 stream)))

;;;////////////////////////////////

