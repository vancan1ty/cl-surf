(defpackage :com.cvberry.test-file
  (:nicknames :test-file))

(in-package :com.cvberry.test-file)

(defun tester (alpha1 &optional (beta2 nil))
  (format t "alpha1 ~a beta2 ~a~%" alpha1 beta2))

(defstruct tstruct
  (firstfield ())
  (secondfield ()))
  

;;;EXPORTING ALL SYMBOLS FROM THIS PACKAGE!
(let ((pack (find-package :com.cvberry.test-file)))
  (do-all-symbols (sym pack) 
    (when (and (eql (symbol-package sym) pack)
	       (handler-case (symbol-function sym)
		 (error (text) (format t "~a~%" text))))
      (format t "exporting ~a from test-test! ~%" sym)
      (export sym))))
