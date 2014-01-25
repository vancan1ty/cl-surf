;;This code initializes the word frequency data from the brown corpus when loaded...
(defpackage :com.cvberry.bootstrap-freq-data
  (:nicknames :bootstrap-freq-data)
  (:use :common-lisp :com.cvberry.util :com.cvberry.wordstat :com.cvberry.controller)
  )

(in-package :com.cvberry.bootstrap-freq-data)

(defvar *bootstrap-complete* nil)

(init-totnum-tothash )
(setf *bootstrap-complete* t)
