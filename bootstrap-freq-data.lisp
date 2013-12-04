;;This code initializes the word frequency data from the brown corpus when loaded...
(defpackage :com.cvberry.bootstrap-freq-data
  (:nicknames :bootstrap-freq-data)
  (:use :common-lisp :com.cvberry.util :com.cvberry.wordstat)
  )

(in-package :com.cvberry.bootstrap-freq-data)

(defvar *bootstrap-complete* nil)

(if (not *bootstrap-complete*)
    (bootstrap-image))
