;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:breeze-asd
  (:use :cl :asdf))

(in-package :breeze-asd)

(defsystem cow
  :name "cl-breeze"
  :version "0.0.1"
  :author "Currell Berry"
  :licence "BSD sans advertising clause (see file COPYING for details)"
  :description "Site search made easy"
  :serial t
  :components ((:file "unittests.lisp"
		    :depends-on ("package" "search"))
	       (:file 
  
