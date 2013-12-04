;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:breeze-asd
  (:use :cl :asdf))

(in-package :breeze-asd)

(ql:quickload :drakma)

(defsystem breeze
  :name "cl-breeze"
  :version "0.0.1"
  :author "Currell Berry"
  :licence "BSD sans advertising clause (see file COPYING for details)"
  :description "Site search made easy"
  :depends-on (:alexandria :drakma :cl-ppcre :cl-html5-parser :split-sequence
			   :hunchentoot :cl-who)
  :pathname "/home/vancan1ty/1332ProjectWorking/"
  :components ((:file "berryutils")
	       (:file "otest" :depends-on ("berryutils"))
	       (:file "bootstrap-freq-data" :depends-on ("otest" "controller"))
	       (:file "stringops" :depends-on ("berryutils"))
	       (:file "file-index" :depends-on ("otest" "stringops"))
	       (:file "crawler" :depends-on ("file-index" "stringops"))
	       (:file "searcher" :depends-on ("file-index" "otest" "stringops"))
	       (:file "mem-cache-handler" :depends-on ("otest" "stringops" "searcher" "file-index"))
	       (:file "controller" :depends-on ("file-index" "otest" "crawler" "searcher" "mem-cache-handler"))
	       (:file "breeze-ui" :depends-on ("controller" "berryutils"))))



(defsystem breeze-test
  :name "breeze-test"
  :description "unit tests for breeze"
  :depends-on (:breeze)
  :components ((:file "unittests")))
