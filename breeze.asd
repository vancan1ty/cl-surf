;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; Currell Bery - cl-breeze search engine system definition file
;;; Right now this is a work in progress...
;;; License: Apache License 2.0. Also please attribute my work.

(defpackage #:breeze-asd
  (:use :cl :asdf))


(in-package :breeze-asd)

(ql:quickload :drakma)

(defsystem breeze
  :name "cl-breeze"
  :version "0.2"
  :author "Currell Berry"
  :licence "Apache License 2.0 + please attribute my work."
  :description "Site search made easy"
  :depends-on (:alexandria :drakma :cl-ppcre :cl-html5-parser :split-sequence
			   :hunchentoot :cl-who)
  :pathname "/home/vancan1ty/1332ProjectWorking/"
  :components ((:file "berryutils")
	       (:file "otest" :depends-on ("berryutils"))
	       (:file "bootstrap-freq-data" :depends-on ("otest" "controller"))
	       (:file "stringops" :depends-on ("berryutils"))
	       (:file "file-index" :depends-on ("otest" "stringops"))
	       (:file "crawler" :depends-on ("file-index" "stringops" "mem-cache-handler"))
	       (:file "searcher" :depends-on ("file-index" "otest" "stringops"))
	       (:file "mem-cache-handler" :depends-on ("otest" "stringops" "searcher" "file-index"))
	       (:file "controller" :depends-on ("file-index" "otest" "crawler" "searcher" "mem-cache-handler"))
	       (:file "breeze-ui" :depends-on ("controller" "berryutils"))))


(defsystem breeze-test
  :name "breeze-test"
  :description "unit tests for breeze"
  :depends-on (:breeze)
  :components ((:file "unittests")))
