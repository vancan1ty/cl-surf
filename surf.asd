;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; Currell Bery - cl-surf search engine system definition file
;;; Right now this is a work in progress...
;;; License: Apache License 2.0. Also please attribute my work.
(ql:quickload :alexandria)
(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-html5-parser)
(ql:quickload :split-sequence)
(ql:quickload :hunchentoot)
(ql:quickload :cl-who)
(defpackage #:surf-asd
  (:use :cl :asdf))

(in-package :surf-asd)

(ql:quickload :drakma)

(defsystem surf
  :name "cl-surf"
  :version "0.2"
  :author "Currell Berry"
  :licence "Apache License 2.0 + please attribute my work."
  :description "Site search made easy"
  :depends-on (:alexandria :drakma :cl-ppcre :cl-html5-parser :split-sequence
			   :hunchentoot :cl-who)
  :components ((:file "berryutils")
	       (:file "otest" :depends-on ("berryutils"))
	       (:file "bootstrap-freq-data" :depends-on ("otest" "controller"))
	       (:file "stringops" :depends-on ("berryutils"))
	       (:file "file-index" :depends-on ("otest" "stringops"))
	       (:file "crawler" :depends-on ("file-index" "stringops" "mem-cache-handler"))
	       (:file "searcher" :depends-on ("file-index" "otest" "stringops"))
	       (:file "mem-cache-handler" :depends-on ("otest" "stringops" "searcher" "file-index"))
	       (:file "controller" :depends-on ("file-index" "otest" "crawler" "searcher" "mem-cache-handler"))
	       (:file "surf-ui" :depends-on ("controller" "berryutils"))))


(defsystem surf-test
  :name "surf-test"
  :description "unit tests for surf"
  :depends-on (:surf)
  :components ((:file "unittests")))
