;;;; kmapper.lisp
;;;; author: Currell Berry
;;;; This program creates karnaugh maps from truth table inputs.
;;
;;;; You may use or modify this program for any purpose, but please 
;;;; include my name in the source.
;;;; make sure to set ttablepost uri to valid uri for your proxy schem
;;;; as well as document root
;;; ********************START WEB SPECIFIC CODE******************************
;;(ql:quickload "html-template")   ;used for commented out no-longer used snippet 
                                   ;later down in the file
(ql:quickload "cl-who")
(ql:quickload "hunchentoot")

(defpackage :breeze-web-interface
  (:use :common-lisp :hunchentoot :cl-who :com.cvberry.controller :com.cvberry.util))
(in-package :breeze-web-interface)

;;replace this with your installation directory...
(defvar *install-dir* #p"/home/vancan1ty/1332ProjectWorking/web/") 

(setf *dispatch-table*
      (list #'dispatch-easy-handlers))
(setf *show-lisp-errors-p* t
      *show-lisp-backtraces-p* t)

;;(setf hunchentoot:*catch-errors-p* nil) 

(define-easy-handler (easy-demop :uri "/search" :default-request-type :both)
    ((searchquery :request-type :post )) 
  ;;set numinputs and truthtableinput to correct values for post and get scenarios
  (let* ((searchtext (if searchquery searchquery)) 

	 (output (controller:run-search *tinfo* searchtext 0 10)))
    (log-message* :INFO "**********post input:~%searchquery: ~a~%" searchquery)

    ;(log-message* :INFO "doing post! output: ~a " output)
    output))

(define-easy-handler (easy-demo :uri "/search"
                                :default-request-type :get)
    ((query :parameter-type 'string))
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "cl-breeze web search"))
     (:body
      :style "margin: 20px"
      (:h1 (fmt "searching ~a..." (getf controller:*currentsite* :siteroot)))
      (:p (:form
           :method :get
           (:table
            :border 0 :cellpadding 5 :cellspacing 0
            (:tr 
                 (:td (:input :type :text
                                    :name "query"
                                    :value query))
                 (:td (:input :type :submit :value "Submit"))))))
      (:p "The string you entered was: <br>" (if query
						 (run-html-search controller:*tinfo* query 0 10) ""))))))


(defvar *macceptor* (make-instance 'hunchentoot:easy-acceptor :port 8080 
                                   :document-root *install-dir*
				   :access-log-destination *terminal-io*
				   :message-log-destination *terminal-io*))
(hunchentoot:start *macceptor*)

(defun getKMapsOnly (truthtable numinputs)
  (let* ((kmapoutput (handler-case (html-create-k-maps truthtable numinputs)
		       (malformed-truth-table-error (se) (concatenate 'string "<br> ERROR: " (text se)))))
	 (generated-html (if (eql (length kmapoutput) 0) "Invalid truth table input!" kmapoutput)))
    generated-html))

;; old code I had which demonstrated filling template
;; (defun getKMaps (truthtable numinputs)
;;   (let* ((kmapoutput (html-create-k-maps truthtable numinputs))
;; 	 (generated-html (list :content (if (eql (length kmapoutput) 0) "Invalid truth table input!" kmapoutput)))
;; 	 (dbgout (list :content truthtable))
;; 	 (html-template:*string-modifier* #'identity))
;;     (html-template:fill-and-print-template #p"./index.tmpl" generated-html :stream *standard-output*)))


(define-condition malformed-truth-table-error (error)
  ((text :initarg :text :reader text)))

