;;;; author: Currell Berry
;;
;;;; You may use or modify this program for any purpose, but please 
;;;; include my name in the source.

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

;; (define-easy-handler (easy-demop :uri "/search" :default-request-type :both)
;;     ((searchquery :request-type :post )) 
;;   ;;set numinputs and truthtableinput to correct values for post and get scenarios
;;   (let* ((searchtext (if searchquery searchquery)) 

;; 	 (output (controller:run-search *tinfo* searchtext 0 10)))
;;     (log-message* :INFO "**********post input:~%searchquery: ~a~%" searchquery)

;;     ;(log-message* :INFO "doing post! output: ~a " output)
;;     output))

(define-easy-handler (easy-demo :uri "/search"
                                :default-request-type :get)
    ((query :parameter-type 'string)
     (start :parameter-type 'integer)
     (end :parameter-type 'integer))
  (if (eql start nil)
      (progn (setf start 0)
	     (setf end 10)))
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "cl-breeze web search")
	    (:link :href "styles.css" :type "text/css" :rel "stylesheet" nil))
     (:body
      (:div :id "wrapper"
	    (:div :id "toparea"
		  (:h1 "cl-breeze")
		  (:p (:form
		       :method :get
		       (:input :type :text
			       :id "querybox"
			       :name "query"
			       :value query)
		       (:input :id "submitbutton" :type :submit :value "Search")))
		  (:div :style "clear: both")
		  )
	    (:div :id "infoline" 
		  (:p (fmt "searching ~a..." (getf controller:*currentsite* :siteroot)))
		  )
	    (:div :id "resultsarea"
		  (:p (if query
			  (run-html-search controller:*tinfo* query start end) "")))
	    
	    (if (and query
		     (> (length query) 0))
		(htm (:div :id "bottomcontrols" 
			   (if (>= start 10) 
			       (htm (:form :action "/search" :method "GET"
					   (:input :type "submit" :class "navbutton" :value "Prev")
					   (:input :type "hidden" :name "query" :value query)
					   (:input :type "hidden" :name "start" :type "hidden" :value (- start 10))
					   (:input :type "hidden" :name "end" :type "hidden" :value start))))
			   
			   (:form :action "/search" :method "GET"
				  (:input :type "submit" :class "navbutton" :value "Next")
				  (:input :type "hidden" :name "query" :value query)
				  (:input :type "hidden" :name "start" :type "hidden" :value end)
				  (:input :type "hidden" :name "end" :type "hidden" :value (+ 10 end))) (:span :id "positionticker" (fmt "displaying ~a to ~a" start end))))))))))


(defvar *macceptor* (make-instance 'hunchentoot:easy-acceptor :port 8080 
                                   :document-root *install-dir*
				   :access-log-destination *terminal-io*
				   :message-log-destination *terminal-io*))

(if (hunchentoot::acceptor-shutdown-p *macceptor*)
    (hunchentoot:start *macceptor*)
    (progn
      (hunchentoot:stop *macceptor*)
      (hunchentoot:start *macceptor*)))
