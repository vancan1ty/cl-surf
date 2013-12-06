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
;<link href="/library/skin/tool_base.css" type="text/css" rel "stylesheet" media ="all" />
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
					   (:input :type "hidden" :name "end" :type "hidden" :value end))))
			   
			   (:form :action "/search" :method "GET"
				  (:input :type "submit" :class "navbutton" :value "Next")
				  (:input :type "hidden" :name "query" :value query)
				  (:input :type "hidden" :name "start" :type "hidden" :value end)
				  (:input :type "hidden" :name "end" :type "hidden" :value (+ 10 end))) (:span :id "positionticker" (fmt "displaying ~a to ~a" start end))))))))))

    ;; <form action="/html/" method="POST">
    ;;       <!-- <a rel="next" href="/html/?q=search&amp;t=D&amp;v=l&amp;l=us-en&amp;p=1&amp;s=30&amp;o=json&amp;dc=18&amp;api=d.js">Next Page &gt;</a> //-->
    ;;       <input type="submit" class='navbutton' value="Next">
    ;;       <input type="hidden" name="q" value="search">
    ;;       <input type="hidden" name="s" value="30">
    ;;       <input type="hidden" name="o" value="json">
    ;;       <input type="hidden" name="dc" value="18">
    ;;       <input type="hidden" name="api" value="d.js">


(defvar *macceptor* (make-instance 'hunchentoot:easy-acceptor :port 8080 
                                   :document-root *install-dir*
				   :access-log-destination *terminal-io*
				   :message-log-destination *terminal-io*))
(hunchentoot:start *macceptor*)

;; (defun getKMapsOnly (truthtable numinputs)
;;   (let* ((kmapoutput (handler-case (html-create-k-maps truthtable numinputs)
;; 		       (malformed-truth-table-error (se) (concatenate 'string "<br> ERROR: " (text se)))))
;; 	 (generated-html (if (eql (length kmapoutput) 0) "Invalid truth table input!" kmapoutput)))
;;     generated-html))

;; old code I had which demonstrated filling template
;; (defun getKMaps (truthtable numinputs)
;;   (let* ((kmapoutput (html-create-k-maps truthtable numinputs))
;; 	 (generated-html (list :content (if (eql (length kmapoutput) 0) "Invalid truth table input!" kmapoutput)))
;; 	 (dbgout (list :content truthtable))
;; 	 (html-template:*string-modifier* #'identity))
;;     (html-template:fill-and-print-template #p"./index.tmpl" generated-html :stream *standard-output*)))


(define-condition malformed-truth-table-error (error)
  ((text :initarg :text :reader text)))

