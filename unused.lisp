;;;stuff that was useful at one time but no more.
(defun store-search-cache (fileindex-ht directory)
  (maphash 
   (lambda (key doc)
       (store-file-index-to-disk doc directory))
   fileindex-ht))

(defun load-search-cache (fileindex-ht directory)
  "directory of form ./indexes/*.*"
  (loop for file in directory do
       (let* ((fileindex (read-file-index-from-file file))
	      (url (slot-value fileindex 'url)))
	 (setf (gethash url fileindex-ht) fileindex))))

(defun scoredocs-ht (stringquery visitedhash tothash nwords-in-tothash)
  "returns an alist of file-index ptrs and their associated score"
  (let ((out ()))
  (maphash (lambda (dockey doc) 
	     (setf out (cons (cons doc (calc-doc-score stringquery doc tothash nwords-in-tothash)) out))) 
	   findex-hashtable)
  out))

(defun get-header-word-list (fileindex)
  (let ((out ()))
    (with-slots (url title keywords description)
	(let* ((urlbase (get-url-base url))
	       (titlelist (split-and-strip title)))))))

(defun domview (node nestlevel)
  (if (not node)
      ()
      (progn
	(loop for i from 1 to nestlevel do
	     (format t " "))
	(format t "name: ~a~10ttype: ~a~10tattributes: ~a~10tvalue: ~a~%" 
		(dom:node-name node)
		(dom:node-type node)
		(dom:attributes node)
		(dom:node-value node))
	(dom:map-node-list (lambda (n-node) (domview n-node (1+ nestlevel)))
			   (dom:child-nodes node)))))

(defun dom-map (fn node)
  (if (not node)
      ()
      (progn
	(funcall fn node) ;apply the function to the node
	(dom:map-node-list (lambda (n-node) (dom-map fn n-node))
		   (dom:child-nodes node))))) 
