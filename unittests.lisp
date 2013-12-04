;gigamonkeys unit testing!

(defpackage :com.cvberry.breeze-tester
  (:nicknames :tester)
  (:use :common-lisp 
	:alexandria 
	:com.cvberry.util 
	:html5-parser 
	:com.cvberry.file-index
	:com.cvberry.stringops
	:com.cvberry.crawler
	:com.cvberry.searcher 
	)
  (:import-from :split-sequence :split-sequence)
  (:import-from :com.cvberry.stringops :split-and-strip)
  (:import-from :cl-ppcre :scan)
  (:export :run-tests))

;;now import various symbols into current package...
(import 'com.cvberry.crawler::process-site-link)
(import 'com.cvberry.crawler::get-site-root)
(import 'com.cvberry.searcher::get-top-matches)
(import 'com.cvberry.stringops::strip-word)

(in-package :com.cvberry.breeze-tester)
(load "gigamonkeys.lisp")

(deftest run-tests ()
  (test-split-and-strip)
  (test-file-index-read-write)
  (test-add-to-freq-table)
  (test-get-top-matches)
  (test-url-handling))

(deftest test-basic () 
  (check 
    (eql 1 2)
    (eql 1 1)))

(deftest test-create-file-index ()
  (let ((tfi (create-file-index "hello there everyone.  how are you?" "hello.txt")))
    (check
      (and (eql 6 (slot-value tfi 'totnumwords))
	   (eql 6 (hash-table-count (slot-value tfi 'position-hash)))
	   (equal "hello.txt" (slot-value tfi 'url))))))

(deftest test-split-and-strip ()
  (check
    (equal (multiple-value-list (split-and-strip "Hi there.  How are you today?")) '(("hi" "there" "how" "are" "you" "today") 6))
    (equal (multiple-value-list (split-and-strip "What is God's plan for life?")) '(("what" "is" "god" "plan" "for" "life") 6))
    (equal (strip-word "Bob") (strip-word "bob"))
    (equal (strip-word "test") "test")))

(deftest test-file-index-read-write ()
  (let ((tfi (make-file-index :url "www.cvberry.com" :totnumwords 212 :position-hash (plist-hash-table (list "a" (make-wordentry :numpositions 3 :positions '(7 21 33)) "that" (make-wordentry :numpositions 2 :positions '(19 25))) :test #'equal))))
    (write-file-index-to-file tfi "iotest.lisp")
  (check
    ;;checks that make-file-index is generally working ok
    (equal '(19 25) (slot-value (gethash "that" (slot-value tfi 'position-hash)) 'positions))
    ;;checks the pair of write-file-index-to-file and read-file-index-from-file
    (equalp (read-file-index-from-file "iotest.lisp") tfi))))

(deftest test-add-to-freq-table ()
  (let ((mhash (make-hash-table :test #'equal)))
    (add-to-freq-table '("this" "is" "pretty" "darn" "big" "time" "it" "is") mhash 0)
    (check (equalp (hash-table-alist mhash) '(("time" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (5))) ("big" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (4))) ("darn" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (3))) ("pretty" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (2))) ("this" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (0))) ("it" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (6))) ("is" . #S(WORDENTRY :NUMPOSITIONS 2 :POSITIONS (7 1))))))
    mhash))

(deftest test-get-top-matches ()
  (check
    (equalp (get-top-matches (copy-list '(("z" . 12) ("d" . 5) ("r" . 33) ("o" . 7))) 0 5)
	    '(("r" . 33) ("z" . 12) ("o" . 7) ("d" . 5)))
    (equalp (get-top-matches (copy-list '(("z" . 12) ("d" . 5) ("r" . 33) ("o" . 7))) 5 6)
	    ())))

(deftest test-url-handling ()
  (check
    (equal (get-site-root "http://www.firecommons.cvberry.com/bla/bla/bla.txt") "http://www.firecommons.cvberry.com")
    (equal (get-site-root "www.firecommons.cvberry.com/bla/bla/bla.txt") nil)
    (equal (process-site-link "images/outdoors.jpg" "http://www.cvberry.com") "http://www.cvberry.com/images/outdoors.jpg")
    (equal (process-site-link "/images/outdoors.jpg" "http://www.cvberry.com/about/index.html") "http://www.cvberry.com/images/outdoors.jpg")
    (equal (process-site-link "http://www.cnn.com" "http://www.cvberry.com/about/index.html") "http://www.cnn.com")
    (equal (process-site-link "images/outdoors.jpg" "http://www.cvberry.com/index.html") "http://www.cvberry.com/images/outdoors.jpg")
    (equal (process-site-link "#" "http://www.cvberry.com/index.html") nil)
    (equal (process-site-link "mailto:cvberry.com" "http://www.cvberry.com/index.html") nil)
    (equalp (mapcar (lambda (url) (get-site-root url)) '("http://www.cvberry.com/index.html" "http://www.weitz.de/drakma/is/awesome_(really)")) 
	    '("http://www.cvberry.com" "http://www.weitz.de"))
    ))

(deftest test-create-keywords-freq-hash ()
  (check
    (equalp (hash-table-alist 
	    (create-keywords-freq-hash "This is my site" "A site about programming" "programming, lisp, c++, thinking about life")) 
	   '(("my" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (2))) ("site" . #S(WORDENTRY :NUMPOSITIONS 2 :POSITIONS (5 3))) ("about" . #S(WORDENTRY :NUMPOSITIONS 2 :POSITIONS (12 6))) ("lisp" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (9))) ("life" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (13))) ("c" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (10))) ("a" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (4))) ("thinking" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (11))) ("this" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (0))) ("is" . #S(WORDENTRY :NUMPOSITIONS 1 :POSITIONS (1))) ("programming" . #S(WORDENTRY :NUMPOSITIONS 2 :POSITIONS (8 7)))))
 (create-file-index "this is an index" "http://example.com" "site made by me" "a very good site" "bad,good,right,wrong")
    ))


;(let ((ilist ()))
;  (push (create-file-index "This is life.  A game you must do your best in." "life.txt") ilist))


