(common-lisp:in-package :css-selectors-test)

(defparameter +header+ nil)
(defparameter +footer+ nil)
(defparameter +article-title+ nil)
(defparameter +doc+
  (buildnode:with-html-document
    (xhtml:html ()
      (xhtml:head ()
	(xhtml:title () "My Page Title"))
      (xhtml:body ()
	(xhtml:div '(:id "page")
	  (setf
	   +header+
	   (xhtml:div '(:id "header")
	     (xhtml:div '(:class "title-holder")
	       (xhtml:h1 '(:class "title") "My Page Title"))
	     (xhtml:div '(:class"nav")
	       (xhtml:ul '(:class "nav-items")
		 (iter (for i from 0 to 10)
		       (collect
			   (xhtml:li () (format nil "Nav ~A" i))))))))
	  (xhtml:div '(:id "content")
	    (setf
	     +article-title+
	     (xhtml:h2 `(:id "article-title" :title "totally the title of this article"
			     :name "article-title-name")
	       "What its about")))
	  (setf
	   +footer+
	   (xhtml:div '(:id "footer" :class "layout box bottom-aligned"
			:title "this-be-the-page-footer-yo")
	     (xhtml:div '(:class "contact-info")
	       (xhtml:span '(:class "name") "Acceleration.net")
	       (xhtml:span '(:class "phone") "352-335-6500x123")))))))))

(defparameter +nodes-and-matching-selectors+
  `((,+footer+ "div,span,label"
	       "div#footer"
	       "#page #footer"
	       "#page #footer.layout.box"
	       "#page #footer.box.layout"
	       "body div#page> #footer.box.layout"
	       "#content + .box.layout"
	       "#header ~ .box.layout"
	       "[title]"
	       "[title|=page]"
	       "[title|='page']"
	       "#page > [title|=\"page\"]"
	       "#footer[class~=\"box\"]"
	       ".box[title^=\"this-be\"]"
	       "#header ~ [title$=footer-yo]"
	       ".box.layout[id=footer]"
	       ".box.layout[id=footer]:has( .contact-info )"
	       ".box.layout[id=footer]:has(.contact-info)"
	       ".box.layout[id=footer]:has(.contact-info>.name)"
	       ".box.layout[id=footer]:is(#footer)"
	       ".box.layout[id=footer]:not(#header)"
	       ".box.layout[id=footer]:not( .contact-info )"
	       " #footer:not( :nth-child(even) )"
	       " #footer:nth-child(odd)"
	       " #footer:not( :nth-last-child(even) )"
	       " #footer:nth-last-child(odd)"
	       " #footer:nth-last-child(1)"
	       " #footer:nth-last-child( 1 )"
	       " #footer:nth-child( +4n-1 )"
	       ".box.layout[id=footer]:has( .contact-info > .name + .phone )"
	       ".box.layout[id=footer]:last-child")
    (,+header+ " #header:nth-last-child( 4n-1 )"
	       "#header:first-child")
    (,(dom:parent-node +header+)
      ":only-child"
      "*:only-child")))

(defparameter +nodes-and-matching-selectors-compiled1+
  (iter (for (n . selectors) in +nodes-and-matching-selectors+)
	(iter (for selector in selectors)
	      (format T "About to compile ~A with ~A~%" n selector)
	      (collect (list n (%compile-css-node-matcher-lambda selector))))))

(defparameter +nodes-and-matching-selectors-compiled2+
  (iter (for (n . selectors) in +nodes-and-matching-selectors+)
	(iter (for selector in selectors)
	      (format T "About to compile ~A with ~A~%" n selector)
	      (collect (list n (make-node-matcher selector))))))

(deftest test-basic-compiler1 (compiler)  
  (iter (for i from 0 to 100)
	(iter (for (n . selectors) in +nodes-and-matching-selectors+)
	      (iter (for selector in selectors)
		    (assert-true (%compile-css-node-matcher-lambda selector)
				 n selector)))))

(deftest test-basic-compiler2 (compiler)  
  (iter (for i from 0 to 100)
	(iter (for (n . selectors) in +nodes-and-matching-selectors+)
	      (iter (for selector in selectors)
		    (format T "About to compile ~A with ~A~%" n selector)
		    (assert-true (make-node-matcher selector)
				 n selector)))))

(deftest test-basic-compiler-execution1 (compiler)
  (iter (for i from 0 to 10000)
	(iter (for (n  selector) in +nodes-and-matching-selectors-compiled1+)
	      (assert-true (%node-matches? n selector)
			   n selector))))

(deftest test-basic-compiler-execution2 (compiler)
  (iter (for i from 0 to 10000)
	(iter (for (n  selector) in +nodes-and-matching-selectors-compiled2+)
	      (assert-true (%node-matches? n selector)
			   n selector))))

(deftest test-matcher-basic-compiler1 (matcher)    
  (iter (for (n . selectors) in +nodes-and-matching-selectors+)
	(iter (for selector in selectors)
	      (assert-true (%node-matches? n (%compile-css-node-matcher-lambda selector))
			   n selector))))

(deftest test-matcher-basic-compiler2 (matcher)  
  (iter (for (n . selectors) in +nodes-and-matching-selectors+)
	(iter (for selector in selectors)
	      (format T "~%About to match ~A with ~A~%-----------------~%"
		      n selector)
	      (assert-true (%node-matches? n (make-node-matcher selector) )
			   n selector))))

(deftest test-query (matcher query)
  (assert-eql 7 (length (query "div" +doc+)))
  (assert-eql +header+ (first (query "#page > div#header" +doc+)))
  (assert-eql +header+ (first (query "#page div#header" +doc+)))
  (assert-false (first (query "div#page" (query "div#page" +doc+)))))

