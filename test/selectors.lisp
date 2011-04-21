(common-lisp:in-package :css-selectors.test)

(defparameter +footer+ nil)
(defparameter +article-title+ nil)
(defparameter +doc+
  (buildnode:with-html-document
    (xhtml:html ()
      (xhtml:head ()
	(xhtml:title () "My Page Title"))
      (xhtml:body ()
	(xhtml:div '(:id "page")
	  (xhtml:div '(:id "header")
	    (xhtml:div '(:class "title-holder")
	      (xhtml:h1 '(:class "title") "My Page Title"))
	    (xhtml:div '(:class"nav")
	      (xhtml:ul '(:class "nav-items")
		(iter (for i from 0 to 10)
		      (collect
			  (xhtml:li () (format nil "Nav ~A" i)))))))
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
	       (xhtml:span '(:class "name")
		 "Acceleration.net")
	       (xhtml:span '(:class "phone")
		 "352-335-6500x123")))))))))

(deftest test-matcher-basic (matcher)
  (assert-true (node-matches? +footer+ "div,span,label"))
  (assert-true (node-matches? +footer+ "div#footer"))
  (assert-true (node-matches? +footer+ "#page #footer"))
  (assert-true (node-matches? +footer+ "#page #footer.layout.box"))
  (assert-true (node-matches? +footer+ "#page #footer.box.layout"))
  (assert-true (node-matches? +footer+ "body div#page> #footer.box.layout"))
  (assert-true (node-matches? +footer+ "#content + .box.layout"))
  (assert-true (node-matches? +footer+ "#header ~ .box.layout"))
  (assert-true (node-matches? +footer+ "[title]"))
  (assert-true (node-matches? +footer+ "[title|=page]"))
  (assert-true (node-matches? +footer+ "[title|='page']"))
  (assert-true (node-matches? +footer+ "#page > [title|=\"page\"]"))
  (assert-true (node-matches? +footer+ "#footer[class~=\"box\"]"))
  (assert-true (node-matches? +footer+ ".box[title^=\"this-be\"]"))
  (assert-true (node-matches? +footer+ "#header ~ [title$=footer-yo]"))
  (assert-true (node-matches? +footer+ ".box.layout[id=footer]")))

(deftest test-query (matcher query)
  (assert-true T))

