(common-lisp:in-package :css-selectors.test)

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

(defun matcher-basic ()
  ;; just proving the compiler macros are doing something
  ;; this takes .001 seconds vs the 1.25 sec of the test below
  (adwutils:time-and-log-around (css.info "matcher-basic")
    (node-matches? +footer+ "div,span,label")
    (node-matches? +footer+ "div#footer")
    (node-matches? +footer+ "#page #footer")
    (node-matches? +footer+ "#page #footer.layout.box")
    (node-matches? +footer+ "#page #footer.box.layout")
    (node-matches? +footer+ "body div#page> #footer.box.layout")
    (node-matches? +footer+ "#content + .box.layout")
    (node-matches? +footer+ "#header ~ .box.layout")
    (node-matches? +footer+ "[title]")
    (node-matches? +footer+ "[title|=page]")
    (node-matches? +footer+ "[title|='page']")
    (node-matches? +footer+ "#page > [title|=\"page\"]")
    (node-matches? +footer+ "#footer[class~=\"box\"]")
    (node-matches? +footer+ ".box[title^=\"this-be\"]")
    (node-matches? +footer+ "#header ~ [title$=footer-yo]")
    (node-matches? +footer+ ".box.layout[id=footer]")
    (node-matches? +footer+ ".box.layout[id=footer]:has( .contact-info )")
    (node-matches? +footer+ ".box.layout[id=footer]:has(.contact-info)")
    (node-matches? +footer+ ".box.layout[id=footer]:has(.contact-info>.name)")
    (node-matches? +footer+ ".box.layout[id=footer]:is(#footer)")
    (node-matches?
     +footer+
     ".box.layout[id=footer]:has( .contact-info > .name + .phone )")
    (node-matches? +footer+ ".box.layout[id=footer]:last-child")
    (node-matches? +header+ "#header:first-child")
    (node-matches? (dom:parent-node +header+) ":only-child")
    (node-matches? (dom:parent-node +header+) "*:only-child")))

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
  (assert-true (node-matches? +footer+ ".box.layout[id=footer]"))
  (assert-true (node-matches? +footer+ ".box.layout[id=footer]:has( .contact-info )"))
  (assert-true (node-matches? +footer+ ".box.layout[id=footer]:has(.contact-info)"))
  (assert-true (node-matches? +footer+ ".box.layout[id=footer]:has(.contact-info>.name)"))
  (assert-true (node-matches? +footer+ ".box.layout[id=footer]:is(#footer)"))
  (assert-true (node-matches? +footer+ ".box.layout[id=footer]:not(#header)"))
  (assert-true (node-matches? +footer+ ".box.layout[id=footer]:not( .contact-info )"))
  (assert-false (node-matches? +footer+ " #footer:nth-child(even)"))
  (assert-true (node-matches? +footer+ " #footer:nth-child(odd)"))
  (assert-false (node-matches? +footer+ " #footer:nth-last-child(even)"))
  (assert-true (node-matches? +footer+ " #footer:nth-last-child(odd)"))
  (assert-true (node-matches?
		+footer+
		".box.layout[id=footer]:has( .contact-info > .name + .phone )"))
  (assert-true (node-matches? +footer+ ".box.layout[id=footer]:last-child"))
  (assert-true (node-matches? +header+ "#header:first-child"))
  (assert-true (node-matches? (dom:parent-node +header+) ":only-child"))
  (assert-true (node-matches? (dom:parent-node +header+) "*:only-child")))

(deftest test-query (matcher query)
  (assert-eql 7 (length (query "div" +doc+)))
  (assert-eql +header+ (first (query "#page > div#header" +doc+)))
  (assert-eql +header+ (first (query "#page div#header" +doc+)))
  (assert-false (first (query "div#page" (query "div#page" +doc+)))))

