(common-lisp:in-package :css-selectors.test)

(deftest basic-parse (parse)
  (assert-equal
   '(:selector (:class "foo"))
   (css::parse-results " .foo "))
  
  (assert-equal
   '(:selectors
     (:selector (:class "foo"))
     (:selector (:hash "bar")))
   (css::parse-results " .foo , #bar "))

  (assert-equal
   '(:selector (:immediate-child (:selector (:class "foo"))
		(:selector (:hash "bar"))))
   (css::parse-results " .foo > #bar "))
  
  (assert-equal
   '(:selector (:immediate-child (:selector (:class "foo"))
		(:selector (:hash "bar"))))
   (css::parse-results " .foo>#bar "))
  
  (assert-equal
   '(:selector :everything (:class "foo") (:element "bar") (:hash "bas"))
   (css::parse-results " * .foo bar #bas "))

  (assert-equal
   '(:selector (:and :everything (:class "foo")) (:element "bar") (:hash "bas"))
   (css::parse-results " *.foo bar #bas "))

  (assert-equal
   '(:selector (:and :everything (:class "foo")) (:and (:element "bar") (:hash "bas")))
   (css::parse-results " *.foo bar#bas "))
  
  )

(deftest attrib-parse (parse)
  (assert-equal
   '(:selector (:and (:element "foo") (:attribute "bar")))
   (css::parse-results " foo[bar]  "))

  (assert-equal
   '(:selector (:element "foo") (:attribute "bar"))
   (css::parse-results " foo  [bar]  "))

  (assert-equal
   '(:selector (:element "foo") (:attribute "bar" (:includes "bast")))
   (css::parse-results " foo  [bar~=bast]  "))

  (assert-equal
   '(:selector (:and (:element "foo") (:attribute "bar" (:includes "bast"))))
   (css::parse-results " foo[bar~=bast]  "))

  (assert-equal
   '(:selector (:attribute "bar" (:includes "bast")))
   (css::parse-results " [bar~=bast]  "))

  (assert-equal
   '(:selector (:attribute "bar" (:dashmatch "bast")))
   (css::parse-results " [bar|=bast]  "))

  (assert-equal
   '(:selector (:attribute "bar" (:substring "bast")))
   (css::parse-results " [bar*=bast]  "))

  (assert-equal
   '(:selector (:attribute "bar" (:equals "bast")))
   (css::parse-results " [bar='bast']  "))

  (assert-equal
   '(:selector (:attribute "bar" (:begins-with "bast")))
   (css::parse-results " [bar^='bast']  "))

  (assert-equal
   '(:selector (:attribute "bar" (:ends-with "bast")))
   (css::parse-results " [bar$='bast']  ")))

(deftest pseudo-parse (parse)
  (assert-equal
   '(:selector (:and (:element "foo") (:pseudo "link")))
   (css::parse-results " foo:link  "))

  (assert-equal
   '(:selector (:element "foo") (:pseudo "link"))
   (css::parse-results " foo :link  "))

  (assert-equal
   '(:selector (:and
		(:and (:element "label")
		 (:pseudo "not" (:selector (:attribute "for"))))
		(:pseudo "has" (:selector (:element "a")))))
   (css::parse-results " label:not([for]):has(a)"))
)


