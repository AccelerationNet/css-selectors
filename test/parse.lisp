(common-lisp:in-package :css-selectors-test)

(deftest basic-parse (parse)
  (assert-equal '(:class "foo")
   (css::parse-results " .foo "))

  (assert-equal
   '(:or
     (:class "foo")
     (:hash "bar"))
   (css::parse-results " .foo , #bar "))

  (assert-equal
   '(:immediate-child (:class "foo") (:hash "bar"))
   (css::parse-results " .foo > #bar "))

  (assert-equal
   ' (:immediate-child (:class "foo")
		       (:hash "bar"))
   (css::parse-results " .foo>#bar "))

  (assert-equal
   '(:child
     (:child (:child :everything (:class "foo"))
      (:element "bar"))
     (:hash "bas"))
   (css::parse-results " * .foo bar #bas "))

  (assert-equal
   '(:child (:child (:and :everything (:class "foo")) (:element "bar")) (:hash "bas"))
   (css::parse-results " *.foo bar #bas "))

  (assert-equal
   '(:child (:and :everything (:class "foo"))
     (:and (:element "bar") (:hash "bas")))
   (css::parse-results " *.foo bar#bas "))

  )

(deftest parse-underscores (parse)
  (assert-equal '(:class "foo-bar_bast")
   (css::parse-results " .foo-bar_bast "))
  (assert-equal '(:class "_foo")
   (css::parse-results " ._foo ")))

(deftest nth-x-pseudo (parse)
  (assert-equal
   '(:nth-pseudo "nth-child" 2 0)
   (css::parse-results " :nth-child(even) "))
  (assert-equal
   '(:nth-pseudo "nth-child" 2 1)
   (css::parse-results " :nth-child(odd) "))
  (assert-equal
   '(:nth-pseudo "nth-child" 2 0)
   (css::parse-results " :nth-child(    even  ) "))
  (assert-equal
   '(:nth-pseudo "nth-child" 2 1)
   (css::parse-results " :nth-child(  odd ) "))

  (assert-equal
   '(:nth-pseudo "nth-last-child" 2 0)
   (css::parse-results " :nth-last-child(2n) "))

  (assert-equal
   '(:nth-pseudo "nth-last-child" 2 0)
   (css::parse-results " :nth-last-child(  2n  ) "))

  (assert-equal
   '(:nth-pseudo "nth-last-child" 2 1)
   (css::parse-results " :nth-last-child(  2n+1  ) "))

  (assert-equal
   '(:nth-pseudo "nth-last-child" -2 0)
   (css::parse-results " :nth-last-child(  -2n  ) "))

  (assert-equal
   '(:nth-pseudo "nth-last-child" -1 0)
   (css::parse-results " :nth-last-child(  -1n  ) "))

  (assert-equal
   '(:nth-pseudo "nth-last-child" -2 -1)
   (css::parse-results " :nth-last-child(  -2n-1  ) "))

  (assert-equal
   '(:nth-pseudo "nth-last-child" 4 3)
   (css::parse-results " :nth-last-child(  4n+3  ) "))



  )

(deftest combinator-parse (parse)
  (assert-equal
   '(:or (:class "foo") (:class "bar"))
   (css::parse-results " .foo , .bar "))
  (assert-equal
   '(:or (:class "foo") (:class "bar"))
   (css::parse-results ".foo,.bar"))

  (assert-equal
   '(:or
     (:class "foo")
     (:immediatly-preceded-by
      (:immediate-child
       (:class "bar")
       (:hash "bar"))
      (:element "bast")))
   (css::parse-results " .foo , .bar > #bar + bast"))

  (assert-equal
   '(:or
     (:and (:element "a") (:class "foo"))
     (:or
      (:immediatly-preceded-by
       (:immediate-child
        (:and (:element "b") (:class "bar"))
	(:hash "bar"))
       (:element "bast"))
      (:and (:element "c") (:hash "foo"))))
   (css::parse-results " a.foo , b.bar > #bar + bast, c#foo"))

    (assert-equal
   '(:or
     (:and (:element "a") (:class "foo"))
     (:or
      (:immediatly-preceded-by
       (:immediate-child
        (:and (:element "b") (:class "bar"))
	(:hash "bar"))
       (:element "bast"))
      (:and (:element "c") (:hash "foo"))))
   (css::parse-results " a.foo , b.bar > #bar + bast, c#foo"))

  )

(deftest attrib-int-value-parse (parse)
  (assert-equal
   '(:AND (:ELEMENT "foo") (:ATTRIBUTE "bar" (:EQUALS "1")))
   (css::parse-results "foo[bar=1]")))

(deftest attrib-parse (parse)
  (assert-equal
   '(:and (:element "foo") (:attribute "bar"))
   (css::parse-results " foo[bar]  "))

  (assert-equal
   '(:child
     (:element "foo")
     (:attribute "bar"))
   (css::parse-results " foo  [bar]  "))

  (assert-equal
   '(:child (:element "foo")
     (:attribute "bar" (:includes "bast")))
   (css::parse-results " foo  [bar~=bast]  "))

  (assert-equal
   '(:and (:element "foo") (:attribute "bar" (:includes "bast")))
   (css::parse-results " foo[bar~=bast]  "))

  (assert-equal
   '(:attribute "bar" (:includes "bast"))
   (css::parse-results " [bar~=bast]  "))

  (assert-equal
   '(:attribute "bar" (:dashmatch "bast"))
   (css::parse-results " [bar|=bast]  "))

  (assert-equal
   '(:attribute "bar" (:substring "bast"))
   (css::parse-results " [bar*=bast]  "))

  (assert-equal
   '(:attribute "bar" (:equals "bast"))
   (css::parse-results " [bar='bast']  "))

  (assert-equal
   '(:attribute "bar" (:begins-with "bast"))
   (css::parse-results " [bar^='bast']  "))

  (assert-equal
   '(:attribute "bar" (:ends-with "bast"))
   (css::parse-results " [bar$='bast']  ")))

(deftest pseudo-parse (parse)
  (assert-equal
   '(:and (:element "foo") (:pseudo "link"))
   (css::parse-results " foo:link  "))

  (assert-equal
   '(:child (:element "foo") (:pseudo "link"))
   (css::parse-results " foo :link  "))

  (assert-equal
   '(:and
     (:and (:element "label")
      (:pseudo "not" (:attribute "for")))
     (:pseudo "has" (:element "a")))
   (css::parse-results " label:not([for]):has(a)"))

  (assert-equal
   '(:and
     (:and (:element "label")
      (:pseudo "not" (:attribute "for")))
     (:pseudo "has" (:and (:element "a") (:class "bar"))))
   (css::parse-results " label:not(  [for]   ):has( a.bar)"))

  (assert-equal
   '(:pseudo "not"
     (:or (:attribute "for")
      (:or (:and :everything (:class "foo"))
       (:immediate-child
	(:immediate-child
         (:element "a")
	 (:element "b"))
        (:element "c")))))
   (css::parse-results " :not(  [for],*.foo  ,  a>b > c )"))
  )

(deftest parse-failing-child-relationships (parse issue-8)
  (assert-equal
   `(:IMMEDIATE-CHILD
     (:IMMEDIATE-CHILD (:hash "it")
      (:CLASS "second"))
     (:ELEMENT "div"))
   (css::parse-results "#it > .second > div"))
  (assert-equal
   `(:IMMEDIATE-CHILD
     (:IMMEDIATE-CHILD (:CLASS "first")
      (:CLASS "second"))
     (:ELEMENT "div"))
   (css::parse-results ".first > .second > div"))
  (assert-equal
   `(:IMMEDIATE-CHILD
     (:IMMEDIATE-CHILD (:ELEMENT "div")
      (:ELEMENT "div"))
     (:ELEMENT "div"))
   (css::parse-results "div > div > div")))




