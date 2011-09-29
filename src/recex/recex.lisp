(in-package :css)

(defparameter +css3-rex-file+
  (asdf:system-relative-pathname :css-selectors "src/recex/css3.rex"))

(defun make-css-dispatchers ()
  (let ((recex:*dispatchers*))
    (recex:clear-dispatchers)
    (recex:add-body-matcher "body")
    (recex:add-matched-pair-matcher "parens" #\( #\))
    (recex:read-rex-file-to-dispatchers  +css3-rex-file+)
    recex:*dispatchers*))

(defparameter +css3-selector-dispatchers+
  (make-css-dispatchers))

(defun recex-parse (inp &optional (regex #?r"(?<selector>)"))
  (let ((recex::*dispatchers* +css3-selector-dispatchers+)
        ;; trim whitespace, not really nec, but nice
        (inp (cl-ppcre:regex-replace-all #?r"(^[ \r\n\t\f]*|[ \r\n\t\f]*$)" inp ""))
        )
    (recex:regex-recursive-groups regex inp)))

(defun side-by-side-parse (inp)
  (list (second (multiple-value-list (recex-parse inp)))
        (parse-results inp)))

(defun timed-side-by-side-parse (inp &optional (iterations 1)
                                     &aux r1 r2)
  (time (iter (for i from 0 to iterations)
          (setf r1 (recex:treeify-regex-results (recex-parse inp)))))
  (time (iter (for i from 0 to iterations)
          (setf r2 (parse-results inp))))
  (format t "~&~S~&~S~&" r1 r2))


(defun run-some-tests ()
  (timed-side-by-side-parse ".foo" )
  (timed-side-by-side-parse "#bar" )
  (timed-side-by-side-parse ":bast" )
  (timed-side-by-side-parse "div[onclick]" )
  (timed-side-by-side-parse "div[onclick=foo]" )
  (timed-side-by-side-parse
   ":nth-last-child(  2n+1  ), foo.bar>bast:blech( .foo )" ))


