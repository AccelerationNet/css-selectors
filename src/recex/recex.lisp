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
  (list (recex:treeify-regex-results (recex-parse inp))
        (parse-results inp)))

(defun timed-side-by-side-parses (inps &optional (iterations 100))
  (format t "~&~S~&"
          (time
           (iter (for inp in inps)
             (collect
                 (iter
                   (with r)
                   (for i from 0 to iterations)
                   (setf r (recex-parse inp))
                   (finally (return (recex:treeify-regex-results r))))))))
  (format t "~&~S~&"
          (time
           (iter (for inp in inps)
             (collect
                 (iter
                   (with r)
                   (for i from 0 to iterations)
                   (setf r (parse-results inp))
                   (finally (return r))))))))


(defun run-some-tests ()
  (timed-side-by-side-parses
   (list ".foo" "#bar" ":bast" "div[onclick]"
         "div[onclick=foo]"
         ":nth-last-child(  2n+1  ), foo.bar>bast:blech( .foo )" )
   1000))


