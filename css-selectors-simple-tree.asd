(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.css-selectors.system)
    (defpackage :net.acceleration.css-selectors.system
      (:use :common-lisp :asdf))))

(in-package :net.acceleration.css-selectors.system)

(defsystem :css-selectors-simple-tree
    :description "An implementation of css selectors that interacts with cl-html5-parser's simple-tree"
    :author "death <github.com/death>"
    :licence "BSD"
    :components
    ((:module :src
	      :serial T
	      :components
	      ((:file "simple-tree"))))
    :depends-on (:css-selectors :cl-html5-parser))
