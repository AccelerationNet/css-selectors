(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.css-selectors.system)
    (defpackage :net.acceleration.css-selectors.system
      (:use :common-lisp :asdf))))

(in-package :net.acceleration.css-selectors.system)
 
(defsystem :css-selectors-stp
    :description "An implementation of css selectors that interacts with cxml-stp"
    :author "<programmers@acceleration.net>"
    :licence "BSD"
    :components
    ((:module :src
	      :serial T
	      :components
	      ((:file "stp"))))
    :depends-on (:css-selectors :cxml-stp))