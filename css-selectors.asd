(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.css-selectors.system)
    (defpackage :net.acceleration.css-selectors.system
      (:use :common-lisp :asdf))))

(in-package :net.acceleration.css-selectors.system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-system 'asdf-system-connections nil)
    (asdf:operate 'asdf:load-op 'asdf-system-connections)))
 
(defsystem :css-selectors
    :description "An implementation of css selectors"
    :author "<programmers@acceleration.net>"
    :licence "LGPL (or talk to me)"
    :version "0.1"
    :components
    ((:module :src
	      :serial T
	      :components
	      ((:file "packages")
	       (:file "parse")
	       (:file "pseudo")
	       (:file "compile"))
	       ))
    :depends-on (:asdf-system-connections
		 :iterate :yacc :cl-ppcre
		 :buildnode :alexandria :cxml))

(asdf:defsystem css-selectors-test
  :description "test for the css-selector library"
  :depends-on (:css-selectors :lisp-unit :buildnode-xhtml)
  :components ((:module :test
			:serial T
			:components
			((:file "setup")
			 (:file "parse")
			 (:file "selectors")
			 ))))