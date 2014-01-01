(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.css-selectors.system)
    (defpackage :net.acceleration.css-selectors.system
      (:use :common-lisp :asdf))))

(in-package :net.acceleration.css-selectors.system)
 
(defsystem :css-selectors
    :description "An implementation of css selectors"
    :author "<programmers@acceleration.net>"
    :licence "BSD"
    :components
    ((:module :src
	      :serial T
	      :components
	      ((:file "packages")
               (:file "node-api")
	       (:file "parse")
	       (:file "pseudo")
	       (:file "compile")
               (:file "dom"))
	       ))
    :depends-on (:iterate :yacc :cl-ppcre
		 :buildnode :alexandria
		 :cxml :cl-interpol :symbol-munger))

(asdf:defsystem css-selectors-test
  :description "test for the css-selector library"
  :depends-on (:css-selectors :lisp-unit2 :buildnode-xhtml)
  :components ((:module :test
			:serial T
			:components
			((:file "setup")
			 (:file "parse")
			 (:file "selectors")
			 ))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :css-selectors))))
  (asdf:oos 'asdf:load-op :css-selectors-test)
  (let ((*package* (find-package :css-selectors-test)))
    (eval (read-from-string "
             (run-tests)
      "))))

