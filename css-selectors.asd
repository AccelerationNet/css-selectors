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
    :depends-on (:iterate :yacc :cl-ppcre
		 :buildnode :alexandria
		 :cxml :cl-interpol))

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

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :css-selectors))))
  (asdf:oos 'asdf:load-op :css-selectors-test)
  (funcall (intern "RUN-TESTS" :css-selectors-test)
	   :use-debugger nil))

;;;; Copyright (C) 2011 Acceleration.net, Russ Tyndall
;;;;   email: bobbysmith007@gmail.com
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as published by
;;;; the Free Software Foundation, under version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; Copyright (C) 2011 Acceleration.net, Russ Tyndall
;;;;   email: bobbysmith007@gmail.com
;;;; This program comes with ABSOLUTELY NO WARRANTY; for details see COPYING.
;;;; This is free software, and you are welcome to redistribute it
;;;; under certain conditions; for details see COPYING.
