(common-lisp:defpackage :css-selectors.test
  (:use :common-lisp :iterate :lisp-unit :arnesi))

(common-lisp:in-package :css-selectors.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-logger 'css)
    (deflogger css ()
      :level arnesi:+debug+
      :appender (make-instance 'adwutils:useful-stream-log-appender
			       :stream *debug-io*))))

(with-package-iterator (sym '(:css-selectors) :internal)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :css-selectors) pkg)
	    (ignore-errors (import (list symbol) :css-selectors.test)))
	  (while more?))))

(defmacro deftest (name (&rest args) &body body)
  (iter (for tag in args)
	(setf (get tag :tests)
	      (union (ensure-list (get tag :tests))
		     (list name))))
  `(lisp-unit:define-test ,name
     ,@body))

(defun run-tests-with-debugging (&key suites tests)
  (let* ((lisp-unit::*use-debugger* T)
	 (tests (append (ensure-list tests)
			(iter (for suite in (ensure-list suites))
			      (appending (get suite :tests)))))
	 (out (with-output-to-string (lisp-unit::*lisp-unit-stream*)
		(lisp-unit::run-test-thunks
		 (lisp-unit::get-test-thunks
		  (if (null tests) (get-tests *package*) tests))))))
    (css.info "~% ** TEST RESULTS ** ~%-----------~%~A~%------------~%" out)))