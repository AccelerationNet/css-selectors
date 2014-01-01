(common-lisp:defpackage :css-selectors-test
  (:use :common-lisp :iterate :lisp-unit2-asserts :css)
  (:shadow :run-tests))

(common-lisp:in-package :css-selectors-test)


(defun log-time (&optional (time (get-universal-time)) stream)
  "returns a date as ${mon}/${d}/${y} ${h}:${min}:{s}, defaults to get-universal-time"
  (multiple-value-bind ( s min h  )
      (decode-universal-time time)
    (format stream "~2,'0d:~2,'0d:~2,'0d "  h min s)))

(defun css.info ( tag message &rest args)
  (setf tag (alexandria:ensure-list tag))
  (format lisp-unit2:*test-stream* "~&")
  (log-time (get-universal-time) lisp-unit2:*test-stream*)
  (when (apply #'format lisp-unit2:*test-stream* tag))
  (apply #'format lisp-unit2:*test-stream* message args)
  (format lisp-unit2:*test-stream* "~%"))

;; import internal css-selector symbols
(with-package-iterator (sym '(:css-selectors) :internal :external)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :css-selectors) pkg)
	    (ignore-errors
	      (unintern symbol :css-selectors-test)
	      (import (list symbol) :css-selectors-test)))
	  (while more?))))

(defmacro deftest (name (&rest args) &body body)
  `(lisp-unit2:define-test ,name (:tags '(,@args))
    ,@body))

(defmacro test-w/doc (name (&rest args) &body body)
  `(deftest ,name (,@args)
     (buildnode:with-html-document (progn ,@body nil))))

(defun run-tests (&key suites tests)
  (let* ((*package* (find-package :css-selectors-test)))
    (lisp-unit2:run-tests
     :name "css-selectors"
     :tags suites
     :tests tests
     :package (when (and (null suites) (null tests))
                :css-selectors-test)
     :run-contexts #'lisp-unit2:with-summary-context)))