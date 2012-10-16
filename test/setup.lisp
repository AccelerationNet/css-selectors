(common-lisp:defpackage :css-selectors-test
  (:use :common-lisp :iterate :lisp-unit :css)
  (:shadow :run-tests))

(common-lisp:in-package :css-selectors-test)


(defun log-time (&optional (time (get-universal-time)) stream)
  "returns a date as ${mon}/${d}/${y} ${h}:${min}:{s}, defaults to get-universal-time"
  (multiple-value-bind ( s min h  )
      (decode-universal-time time)
    (format stream "~2,'0d:~2,'0d:~2,'0d "  h min s)))

(defun css.info (message &rest args)
  (format *standard-output* "~&")
  (log-time (get-universal-time) *standard-output*)
  (apply #'format *standard-output* message args)
  (format *standard-output* "~%"))

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defmacro log-around ((log-name message &rest args) &body body)
    "Logs the beginning and end of a body.  ARGS are evaluated twice"
    (let  ((gmessage (gensym "GMESSAGE-")))
      `(let ((,gmessage ,message))
	 (flet ((msg (&optional tag)
		  (format nil "~A ~a"
			  tag ,gmessage)))
	   (,log-name (msg "BEGIN") ,@args)
	   (multiple-value-prog1	     
	       (progn ,@body)
	     (,log-name (msg "  END") ,@args))))))

  (defmacro time-and-log-around ((log-name message &rest args) &body body)
    "Logs the beginning and end of a body.  ARGS are evaluated twice"
    (let  ((trace-output (gensym "TRACE-OUTPUT-")))
      `(let (,trace-output) ;;leave nil so the first log call doesn't print an extra newline
	 (log-around (,log-name ,(concatenate 'string message "~@[~%~a~]") ,@args ,trace-output)
	   (setf ,trace-output
		 (make-array 10 :element-type 'character :adjustable T :fill-pointer 0))
	   (with-output-to-string (*trace-output* ,trace-output)
	     (time (progn ,@body))))))))

(with-package-iterator (sym '(:css-selectors) :internal :external)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :css-selectors) pkg)
	    (ignore-errors
	      (unintern symbol :css-selectors-test)
	      (import (list symbol) :css-selectors-test)))
	  (while more?))))

(defmacro deftest (name (&rest args) &body body)
  (iter (for tag in args)
	(setf (get tag :tests)
	      (union (alexandria:ensure-list (get tag :tests))
		     (list name))))
  `(lisp-unit:define-test ,name
     (time-and-log-around (css.info "Running Test ~A" ',name)
       ,@body)))

(defmacro test-w/doc (name (&rest args) &body body)
  `(deftest ,name (,@args)
     (buildnode:with-html-document (progn ,@body nil))))

(defun run-tests (&key suites tests (use-debugger T))
  (let* ((*package* (find-package :css-selectors-test))
         (lisp-unit:*print-failures* t)
         (lisp-unit:*print-errors* t)
	 (lisp-unit::*use-debugger* use-debugger)
	 (tests (append (alexandria:ensure-list tests)
			(iter (for suite in (alexandria:ensure-list suites))
                          (appending (get suite :tests)))))
         (actual-std-out *standard-output*)
	 (out (with-output-to-string (s)
		(let ((*standard-output*
                        (make-broadcast-stream s actual-std-out)))
                  (if (null tests)
                      (lisp-unit::%run-all-thunks)
                      (lisp-unit::%run-thunks tests))))))
    (css.info
     "~&~% ** TEST RESULTS: CSS-SELECTORS ** ~%-----------~%~A~%------ END TEST RESULTS ------~%"
     out)))