(in-package :css)

(defun attrib-includes? (node attrib value)
  (member value
	  (cl-ppcre:split "\\s+" (buildnode:get-attribute node attrib))
	  :test #'string-equal))

(defun transform-css-parse-tree (tree)
  (case (typecase tree
	  (atom tree)
	  (list (car tree)))
    (:or `(or ,(transform-css-parse-tree (second tree))
	      ,(transform-css-parse-tree (third tree))))
    (:and `(and ,(transform-css-parse-tree (second tree))
		,(transform-css-parse-tree (third tree))))
    (:class `(attrib-includes? %node% "class" ,(second tree)))
    (:hash `(string-equal (buildnode:get-attribute %node% :id) ,(second tree)))
    (:element `(string-equal (dom:tag-name %node%) ,(second tree)))
    (:everything 'T)
    (:attribute
       (let ((attrib (second tree)))
	 (ecase (length tree)
	   (2 `(not (null (buildnode:get-attribute %node% ,attrib))))
	   (3 (destructuring-bind (match-type match-to) (third tree)
		(case match-type
		  (:equals `(string-equal (buildnode:get-attribute %node% ,attrib) ,match-to))
		  (:includes `(attrib-includes? %node% ,attrib ,match-to))
		  (:dashmatch `(member ,match-to
				       (cl-ppcre:split "-" (buildnode:get-attribute %node% ,attrib))
				       :test #'string-equal))
		  (:begins-with `(alexandria:starts-with-subseq
				  ,match-to
				  (buildnode:get-attribute %node% ,attrib)
				  :test #'char-equal))
		  (:ends-with `(alexandria:ends-with-subseq
				,match-to
				(buildnode:get-attribute %node% ,attrib)
				:test #'char-equal))
		  (:substring `(search ,match-to (buildnode:get-attribute %node% ,attrib)
				       :test #'string-equal ))))))))
    (:immediate-child
       `(and ,(transform-css-parse-tree (third tree))
	     (let ((%node% (dom:parent-node %node%)))
	       ,(transform-css-parse-tree (second tree)))))
    (:child
       (let ((n (gensym "N-")))
	 `(and ,(transform-css-parse-tree (third tree))
	       (iter (for ,n in-dom-parents %node%)
		     (let* ((%node% ,n))
		       (thereis ,(transform-css-parse-tree (second tree))))))))
    (:immediatly-preceded-by
       `(and ,(transform-css-parse-tree (third tree))
	     (let ((%node% (dom:previous-sibling %node%)))
	       (and %node% ,(transform-css-parse-tree (second tree))))))
    (:preceded-by
       (let ((n (gensym "N-")))
	 `(and ,(transform-css-parse-tree (third tree))
	       (iter (for ,n initially (dom:previous-sibling %node%)
			  then (dom:previous-sibling ,n))
		     (while ,n)
		     (let ((%node% ,n))
		       (thereis ,(transform-css-parse-tree (second tree))))))))
    (:pseudo
       (destructuring-bind (pseudo name &optional subselector) tree
	 (declare (ignore pseudo))
	 (when subselector
	   (setf subselector
		 (list
		  `(function ,(%compile-css-node-matcher-lambda subselector)))))
	 (let ((fn (intern (string-upcase name) :pseudo)))
	   `(,fn %node% ,@subselector))))
    (:nth-pseudo
       (destructuring-bind (pseudo name mul add) tree
	 (declare (ignore pseudo ))
	 (let ((fn (intern (string-upcase name) :pseudo)))
	   `(,fn %node% ,mul ,add))
	 ))
    ))

(defun %lisp-parse-results (inp)
  "inp should be either css-selector parse results or a string to be parsed
   this will be transformed into tree of lisp code"
  (transform-css-parse-tree
   (typecase inp
     (string (parse-results inp))
     (list inp))))

(defun %compile-css-node-matcher-lambda (inp)
  `(lambda (%node%) ,(%lisp-parse-results inp)))

(defun compile-css-node-matcher (inp)
  (typecase inp
    ((or string list)
       (compile nil (%compile-css-node-matcher-lambda inp)))
    (function inp)))

(defun %node-matches? (node inp)
  (funcall (compile-css-node-matcher inp) node))

(defun node-matches? (node inp)
  (%node-matches? node inp))

(define-compiler-macro node-matches? (node inp &environment e)
  `(%node-matches?
    ,node
    ,(if (constantp inp e)
	 `(function ,(%compile-css-node-matcher-lambda inp))
	 inp)))

(defun %query (inp &optional (trees buildnode:*document*))
  (let* ((matcher (compile-css-node-matcher inp)))
    ;; ensure that queries on a list dont return the items in that list
    (iter top
      (for tree in (alexandria:ensure-list trees))
      (iter (for n in-dom tree)
	    (when (and (not (eql tree n))
		       (typep n 'dom:element)
		       (funcall matcher n))
	      (in top (collect n)))))))

(defun query (inp &optional (trees buildnode:*document*))
  (%query inp trees))

(define-compiler-macro query (inp &optional (trees 'buildnode:*document*) &environment e)
  `(%query ,(if (constantp inp e)
		`(function ,(%compile-css-node-matcher-lambda inp))
		inp)
	   ,trees))



