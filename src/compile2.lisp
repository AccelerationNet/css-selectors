(in-package :css)

(defun make-or-matcher (forms)
  (let ((matchers (mapcar (lambda (f) (make-matcher-aux f)) forms)))
    (lambda (%node%)
      "or-matcher"
      (iter (for matcher in matchers)
	    (thereis (funcall matcher %node%))))))

(defun make-and-matcher (forms  )
  (let ((matchers (mapcar (lambda (f) (make-matcher-aux f)) forms)))
    (lambda (%node%)
      "and-matcher"
      (iter (for matcher in matchers)
	    (always (funcall matcher %node%))))))

(defun make-class-matcher ( class )
  (lambda (%node%)
    "class-matcher"
    (attrib-includes? %node% "class" class)))

(defun make-hash-matcher ( id )
  (lambda (%node%)
    "hash-matcher"
    (string-equal (buildnode:get-attribute %node% :id) id)))

(defun make-elt-matcher ( tag )
  (lambda (%node%)
    "elt-matcher"
    (string-equal (dom:tag-name %node%) tag)))

(defun make-attrib-matcher ( attrib match-type match-to   )
  "attrib-matcher"
  (lambda (%node%)
    (case match-type
      (:equals (string-equal (buildnode:get-attribute %node% attrib) match-to))
      (:includes (attrib-includes? %node% attrib match-to))
      (:dashmatch (member match-to
			  (cl-ppcre:split "-" (buildnode:get-attribute %node% attrib))
			  :test #'string-equal))
      (:begins-with (alexandria:starts-with-subseq
		     match-to
		     (buildnode:get-attribute %node% attrib)
		     :test #'char-equal))
      (:ends-with (alexandria:ends-with-subseq
		   match-to
		   (buildnode:get-attribute %node% attrib)
		   :test #'char-equal))
      (:substring (search match-to (buildnode:get-attribute %node% attrib)
			  :test #'string-equal ))
      (:exists (buildnode:get-attribute %node% attrib)))))

(defun make-immediate-child-matcher (parent-matcher child-matcher)
  (lambda (%node%)
    (and (funcall child-matcher %node%)
	 (dom:parent-node %node%)
	 (funcall parent-matcher (dom:parent-node %node%)))))

(defun make-child-matcher (parent-matcher child-matcher  )
  (lambda (%node%)
    (and (funcall child-matcher %node%)
	 (iter (for n in-dom-parents %node%)
	       (thereis (funcall parent-matcher n))))))

(defun make-immediatly-preceded-by-matcher (this-matcher sibling-matcher  )
  (lambda (%node%)
    (and (funcall this-matcher %node%)
	 (dom:previous-sibling %node%)
	 (funcall sibling-matcher (dom:previous-sibling %node%)))))

(defun make-preceded-by-matcher (this-matcher sibling-matcher  )
  (lambda (%node%)
    (and (funcall this-matcher %node%)
	 (iter (for n initially (dom:previous-sibling %node%)
		    then (dom:previous-sibling n))
	       (while n)
	       (thereis (funcall sibling-matcher n))))))

(defun make-pseudo-matcher (pseudo submatcher)
  (lambda (%node%) (funcall pseudo %node% submatcher)))

(defun make-nth-pseudo-matcher (pseudo mul add)
  (lambda (%node%) (funcall pseudo %node% mul add)))

(defun make-matcher-aux (tree)
  (ecase (typecase tree
	   (atom tree)
	   (list (car tree)))
    (:or (make-or-matcher (rest tree)  ))
    (:and (make-and-matcher (rest tree)  ))
    (:class (make-class-matcher (second tree)  ))
    (:hash (make-hash-matcher (second tree)  ))
    (:element (make-elt-matcher (second tree)  ))
    (:everything (lambda (%node%) (declare (ignore %node%)) T))
    (:attribute
       (let ((attrib (second tree)))
	 (ecase (length tree)
	   (2 (make-attrib-matcher attrib :exists nil  ))
	   (3 (destructuring-bind (match-type match-to) (third tree)
		(make-attrib-matcher attrib match-type match-to  ))))))
    (:immediate-child
       (make-immediate-child-matcher
	(make-matcher-aux (second tree))
	(make-matcher-aux (third tree))
	))
    (:child
       (make-child-matcher
	(make-matcher-aux (second tree))
	(make-matcher-aux (third tree))
	))
    (:immediatly-preceded-by
       (make-immediatly-preceded-by-matcher
	(make-matcher-aux (third tree))
	(make-matcher-aux (second tree))
	))
    (:preceded-by
       (make-preceded-by-matcher
	(make-matcher-aux (third tree))
	(make-matcher-aux (second tree))
	))
    (:pseudo
       (destructuring-bind (pseudo name &optional subselector) tree
	 (declare (ignore pseudo ))
	 (make-pseudo-matcher
	  (fdefinition (intern (string-upcase name) :pseudo))
	  (when subselector
	    (make-matcher-aux subselector  ))
	  )))
    (:nth-pseudo
       (destructuring-bind (pseudo name mul add) tree
	 (declare (ignore pseudo ))
	 (make-nth-pseudo-matcher
	  (fdefinition (intern (string-upcase name) :pseudo))
	  mul add  )))))

(defun make-node-matcher (expression)
  (make-matcher-aux
   (typecase expression
     (string (parse-results expression))
     (list expression))))
