(in-package :css)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:disable-sql-reader-syntax)

(defparameter +option+ "%option\\s")
(defparameter +option-case-insensitive+ "%option\\scase-insensitive")
(defparameter +end-of-defs+ "^%%")
(defparameter +return-value+ "\\s*{\\s*return\\s*([^;]*)\\s*;\\s*}\\s*")

(defun replace-expansions (defs new-regex)
  (iter (for (k v) in-hashtable defs)
	(setf new-regex
	      (adwutils:replace-all
	       new-regex (format nil "{~A}" k) v )))
  new-regex)

(defun handle-quoted-rules (production)
  "quotes in flex productions should be direct string matches"
  `(:sequence :START-ANCHOR
    ,@(iter
       (with start = 0)
       (with idx-start = start)
       (with opening)
       (while (<= start (length production)))
       (when (< idx-start start) (setf idx-start start))
       (for idx = (position #\" production :test #'char-equal :start idx-start))
       (when (and idx (>= idx 1)
		  (char-equal #\\ (elt production (- idx 1))))
	 (setf idx-start (+ 1 idx))
	 (next-iteration))
       (cond
	 ((and idx (>= idx 0) opening)
	  (collect (subseq production opening idx ))
	  (setf start (+ 1 idx) opening nil))
	 ((and idx (>= idx 0))
	  (let ((frag (subseq production start idx)))
	    (when (plusp (length frag))
	      (collect (cl-ppcre:parse-string frag))))
	  (setf opening (+ 1 idx)
		start (+ 1 idx)))
	 (T
	  (let ((frag (subseq production start)))
	    (when (plusp (length frag))
	      (collect (cl-ppcre:parse-string frag))))
	  (finish))
	 ))))

(defun symbolize (s)
  (adwutils:symbolize-string s))

(defun keywordize (s)
  (adwutils:symbolize-string s :keyword))

(defun all-group-matches-as-string (regex target)
  "Expects a single matching group"
  (let (results)
    (cl-ppcre:do-register-groups (grp) (regex target (reverse results))
      (push grp results))))

(defun string-upcase? (s) (string= (string-upcase s) s))

(defun expand-return-value ( rtn )
  (let ((rtn (first (all-group-matches-as-string +return-value+ rtn))))
    (cond
      ;; 
      ((string-upcase? rtn) (keywordize rtn))
      ((string-equal rtn "*yytext") nil)
      )))

(defun read-flex-file (file &aux
		       ci?
		       (defs (make-hash-table :test 'equalp))
		       done-with-defs?)
  
  (iter (for line in-file file using #'adwutils:dos-safe-read-line)
	(setf line (adwutils:trim-and-nullify line))
	(unless line (next-iteration))
	(for parts = (mapcar #'adwutils:trim-whitespace
			     (cl-ppcre:split "\\t+" line :limit 2)))
	(when (and (not ci?)
		   (not done-with-defs?)
		   (cl-ppcre:all-matches +option-case-insensitive+ line))
	  (setf ci? T)
	  (next-iteration))
	(when (and (not done-with-defs?)
		   (cl-ppcre:all-matches +end-of-defs+ line))
	  (setf done-with-defs? T)
	  (next-iteration))
	(cond
	  (done-with-defs? ;; new expansion
	   (destructuring-bind (regex return) parts
	     (setf regex (handle-quoted-rules
			  (replace-expansions defs regex)))
;;	     (handler-case (cl-ppcre:parse-string regex) (error () (error "Error creating production: ~A ~A" regex return)))
	     (collect (list (cl-ppcre:create-scanner regex :case-insensitive-mode ci?)
			    (expand-return-value return))
	       into productions))
	   )
	  (t ;;new def
	   (destructuring-bind (name regex) parts
	     (when (gethash name defs)
	       (error "~A already defined" name))
	     (setf (gethash name defs) (replace-expansions defs regex))
	     (handler-case (cl-ppcre:parse-string (gethash name defs))
	       (error () (error "Error creating def: ~A ~A" name (gethash name defs)))))
	   ))
	(finally (return (values productions ci? defs)))
	))



(defun make-flex-lexer (inp file)
  (let ((start 0) )
    (multiple-value-bind (productions ci?) (read-flex-file file)
      (declare (ignore ci?))
      (lambda ()
	(iter
	  (while (< start (length inp)))
	  (for (regex rtn) in (copy-list productions))
	  (multiple-value-bind (match-start match-end)
	      (cl-ppcre:scan regex inp :start start)
	    (when match-start
	      (setf start match-end)
	      (let ((match (subseq inp match-start match-end)))
		(if rtn
		    (return (values rtn match))
		    (return (values (keywordize match) nil)))))
	    )
	  )))))

(defun make-css3-lexer (inp)
  (make-flex-lexer inp "css-selectors/src/css3.lex"))

(defun lex-results (&optional (inp "#foo, .foo #bar, .foo > bar[src~=blech]"))
  (setf inp (adwutils:trim-whitespace inp))
  (iter  (with lex = (make-css3-lexer inp))
	 (for i = (multiple-value-list (funcall lex)))
	 (while (first i))
	 (collect i into col) (count i into cnt)
	 (finally (return (values col cnt)))))

(defun but-quotes (s) (subseq s 1 (- (length s) 1)))
(defun but-last (s) (subseq s 0 (- (length s) 1)))
(defun but-first (s) (subseq s 1))

(yacc:define-parser *css3-selector-parser*
  (:start-symbol multi-selector)
  (:terminals (:|,| :|*| :|)| :|(| :|>| :|+| :|~| :|:| :|[| :|]| :|=|
		:S :IDENT :HASH :CLASS :STRING :FUNCTION
		:INCLUDES :DASHMATCH :BEGINS-WITH :ENDS-WITH :SUBSTRING ))
  (:precedence )

  (multi-selector
   (selector spaces :|,| spaces multi-selector
	     (lambda (&rest args)
	       (let* ((others (nth 4 args))
		      (others (if (eql :selectors (first others))
				  (cdr others)
				  (list others))))
		 `(:selectors ,(nth 0 args)
			      ,@others))))
   (selector #'identity))
  
  (selector
   (selector spaces combinator spaces simple-selector
    (lambda (&rest args) `(:selector (,(nth 2 args)
				  ,(nth 0 args)
				  (:selector ,(nth 4 args))
				  ))))
   (selector spaces simple-selector
    (lambda (sel spaces simp)
      (if spaces
	  `(:selector ,@(rest sel) ,simp)
	  ;; no space this needs to be an :and node 
	  (let ((others (butlast (cdr sel)))
		(anded (first (last sel))))
	  `(:selector ,@others (:and ,anded ,simp))))))
   (simple-selector
    (lambda (it) `(:selector ,it))))
  
  (simple-selector
   (:HASH (lambda ( o ) (list :hash (but-first o))))
   (:CLASS (lambda ( o ) (list :class (but-first o))))
   (:IDENT (lambda ( it ) (list :element it)))
   (:|*| (constantly :everything))
   (attrib #'identity)
   (pseudo #'identity))

  (combinator (:|+| (constantly :immediatly-preceded-by))
	      (:|~| (constantly :preceded-by))
	      (:|>| (constantly :immediate-child)))

  (attrib
   (:|[| spaces :IDENT spaces :|]|
     (lambda (&rest args) `(:attribute ,(third args))))
   
   (:|[| spaces :IDENT spaces attrib-value-def spaces :|]|
     (lambda (&rest args)
       `(:attribute ,(nth 2 args) ,(nth 4 args))))
   )

  (attrib-value-def
   (:|=| spaces attrib-value
     (lambda (&rest args) (list :equals (nth 2 args))))
   (:INCLUDES spaces attrib-value
	      (lambda (&rest args) (list :includes (nth 2 args))))
   (:DASHMATCH spaces attrib-value
	       (lambda (&rest args) (list :dashmatch (nth 2 args))))
   (:BEGINS-WITH spaces attrib-value
		 (lambda (&rest args) (list :begins-with (nth 2 args))))
   (:ENDS-WITH spaces attrib-value
	       (lambda (&rest args) (list :ends-with (nth 2 args))))

   (:SUBSTRING spaces attrib-value
	       (lambda (&rest args) (list :substring (nth 2 args)))))

  (attrib-value
   (:IDENT (lambda (&rest args) (nth 0 args)))
   (:STRING (lambda (&rest args) (but-quotes (nth 0 args)))))
  
  (pseudo
   (:|:| :IDENT
     (lambda (_ ident) (declare (ignore _))
       (list :pseudo ident)))
   (:|:| :FUNCTION multi-selector :|)|
     (lambda (&rest args)
       (list :pseudo (but-last (nth 1 args)) (nth 2 args)))))
  
  (spaces
   (:S )
   ( )))

(defun parse-results (&optional (inp "#foo, .foo #bar .bast,   .foo > bar[src~=blech],  .foo:hover"))
  (setf inp (adwutils:trim-whitespace inp))
  (yacc:parse-with-lexer (make-css3-lexer inp) *css3-selector-parser*))

(defun transform-parse-tree (pt)
  (case (first pt)
    (:selectors
       `(or ,@(iter (for clause in pt)
		    (collect (transform-parse-tree clause)))))
    (:selector)
    (:pseudo)
    (:attribute)
    (:preceded-by)
    (:immediate-child)
    (:immediatly-preceded-by)))

#|
(defun parse-tree-to-matcher-lambda (pt)
  (lambda (node)
    
    ))

|#