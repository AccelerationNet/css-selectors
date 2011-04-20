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

(defun is-special? (s)
  (when (or (symbolp s) (stringp s))
    (char-equal #\& (elt (string s) 0))))

(defun arg-list-var-names (args)
  (iter (for i in args)
	(unless (is-special? i)
	  (typecase i
	    (symbol (collect i))
	    (list (collect (car i)))))))

(defun uniquify-and-reintern (x &optional (package *package*))
  (iter (for (sym . rest) on x)
	(for count = (iter (for s in new)
			   (when (alexandria:starts-with-subseq
				  (symbol-name sym) (symbol-name s)
				  :test #'char-equal)
			     (count s))))
	(if (or (find sym rest) (plusp count))
	    (collect (intern #?"${sym}-${count}" package) into new)
	    (collect (intern (symbol-name sym) package) into new))
	(finally (return new))))

(defmacro lam (args &body body)
  (let ((arg-names (arg-list-var-names args)))
    `#'(lambda ,args (declare (ignorable ,@arg-names)) ,@body)))

(defmacro rule (args &body body)
  `(quote (,@args (lam ,(uniquify-and-reintern args) ,@body))))


(yacc:define-parser *css3-selector-parser*
  (:start-symbol multi-selector)
  (:terminals (:|,| :|*| :|)| :|(| :|>| :|+| :|~| :|:| :|[| :|]| :|=|
		:S :IDENT :HASH :CLASS :STRING :FUNCTION
		:INCLUDES :DASHMATCH :BEGINS-WITH :ENDS-WITH :SUBSTRING ))
  (:precedence )

  (multi-selector
   (selector #'identity)
   #.(rule (selector spaces :|,| spaces multi-selector)
       (let* ((others (if (eql :selectors (first multi-selector))
			  (cdr multi-selector)
			  (list multi-selector))))
	 `(:selectors ,selector ,@others))))
  
  (selector
   #.(rule (simple-selector)
       `(:selector ,simple-selector))
   
   #.(rule (selector spaces combinator spaces simple-selector)
       `(:selector (,combinator ,selector (:selector ,simple-selector))))
   
   #.(rule (selector spaces simple-selector)       
       (if spaces
	   `(:selector ,@(rest selector) ,simple-selector)
	   ;; no space this needs to be an :and node 
	   (let ((others (butlast (cdr selector)))
		 (anded (first (last selector))))
	     `(:selector ,@others (:and ,anded ,simple-selector)))))
   )
  
  (combinator (:|+| (constantly :immediatly-preceded-by))
	      (:|~| (constantly :preceded-by))
	      (:|>| (constantly :immediate-child)))

  (simple-selector
   #.(rule (:HASH) `(:hash ,(but-first hash)))
   #.(rule (:CLASS) `(:class ,(but-first class)))
   #.(rule (:IDENT) `(:element ,ident))
   (:|*| (constantly :everything))
   (attrib #'identity)
   (pseudo #'identity))

  (attrib
   #.(rule (:|[| spaces :IDENT spaces :|]|)
       `(:attribute ,(third ident)))
   
   #.(rule (:|[| spaces :IDENT spaces attrib-value-def spaces :|]|)
       `(:attribute ,ident ,attrib-value-def))
   )

  (attrib-value-def
   #.(rule (attrib-match-type attrib-value)
       (list attrib-match-type attrib-value)))

  (attrib-match-type
   #.(rule (:|=|) :equals)
   #.(rule (:includes) :includes)
   #.(rule (:dashmatch) :dashmatch)
   #.(rule (:begins-with) :begins-with)
   #.(rule (:ends-with) :ends-with)
   #.(rule (:substring) :substring))

  (attrib-value
   #.(rule (:ident) ident)
   #.(rule (:string) (but-quotes string)))
  
  (pseudo
   #.(rule (:|:| :IDENT) (list :pseudo ident))
   
   ;; trailing spaces was causing s/r conflict - still straightening it out
   #.(rule (:|:| :FUNCTION spaces multi-selector ;spaces
	     :|)|)
       (list :pseudo (but-last function) multi-selector)))
  
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