(in-package :css)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:disable-sql-reader-syntax)

;;;; COMMON UTILS COPIED SO AS NOT TO DEPEND ON my utils lib

(defparameter +common-white-space-trimbag+
  '(#\space #\newline #\return #\tab #\no-break_space))

(defun trim-whitespace (s)
  (string-trim +common-white-space-trimbag+ s))

(defun trim-and-nullify (s)
  "trims the whitespace from a string returning nil
   if trimming produces an empty string or the string 'nil' "
  (when s
    (let ((s (trim-whitespace s)))
      (cond ((zerop (length s)) nil)
	    ((string-equal s "nil") nil)
	    (T s)))))

(defun replace-all (string part replacement &key (test #'char=) stream)
  "Returns a new string in which all the occurences of the part 
is replaced with replacement. [FROM http://cl-cookbook.sourceforge.net/strings.html#manip]"
  (let ((out (or stream (make-string-output-stream))))
    (loop with part-length = (length part)
	  for old-pos = 0 then (+ pos part-length)
	  for pos = (search part string
			    :start2 old-pos
			    :test test)
	  do (write-string string out
		   :start old-pos
		   :end (or pos (length string)))
	  when pos do (write-string replacement out)
	    while pos)
    (unless stream
      (get-output-stream-string out))))

(defun symbolize-string (str &optional (package *package*))
  "Turns a string into a happy symbol 
   ex: ''foo bar_bast'' -> FOO-BAR-BAST

   * can also 'change' package of symbol
   ex: :foo -> adw::foo
  "
  (intern
    (etypecase str
      (string (nsubstitute
	       #\- #\_
	       (nsubstitute #\- #\space (string-upcase str) :test #'char=)
	       :test #'char=))
      (symbol (symbol-name str)))
    package))

(defun dos-safe-read-line (stream &optional (eof-error-p t) eof-value recursive-p)
  "readline that can read unix or dos lines"
  (let ((line (read-line stream eof-error-p eof-value recursive-p)))
    (if (stringp line)
	(delete #\return line)
	line)))

;;;;; END COMMON UTILS


(defparameter +option+ "%option\\s")
(defparameter +option-case-insensitive+ "%option\\scase-insensitive")
(defparameter +end-of-defs+ "^%%")
(defparameter +return-value+ "\\s*{\\s*return\\s*([^;]*)\\s*;\\s*}\\s*")

(defun replace-expansions (defs new-regex)
  (iter (for (k v) in-hashtable defs)
	(setf new-regex
	      (replace-all
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
  (symbolize-string s))

(defun keywordize (s)
  (symbolize-string s :keyword))

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
  
  (iter (for line in-file file using #'dos-safe-read-line)
	(setf line (trim-and-nullify line))
	(unless line (next-iteration))
	(for parts = (mapcar #'trim-whitespace
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
  (setf inp (trim-whitespace inp))
  (iter  (with lex = (make-css3-lexer inp))
	 (for i = (multiple-value-list (funcall lex)))
	 (while (first i))
	 (collect i into col) (count i into cnt)
	 (finally (return (values col cnt)))))



(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
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
	  (finally (return new)))))

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defmacro lam (args &body body)
    (let ((arg-names (arg-list-var-names args)))
      `#'(lambda ,args (declare (ignorable ,@arg-names)) ,@body)))

  (defun %rule (args body)
    `(,@args (lam ,(uniquify-and-reintern args) ,@body)))

  (defmacro rule (args &body body)
    `(quote ,(%rule args body)))

  (defun %rule-set (name rules)
    (iter
      (for (args &rest body) in rules)
      (if (first-iteration-p) (collect name))
      (collect (%rule args body))))

  (defmacro rule-set (name &body rules)
    `(quote ,(%rule-set name rules))))



(yacc:define-parser *css3-selector-parser*
  (:start-symbol selector)
  (:terminals (:|,| :|*| :|)| :|(| :|>| :|+| :|~| :|:| :|[| :|]| :|=| :|-|
		:S :IDENT :HASH :CLASS :STRING :FUNCTION :NTH-FUNCTION
		:INCLUDES :DASHMATCH :BEGINS-WITH :ENDS-WITH :SUBSTRING
		:integer))
  (:precedence ((:left :|)| :s :|,| :|+| :|~| )) )
  
  (selector #.(rule (or-sel) or-sel))

  (or-sel
   #.(rule (comb-sel :|,| spaces or-sel)
       (list :or comb-sel or-sel))
   #.(rule (comb-sel) comb-sel))
  
  (comb-sel
   #.(rule (and-sel combinator comb-sel)
       (list combinator and-sel comb-sel))
   #.(rule
	 ;; need to handle trailing spaces here
	 ;; to avoid s/r
	 (and-sel spaces) and-sel))

  (combinator 
   (:s (constantly :child))
   (spaces :|>| spaces (constantly :immediate-child))
   (spaces :|~| spaces (constantly :preceded-by))
   (spaces :|+| spaces (constantly :immediatly-preceded-by)))
  
  (and-sel
   #.(rule (and-sel simple-selector)
       (list :and and-sel simple-selector))
   #.(rule (simple-selector) simple-selector))
  
  (simple-selector
   #.(rule (:HASH) `(:hash ,(but-first hash)))
   #.(rule (:CLASS) `(:class ,(but-first class)))
   #.(rule (:IDENT) `(:element ,ident))
   (:|*| (constantly :everything))
   (attrib #'identity)
   (pseudo #'identity))

  (attrib
   #.(rule (:|[| spaces :IDENT spaces :|]|)
       `(:attribute ,ident))
   
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
   
   #.(rule (:|:| :FUNCTION spaces selector :|)|)
       (list :pseudo (but-last function) selector))
   #.(rule (:|:| :NTH-FUNCTION spaces nth-expr spaces :|)| )
       `(:nth-pseudo ,(but-last nth-function)
		     ,@nth-expr)))

  (nth-expr
   #.(rule (:ident)
       (cond ((string-equal ident "even") (list 2 0))
	     ((string-equal ident "odd") (list 2 1))
	     (T (error "invalid nth subexpression"))))
   
   #.(rule (nth-sign :integer)
       (list 0 (if (string-equal nth-sign "-")
		   (* -1 (parse-integer integer))
		   (parse-integer integer))))
   
   #.(rule (nth-sign :integer :ident)
       (let (extra-num)
	 (cond
	   ((string-equal "n" ident) T)
	   ;; this is because our lexer will recogince n-1 as a valid ident
	   ;; but n+1 will hit the rule below
	   ((alexandria:starts-with-subseq "n" ident)
	    (setf extra-num (parse-integer (subseq ident 1))))
	   (T (error "invalid nth subexpression in (what is ~A)" ident)))
	 (list (or (if (string-equal nth-sign "-")
		       (* -1 (parse-integer integer))
		       (parse-integer integer))
		   0)
	       (or extra-num 0))))
   #.(rule (nth-sign :integer :ident nth-sign :integer)
       (when (and integer-1 (null nth-sign-1))
	 (error "invalid nth subexpression 2n+1 style requires a sign before the second number"))
       (list (or (if (string-equal nth-sign-0 "-")
		     (* -1 (parse-integer integer-0))
		     (parse-integer integer-0))
		 0)
	     (or (if (string-equal nth-sign-1 "-")
		     (* -1 (parse-integer integer-1))
		     (parse-integer integer-1))
		 0))
       ))
  
   (nth-sign
    #.(rule (:|+|) :|+|)
    #.(rule (:|-|) :|-|)
    #.(rule () ()))
  
   (spaces
    (:S )
    ( )))

(defun parse-results (&optional (inp "#foo, .foo #bar .bast,   .foo > bar[src~=blech],  .foo:hover"))
  (setf inp (trim-whitespace inp))
  (yacc:parse-with-lexer (make-css3-lexer inp) *css3-selector-parser*))

