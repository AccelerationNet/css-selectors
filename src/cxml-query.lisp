(in-package :net.acceleration.buildnode)
(cl-interpol:enable-interpol-syntax)

(setf cl-ppcre:*allow-named-registers* T)




(defun all-group-matches-as-string (regex target)
  "Expects a single matching group"
  (let (results)
    (cl-ppcre:do-register-groups (grp) (regex target (reverse results))
      (push grp results))
    (nreverse results)))

(defun tokens-from (selector)
  (cl-ppcre:all-matches-as-strings +tokenizer+ selector))

;;(defun css3-lexer (selector) (let ((token-stream  )) #'(lambda () )))

;; (yacc:define-grammar css3 )