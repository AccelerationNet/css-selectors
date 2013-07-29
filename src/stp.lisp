(in-package :css)

(defmethod tag-name ((elt stp:element))
  (stp:local-name elt))

(defmethod get-attribute ((elt stp:element) attr)
  ;; TODO: special case namespace-uri
  (stp:attribute-value elt (string-downcase attr)))

(defmethod element-p ((elt stp:element))
  elt)

(defmethod parent-node ((n stp:node))
  "gets the parent node"
  (stp:parent n))

(defmethod previous-sibling ((n stp:element))
  "gets the parent dom:element (rather than ever returning the document node)"
  (unless (eq n (stp:first-child (stp:parent n)))
    (element-p (stp:previous-sibling n))))

(defmethod child-elements ((n stp:node))
  (iter (for kid in (stp:list-children n))
    (when (element-p kid) (collect kid))))

(defmethod child-nodes ((n stp:node))
  (stp:list-children n))

(defmethod document-of ((n stp:node))
  (stp:document n))

(defun stp-do-query (matcher elt first?)
  (if first?
      (stp:find-recursively-if
       (lambda (node)
         (when (and (element-p node) (funcall matcher node))
           node))
       elt)
      (let ((matches '()))
        (stp:filter-recursively
         (lambda (node)
           (when (and (element-p node) (funcall matcher node))
             (push node matches)))
         elt)
        (nreverse matches))))

(defmethod %do-query (matcher (elt stp:element) &key (first? nil))
  (stp-do-query matcher elt first?))

(defmethod %do-query (matcher (elt stp:document) &key (first? nil))
  (stp-do-query matcher elt first?))
