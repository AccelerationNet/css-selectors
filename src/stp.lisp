(in-package :css)

(defmethod tag-name ((elt stp:element))
  (stp:local-name elt))

(defmethod get-attribute ((elt stp:element) attr)
  ;; TODO: special case namespace-uri
  (stp:attribute-value elt attr))

(defmethod element-p ((elt stp:element))
  elt)

(defmethod parent-node ((n stp:node))
  "gets the parent node"
  (stp:parent-node n))

(defmethod previous-sibling ((n stp:element))
  "gets the parent dom:element (rather than ever returning the document node)"
  (element-p (stp:previous-sibling n)))

(defmethod child-elements ((n stp:node))
  (iter (for kid in (stp:list-children n))
    (when (element-p kid) (collect kid))))

(defmethod child-nodes ((n stp:node))
  (stp:list-children n))

(defmethod document-of ((n stp:node))
  (stp:document n))

(defmethod %do-query (matcher (elt stp:element) &key (first? nil))
  (stp:filter-children
   (lambda (node)
     (let ((res (and (element-p node)
                     (or (cl:not matcher);; TODO: why would this be nil
                         (funcall matcher node)))))
       (if (and first? res)
           (return-from %do-query node)
           res)))
   elt))

