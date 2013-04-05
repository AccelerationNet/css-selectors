(in-package :css)

(defmethod tag-name ((elt dom:element))
  (dom:tag-name elt))

(defmethod element-p ((elt dom:element))
  elt)

(defmethod get-attribute ((elt dom:element) attrib)
  ;; TODO: special case namespace-uri
  (dom:get-attribute elt attrib))

(defmethod parent-node ((n dom:node))
  (dom:parent-node n))

(defmethod previous-sibling ((n dom:element))
  "gets the parent dom:element (rather than ever returning the document node)"
  (element-p (dom:previous-sibling n)))

(defmethod child-elements ((n dom:node))
  (iter (for kid in-dom-children n)
    (when (element-p kid) (collect kid))))

(defmethod child-nodes ((n dom:node))
  (iter (for kid in-dom-children n) (collect kid)))

(defmethod document-of ((n dom:node))
  (dom:owner-document n))

(defmethod %do-query (matcher (elt dom:node) &key (first? nil))
  (iter (for n in-dom elt)
    (when (and (element-p n) (funcall matcher n))
      (if first?
          (return n)
          (collect n)))))

