(in-package :css)

(defmethod tag-name ((elt html5-parser::node))
  (html5-parser:node-name elt))

(defmethod element-p ((elt html5-parser::element))
  elt)

(defmethod get-attribute ((elt html5-parser::element) attrib)
  (html5-parser:element-attribute elt attrib))

(defmethod parent-node ((elt html5-parser::node))
  (html5-parser:node-parent elt))

(defmethod previous-sibling ((elt html5-parser::node))
  (html5-parser:node-previous-sibling elt))

(defmethod child-elements ((elt html5-parser::node))
  (let ((children '()))
    (html5-parser:element-map-children
     (lambda (child)
       (when (element-p child)
         (push child children)))
     elt)
    (nreverse children)))

(defmethod child-nodes ((elt html5-parser::node))
  (let ((children '()))
    (html5-parser:element-map-children
     (lambda (child)
       (push child children))
     elt)
    (nreverse children)))

(defmethod document-of ((elt html5-parser::node))
  (do ((node elt (html5-parser:node-parent node)))
      ((typep node '(or html5-parser::document html5-parser::document-fragment))
       node)))

(defmethod %do-query (matcher (elt html5-parser::node)
                           &key (first? nil))
  (let ((matches '()))
    (labels ((rec (elt)
               (html5-parser:element-map-children
                (lambda (child)
                  (when (element-p child)
                    (when (funcall matcher child)
                      (if first?
                          (return-from %do-query child)
                          (push child matches)))
                    (rec child)))
                elt)))
      (rec elt))
    (nreverse matches)))
