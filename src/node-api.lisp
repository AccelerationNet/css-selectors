(in-package :css)

(defgeneric tag-name (elt)
  (:documentation "retrieves the name of the given element"))

(defgeneric get-attribute (node attrib)
  (:documentation "retrieves the value of a given attribute of the node"))

(defgeneric parent-node (node)
  (:documentation "returns the parent node of this node"))

(defgeneric parent-element (node)
  (:documentation "returns the parent element of this node")
  (:method (n) (element-p (parent-node n))))

(defgeneric previous-sibling (node)
  (:documentation "returns the parent element of this node"))

(defgeneric element-p (node)
  (:documentation "Is this an element")
  (:method (elt) nil))

(defgeneric parent-elements (node)
  (:documentation "Returns all parent elements of this node")
  (:method (node)
    (iter
      (with n = node)
      (setf n (parent-element n))
      (while n)
      (collect n))))

(defgeneric child-elements (node)
  (:documentation "Returns all child elements of this node"))

(defgeneric child-nodes (node)
  (:documentation "Returns all child nodes of this node"))

(defgeneric document-of (node)
  (:documentation "Returns the document of the given node"))