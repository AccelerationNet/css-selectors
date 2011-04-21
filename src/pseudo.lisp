(in-package :css-selectors.pseudo)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:disable-sql-reader-syntax)

(export '(has is root first-child last-child only-child) :css-selectors.pseudo)

(defun pseudo:has (node &optional sub-sel-function)
  (unless sub-sel-function
    (error "Has pseudo selector requires a sub-selector argument"))
  (iter (for kid in-dom-children node)
	(iter (for n in-dom kid)
	      (when (and
		     (typep n 'dom:element)
		     (funcall sub-sel-function n))
		(return-from pseudo:has T)))))

(defun pseudo:is (node &optional sub-sel-function)
  (unless sub-sel-function
    (error "Has pseudo selector requires a sub-selector argument"))
  (funcall sub-sel-function node))

(defun pseudo:root (node &optional sub-sel-function)
  (when sub-sel-function
    (error "root pseudo selector doesnt support sub-selection arguments"))
  (and (null (dom:parent-node node))
       (find node (dom:child-nodes (dom:owner-document node)))))

(defun pseudo:first-child (node &optional sub-sel-function)
  (when sub-sel-function
    (error "first-child pseudo selector doesnt support sub-selection arguments"))
  (and (dom:parent-node node)
       (eql node (elt (dom:child-nodes (dom:parent-node node)) 0))))

(defun pseudo:last-child (node &optional sub-sel-function)
  (when sub-sel-function
    (error "last-child pseudo selector doesnt support sub-selection arguments"))
  (and (dom:parent-node node)
       (let ((kids (dom:child-nodes (dom:parent-node node))))
	 (eql node (elt kids (- (length kids) 1))))))

(defun pseudo:only-child (node &optional sub-sel-function)
  (when sub-sel-function
    (error "only-child pseudo selector doesnt support sub-selection arguments"))
  (and (dom:parent-node node)
       (let ((kids (dom:child-nodes (dom:parent-node node))))
	 (eql node (elt kids (- (length kids) 1))))))


