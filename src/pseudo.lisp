(in-package :css-selectors)

(defun pseudo:not (node &optional sub-sel-function)
  (unless sub-sel-function
    (error "Has pseudo selector requires a sub-selector argument"))
  (cl:not (funcall sub-sel-function node)))

(defun pseudo:is (node &optional sub-sel-function)
  (unless sub-sel-function
    (error "Has pseudo selector requires a sub-selector argument"))
  (funcall sub-sel-function node))

(defun pseudo:has (node &optional sub-sel-function)
  (unless sub-sel-function
    (error "Has pseudo selector requires a sub-selector argument"))
  (iter (for kid in (child-elements node))
    (thereis (%do-query sub-sel-function kid :first? t))))

(defun pseudo:root (node &optional sub-sel-function)
  (when sub-sel-function
    (error "root pseudo selector doesnt support sub-selection arguments"))
  (and (null (parent-element node))
       (find node (child-elements (document-of node)))))

(defun pseudo:first-child (node &optional sub-sel-function)
  (when sub-sel-function
    (error "first-child pseudo selector doesnt support sub-selection arguments"))
  (and (parent-element node)
       (eql node (elt (child-nodes (parent-element node)) 0))))

(defun pseudo:nth-child (node mul add)
  ;; css uses one based indexes http://www.w3.org/TR/css3-selectors/#nth-child-pseudo
  (let ((pos (+ 1 (position node (child-nodes (parent-element node))))))
    (eql (if (and mul (not (zerop mul)))
	     (mod pos mul)
	     pos)
	 (if (plusp add)
	     add
	     (+ mul add)))))

(defun pseudo:nth-last-child (node mul add)
  ;; css uses one based indexes http://www.w3.org/TR/css3-selectors/#nth-child-pseudo
  (let* ((kids (child-nodes (parent-element node)))
	 (pos (- (length kids) (position node kids))))
    (eql (if (and mul (not (zerop mul)))
	     (mod pos mul)
	     pos)
	 (if (plusp add)
	     add
	     (+ mul add)))))

(defun pseudo:last-child (node &optional sub-sel-function)
  (when sub-sel-function
    (error "last-child pseudo selector doesnt support sub-selection arguments"))
  (and (parent-element node)
       (let ((kids (child-nodes (parent-element node))))
	 (eql node (elt kids (- (length kids) 1))))))

(defun pseudo:only-child (node &optional sub-sel-function)
  (when sub-sel-function
    (error "only-child pseudo selector doesnt support sub-selection arguments"))
  (and (parent-node node)
       (let ((kids (child-nodes (parent-node node))))
	 (and (= (length kids) 1)
              (eql node (elt kids 0))))))


