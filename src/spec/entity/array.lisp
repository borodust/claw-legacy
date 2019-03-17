(cl:in-package :claw.spec)

;;;
;;; ARRAY
;;;
(defmethod parse-form (form (tag (eql :array)))
  (alist-bind (type size) form
    `(:array ,(parse-form type (aval :tag type)) ,size)))


(defmethod compose-type-reference ((type-group (eql :array)) type &rest args)
  (destructuring-bind (size) args
    (alist :tag ":array"
           :size size
           :type (compose-reference type))))
