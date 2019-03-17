(cl:in-package :claw.spec)

;;;
;;;
;;;
(defmethod parse-form (form (tag (eql :pointer)))
  (alist-bind (type) form
    (alist-bind ((type-tag :tag)) type
      `(:pointer ,(parse-form type type-tag)))))


(defmethod compose-type-reference ((type-group (eql :pointer)) type &rest type-args)
  (declare (ignore type-args))
  (alist :tag ":pointer"
         :type (compose-reference type)))
