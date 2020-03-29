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

;;;
;;; RESECT
;;;
(defmethod parse-type ((category (eql :array)) kind type)
  (declare (ignore category kind))
  `(:array ,(parse-type-by-category (%resect:array-element-type type))
           ,@(let ((size (%resect:array-size type)))
               (when (>= size 0)
                 (list size)))))
