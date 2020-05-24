(cl:in-package :claw.spec)


;;;
;;;
;;;
(defmethod parse-form (form (tag (eql :reference)))
  (alist-bind (type) form
    (alist-bind ((type-tag :tag)) type
      `(:reference ,(parse-form type type-tag)))))


(defmethod compose-type-reference ((type-group (eql :reference)) type &rest type-args)
  (declare (ignore type-args))
  (alist :tag ":reference"
         :type (compose-reference type)))

;;;
;;;
;;;
(defmethod parse-type ((category (eql :reference)) kind type)
  (declare (ignorable category kind))
  `(:reference ,(parse-type-by-category (%resect:reference-pointee-type type))))
