(cl:in-package :claw.spec)


(defvar *parsed-pointer-types* nil)

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

;;;
;;;
;;;
(defmethod parse-type ((category (eql :pointer)) kind type)
  (declare (ignorable category kind))
  (if (member (cffi:pointer-address type) *parsed-pointer-types* :test #'=)
      '(:pointer "void")
      (let* ((*parsed-pointer-types* (push (cffi:pointer-address type) *parsed-pointer-types*))
             (pointee-type (parse-type-by-category (%resect:pointer-pointee-type type))))
        `(:pointer ,pointee-type))))
