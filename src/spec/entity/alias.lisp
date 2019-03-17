(cl:in-package :claw.spec)


;;;
;;;
;;;
(defclass foreign-alias (foreign-entity)
  ((aliased-type :initarg :aliased :initform (error ":aliased missing")
                 :reader foreign-alias-type)))


(defun find-base-alias-type (alias &optional (spec *library-specification*))
  (labels ((unwrap-alias (alias)
             (if (typep alias 'foreign-alias)
                 (unwrap-alias (find-foreign-entity
                                (extract-entity-type (foreign-alias-type alias))
                                spec))
                 alias)))
    (foreign-entity-type (unwrap-alias alias))))


(defmethod parse-form (form (tag (eql :typedef)))
  (alist-bind (name location type) form
    (foreign-entity-type
     (register-foreign-entity name
                              (make-instance 'foreign-alias
                                             :name name
                                             :type name
                                             :aliased (parse-form type (aval :tag type))
                                             :location location)))))


(defmethod compose-form ((this foreign-alias))
  (alist :tag "typedef"
         :name (foreign-entity-name this)
         :location (foreign-entity-location this)
         :type (compose-reference (foreign-alias-type this))))


(defmethod compose-entity-reference ((this foreign-alias))
  (alist :tag (foreign-entity-name this)))
