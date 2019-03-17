(cl:in-package :claw.sffi.c)


(defclass c-constant (c-type)
  ((value :initarg :value :initform (error ":value missing") :reader value-of)))


(defmethod parse-c-type ((entity claw.spec:foreign-constant) spec)
  (let* ((id (entity-type->c entity))
         (value (claw.spec:foreign-constant-value entity)))
    (register-c-type (make-instance 'c-constant :id id :value value))
    id))


(defmethod generate-binding ((type c-constant) &key)
  `((define-constant ,(symbolicate '+ (id-of type) '+) ,(value-of type))))
