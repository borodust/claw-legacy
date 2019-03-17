(cl:in-package :claw.sffi.c)


(defclass c-primitive (c-type) ())


(defmethod parse-c-type ((entity claw.spec:foreign-primitive) spec)
  (let* ((id (primitive->c (claw.spec:foreign-entity-type entity))))
    (register-c-type (make-instance 'c-primitive :id id :name id))
    id))


(defmethod generate-binding ((type c-primitive) &key)
  nil)
