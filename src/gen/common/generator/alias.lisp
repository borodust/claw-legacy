(cl:in-package :claw.generator.common)


(defun %generate-typedef-binding (entity)
  (let* ((id (entity->cffi-type entity))
         (aliased-type (entity->cffi-type (claw.spec:foreign-enveloped-entity entity))))
    (export-symbol id)
    `((cffi:defctype ,id ,aliased-type))))


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-alias) &key)
  (%generate-typedef-binding entity))


(defmethod generate-forward-declaration ((generator generator)
                                         (entity claw.spec:foreign-alias) &key name)
  (let ((base-type (find-canonical-type entity)))
    (append (when base-type
              (generate-forward-declaration generator base-type :name name))
            (%generate-typedef-binding entity))))
