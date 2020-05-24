(cl:in-package :claw.cffi.c)


(defun %generate-typedef-binding (entity)
  (let* ((id (entity-type->cffi entity))
         (aliased-type (entity-typespec->cffi (claw.spec:foreign-alias-type entity))))
    (export-symbol id)
    `((cffi:defctype ,id ,aliased-type))))


(defmethod generate-binding ((entity claw.spec:foreign-alias) &key)
  (%generate-typedef-binding entity))


(defmethod generate-forward-declaration ((entity claw.spec:foreign-alias) &key name)
  (let ((base-type (claw.spec:find-base-alias-type entity *spec*)))
    (append (when-let ((dep (claw.spec:find-foreign-entity base-type *spec*)))
              (generate-forward-declaration dep :name name))
            (%generate-typedef-binding entity))))
