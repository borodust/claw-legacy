(cl:in-package :claw.cffi.c)


(defmethod generate-binding ((entity claw.spec:foreign-alias) &key)
  (let* ((id (entity-type->cffi entity))
         (aliased-type (entity-typespec->cffi (claw.spec:foreign-alias-type entity))))
    (export-symbol id)
    `((cffi:defctype ,id ,aliased-type))))
