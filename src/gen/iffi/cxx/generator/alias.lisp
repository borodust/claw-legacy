(cl:in-package :claw.iffi.cxx)


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-alias) &key)
  (let* ((id (entity->cffi-type entity))
         (aliased-type (entity->cffi-type (check-entity-known
                                           (claw.spec:foreign-enveloped-entity entity)))))
    (export-symbol id)
    `((iffi:defitype ,id ,aliased-type))))
