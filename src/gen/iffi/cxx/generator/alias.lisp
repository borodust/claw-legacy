(cl:in-package :claw.iffi.cxx)


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-alias) &key)
  (let* ((id (entity->cffi-type entity))
         (aliased-type (entity->cffi-type (check-entity-known
                                           (claw.spec:foreign-enveloped-entity entity)))))
    (when-let (owner (claw.spec:foreign-owner entity))
      (check-entity-known owner))
    (export-symbol id)
    (unless (or (claw.spec:foreign-entity-private-p entity)
                ;; next happens with typedefs for records with the same name
                (eq id aliased-type))
      `((iffi:defitype ,id ,aliased-type
          ,(claw.spec:format-foreign-location (claw.spec:foreign-entity-location entity)))))))
