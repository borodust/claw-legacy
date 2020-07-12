(cl:in-package :claw.resect)


(defun optimize-specification (spec
                               include-definitions
                               include-sources
                               exclude-definitions
                               exclude-sources)
  (let* ((*library-specification* spec)
         (*inclusion-table* (make-inclusion-table *library-specification*
                                                  include-definitions
                                                  include-sources
                                                  exclude-definitions
                                                  exclude-sources))
         (optimized-spec (make-instance 'library-specification)))
    (do-foreign-entities (entity *library-specification*)
      (when-let ((optimized-entity (optimize-entity entity)))
        (let ((entity-type (foreign-entity-type optimized-entity)))
          (register-foreign-entity entity-type optimized-entity optimized-spec))))
    optimized-spec))
