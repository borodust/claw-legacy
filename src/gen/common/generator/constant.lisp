(cl:in-package :claw.generator.common)


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-constant) &key)
  (let* ((name (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :constant))
         (value (claw.spec:foreign-entity-value entity)))
    (expand-constant name value)))


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-variable) &key)
  (let* ((name (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :variable))
         (type (entity->cffi-type (check-entity-known (claw.spec:foreing-variable-type entity))))
         (value (claw.spec:foreign-entity-value entity))
         (decorated-name (if (claw.spec:foreing-variable-external-p entity)
                             (format-symbol (symbol-package name) "*~A*" name)
                             (format-symbol (symbol-package name) "+~A+" name))))
    (export-symbol decorated-name)
    (if (claw.spec:foreing-variable-external-p entity)
        `((define-symbol-macro ,decorated-name
              (cffi:mem-ref
               (cffi:foreign-symbol-pointer ,(claw.spec:foreign-entity-name entity))
               ',type)))
        `((defparameter ,decorated-name ,value)))))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-variable))
  (find-foreign-dependencies (claw.spec:foreing-variable-type entity)))
