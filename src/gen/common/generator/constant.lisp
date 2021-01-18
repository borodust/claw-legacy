(cl:in-package :claw.generator.common)


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-constant) &key)
  (let* ((name (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :constant))
         (value (claw.spec:foreign-entity-value entity)))
    (expand-constant name value)))


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-variable) &key)
  (let* ((name (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :variable))
         (type (check-entity-known (claw.spec:foreing-variable-type entity)))
         (value (claw.spec:foreign-entity-value entity))
         (decorated-name (format-symbol (symbol-package name) "*~A*" name)))
    (export-symbol decorated-name)
    (if (claw.spec:foreing-variable-external-p entity)
        `((define-symbol-macro ,decorated-name
              ,(if (typep type 'claw.spec:foreign-array)
                   `(cffi:foreign-symbol-pointer ,(claw.spec:foreign-entity-name entity))
                   (let ((ptr (format-symbol (symbol-package decorated-name) "~A" 'ptr)))
                     `(let ((,ptr (cffi:foreign-symbol-pointer ,(claw.spec:foreign-entity-name entity))))
                        (when ,ptr
                          (cffi:mem-ref ,ptr ',(entity->cffi-type type))))))))
        `((defparameter ,decorated-name ,value)))))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-variable))
  (find-foreign-dependencies (claw.spec:foreing-variable-type entity)))
