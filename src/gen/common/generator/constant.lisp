(cl:in-package :claw.generator.common)


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-constant) &key)
  (let* ((name (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :constant))
         (value (claw.spec:foreign-constant-value entity)))
    (expand-constant name value)))
