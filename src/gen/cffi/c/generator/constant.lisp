(cl:in-package :claw.cffi.c)


(defmethod generate-binding ((generator cffi-generator) (entity claw.spec:foreign-constant) &key)
  (let* ((name (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :constant))
         (value (claw.spec:foreign-constant-value entity)))
    (expand-constant name value)))
