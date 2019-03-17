(cl:in-package :claw.cffi.c)


(defmethod generate-binding ((entity claw.spec:foreign-constant) &key)
  (let* ((name (c-name->lisp (claw.spec:foreign-entity-name entity)))
         (value (claw.spec:foreign-constant-value entity)))
    (expand-constant name value)))
