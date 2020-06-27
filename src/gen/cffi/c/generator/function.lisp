(cl:in-package :claw.cffi.c)


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-function))
  (list* (claw.spec:foreign-function-return-type entity)
         (mapcar #'claw.spec:foreign-enveloped-entity (claw.spec:foreign-function-parameters entity))))
