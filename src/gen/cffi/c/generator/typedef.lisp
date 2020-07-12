(cl:in-package :claw.cffi.c)


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-alias))
  (list (claw.spec:foreign-enveloped-entity entity)))


(defmethod dependablep ((entity claw.spec:foreign-alias))
  (declare (ignore entity))
  t)
