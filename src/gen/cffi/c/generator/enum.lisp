(cl:in-package :claw.cffi.c)


(defmethod dependablep ((entity claw.spec:foreign-enum))
  (declare (ignore entity))
  t)
