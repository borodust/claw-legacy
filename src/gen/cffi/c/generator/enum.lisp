(cl:in-package :claw.cffi.c)


(defmethod dependable-p ((entity claw.spec:foreign-enum))
  (declare (ignore entity))
  t)
