(cl:in-package :claw.iffi.cxx)


(defmethod dependablep ((this claw.spec:foreign-entity-parameter))
  (declare (ignore this))
  nil)
