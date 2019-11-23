(cl:in-package :claw.spec)


(defmethod parse-form (form (tag (eql :class)))
  #++(warn "Skipping class ~A" form))
