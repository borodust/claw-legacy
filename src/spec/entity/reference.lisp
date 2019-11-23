(cl:in-package :claw.spec)


(defmethod parse-form (form (tag (eql :reference)))
  #++(warn "Skipping reference ~A" form))
