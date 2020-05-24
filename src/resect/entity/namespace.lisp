(cl:in-package :claw.spec)


(defmethod parse-form (form (tag (eql :namespace)))
  #++(warn "Skipping namespace ~A" form))
