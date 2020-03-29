(cl:in-package :claw.spec)


;;;
;;;
;;;
(defclass foreign-extern (foreign-symbol) ())


(defmethod parse-form (form (tag (eql :extern)))
  (alist-bind (name type location) form
    (let ((extern (make-instance 'foreign-extern
                                 :name name
                                 :location location
                                 :type (parse-form type (aval :tag type)))))
      (register-foreign-entity name extern)
      (foreign-entity-type extern))))


(defmethod compose-form ((this foreign-extern))
  (alist :tag "extern"
         :name (foreign-entity-name this)
         :location (foreign-entity-location this)
         :type (compose-reference (foreign-entity-type this))))
