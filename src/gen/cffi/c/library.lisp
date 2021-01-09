(cl:in-package :claw.cffi.c)


(defclass cffi-generator (generator) ())


(defmethod list-required-systems ((this cffi-generator))
  '(:cffi))


(defmethod claw.wrapper:generate-bindings ((generator (eql :claw/cffi))
                                           (language (eql :c))
                                           wrapper
                                           configuration)
  (let ((*qualify-records* t))
    (explode-library-definition (make-instance 'cffi-generator) language wrapper configuration)))
