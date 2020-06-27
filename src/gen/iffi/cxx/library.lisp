(cl:in-package :claw.iffi.cxx)


(defclass iffi-generator (generator) ())


(defmethod claw.wrapper:generate-bindings ((generator (eql :claw/iffi))
                                           (language (eql :c++))
                                           wrapper
                                           configuration)
  (explode-library-definition (make-instance 'iffi-generator) language wrapper configuration))
