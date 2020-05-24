(cl:in-package :claw.iffi.cxx)


(defclass genetaror () ())


(defmethod claw.wrapper:expand-library-definition ((generator (eql :claw/iffi))
                                                   (language (eql :c++))
                                                   wrapper
                                                   configuration)
  (explode-library-definition (make-instance 'generator) language wrapper configuration))
