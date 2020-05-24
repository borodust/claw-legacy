(cl:in-package :claw.cffi.c)


(defclass genetaror () ())


(defmethod claw.wrapper:expand-library-definition ((generator (eql :claw/cffi))
                                                   (language (eql :c))
                                                   wrapper
                                                   configuration)
  (explode-library-definition (make-instance 'generator) language wrapper configuration))
