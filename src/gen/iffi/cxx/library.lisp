(cl:in-package :claw.iffi.cxx)


(defclass iffi-generator (generator) ())


(defmethod claw.wrapper:generate-bindings ((generator (eql :claw/iffi))
                                           (language (eql :c++))
                                           wrapper
                                           configuration)
  (list*
   (let* ((size-t-entity (loop for entity in (claw.wrapper:wrapper-entities wrapper)
                                 thereis (and (string= "size_t" (claw.spec:foreign-entity-name entity))
                                              entity)))
          (size-t-type (case (and size-t-entity (claw.spec:foreign-entity-bit-size size-t-entity))
                         (64 :int64)
                         (32 :int32)
                         (16 :int16)
                         (8 :int8)
                         (t :int64))))
     `(iffi:initialize :size-t-type ,size-t-type))
   (explode-library-definition (make-instance 'iffi-generator) language wrapper configuration)))
