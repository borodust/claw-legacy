(cl:in-package :claw.generator.common)


(define-constant +emulated-primitives+ '("long double"
                                         "int128"
                                         "uint128"
                                         "float128")
  :test 'equal)


(defun generate-primitive-byte-holder (type)
  (let ((size (ceiling (/ (claw.spec:foreign-entity-bit-size type)
                          +byte-size+)))
        (name (entity->cffi-type type)))
    (export-symbol name)
    `((cffi:defcstruct (,name :size ,size)
        (data :unsigned-char :count ,size))
      (defmethod cffi:foreign-type-alignment ((this ,name))
        (declare (ignore this))
        ,(/ (claw.spec:foreign-entity-bit-alignment type) +byte-size+))
      (cffi:defctype ,name (:struct ,name)))))


(defun emulated-primitive-p (entity)
  (and (typep entity 'claw.spec:named)
       (member (claw.spec:foreign-entity-name entity) +emulated-primitives+ :test #'string=)))


(defmethod generate-binding ((generator generator) (type claw.spec:foreign-primitive) &key)
  (when (emulated-primitive-p type)
    (generate-primitive-byte-holder type)))


(defmethod dependable-p ((entity claw.spec:foreign-primitive))
  (declare (ignore entity))
  t)
