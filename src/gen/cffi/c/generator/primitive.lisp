(cl:in-package :claw.cffi.c)


(defun generate-primitive-byte-holder (type)
  (let ((size (ceiling (/ (claw.spec:foreign-entity-bit-size type)
                          +byte-size+)))
        (name (entity-type->cffi type)))
    (export-symbol name)
    `((cffi:defcstruct (,name :size ,size)
        (data :unsigned-char :count ,size))
      (cffi:defctype ,name (:struct ,name)))))


(defmethod generate-binding ((type claw.spec:foreign-primitive) &key)
  (switch ((claw.spec:foreign-entity-type type) :test #'string=)
    ("long double" (generate-primitive-byte-holder type))
    ("int128" (generate-primitive-byte-holder type))
    ("uint128" (generate-primitive-byte-holder type))))
