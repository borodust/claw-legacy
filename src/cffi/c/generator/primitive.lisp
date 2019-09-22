(cl:in-package :claw.cffi.c)


(defmethod generate-binding ((type claw.spec:foreign-primitive) &key)
  (switch ((claw.spec:foreign-entity-type type) :test #'string=)
    ("long double" (let ((size (ceiling (/ (claw.spec:foreign-entity-bit-size type)
                                           +byte-size+)))
                         (name (c-name->lisp 'long-double :type)))
                     (export-symbol name)
                     `((cffi:defcstruct (,name :size ,size)
                         (data :unsigned-char :count ,size))
                       (cffi:defctype ,name (:struct ,name)))))))
