(cl:in-package :claw.generator.common)


(define-constant +emulated-primitives+ '("long double"
                                         "int128"
                                         "uint128"
                                         "float128"
                                         "nullptr")
  :test 'equal)


(defun generate-primitive-byte-holder (type)
  (let* ((size (ceiling (/ (claw.spec:foreign-entity-bit-size type)
                           +byte-size+)))
         (name (entity->cffi-type type))
         (emulated-name (format-symbol (symbol-package name) "~A~A" 'emulated$ name)))
    (export-symbol name)
    `((cffi:defcstruct (,name :class ,emulated-name :size ,size)
        (data :unsigned-char :count ,size))
      (defmethod cffi:foreign-type-alignment ((this ,emulated-name))
        (declare (ignore this))
        ,(/ (claw.spec:foreign-entity-bit-alignment type) +byte-size+))
      (cffi:defctype ,name (:struct ,name)))))


(defun emulated-primitive-p (entity)
  (and (claw.spec:foreign-named-p entity)
       (member (claw.spec:foreign-entity-name entity) +emulated-primitives+ :test #'string=)))


(defmethod generate-binding ((generator generator)
                             (type claw.spec:foreign-primitive)
                             &key)
  (switch ((claw.spec:foreign-entity-name type) :test #'string=)
    ("wchar_t" (let ((name (entity->cffi-type type)))
                 `((cffi:defctype ,name ,(eswitch ((claw.spec:foreign-entity-bit-size type)
                                                   :test #'=)
                                           (8 :char)
                                           (16 :uint16)
                                           (32 :uint32)
                                           (64 :uint64))))))
    (t (when (emulated-primitive-p type)
         (generate-primitive-byte-holder type)))))


(defmethod dependablep ((entity claw.spec:foreign-primitive))
  (declare (ignore entity))
  t)
