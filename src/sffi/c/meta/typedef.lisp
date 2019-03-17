(cl:in-package :claw.sffi.c)


(defclass c-typedef (c-type)
  ((aliased-type :initarg :aliased-type
                 :initform (error ":aliased-type missing")
                 :reader aliased-type-of)))


(defun parse-aliased-c-type (aliased-type spec)
  (if (listp aliased-type)
      (case (first aliased-type)
        ((or :pointer :array) (list (first aliased-type)
                                    (parse-aliased-c-type (second aliased-type) spec)))
        (t (ensure-c-type (claw.spec:find-foreign-entity aliased-type spec) spec)))
      (ensure-c-type (claw.spec:find-foreign-entity aliased-type spec) spec)))


(defmethod parse-c-type ((entity claw.spec:foreign-alias) spec)
  (let* ((id (entity-type->c entity))
         (aliased-type (parse-aliased-c-type (claw.spec:foreign-alias-type entity) spec)))
    (register-c-type (make-instance 'c-typedef :id id
                                               :name id
                                               :aliased-type aliased-type))
    id))


(defmethod generate-binding ((type c-typedef) &key)
  (let ((base-type (find-c-type (aliased-type-of type))))
    (if (and base-type (anonymous-p base-type))
        (generate-binding base-type :name (id-of type))
        `((cffi:defctype ,(id-of type)
              ,(find-native-cffi-type (if base-type
                                          (id-of base-type)
                                          (aliased-type-of type))))))))
