(cl:in-package :claw.sffi.c)


(defclass c-parameter (c-type) ())

(defclass c-function (c-type)
  ((c-name :initarg :c-name :initform (error ":c-name missing"))
   (return-type :initarg :return-type :initform (error ":return-type missing"))
   (parameters :initarg :parameters :initform (error ":parameters missing"))))


(defun parse-c-parameters (parameters)
  (loop for param in parameters
        for param-idx from 0
        collect (let* ((id (entity-typespec->c
                            (claw.spec:foreign-entity-type param)))
                       (name (default-c-name-to-lisp
                              (claw.spec:foreign-entity-name param)))
                       (name (or name (format-symbol *package* "~A~A" 'arg param-idx))))
                  (make-instance 'c-parameter :id id :name (or name)))))


(defmethod parse-c-type ((entity claw.spec:foreign-function) spec)
  (let* ((id (entity-typespec->c (claw.spec:foreign-entity-type entity)))
         (c-name (claw.spec:foreign-entity-name entity))
         (return-type (entity-typespec->c
                       (claw.spec:foreign-function-return-type entity)))
         (params (parse-c-parameters (claw.spec:foreign-function-parameters entity))))
    (register-c-type (make-instance 'c-function :id id
                                                :name id
                                                :c-name c-name
                                                :return-type return-type
                                                :parameters params))
    id))


(defmethod generate-binding ((type c-function) &key)
  (with-slots (c-name return-type parameters) type
    `((cffi:defcfun (,c-name ,(name-of type)) ,return-type
        ,@(loop for param in parameters
                collect `(,(name-of param) ,(find-native-cffi-type (id-of param))))))))
