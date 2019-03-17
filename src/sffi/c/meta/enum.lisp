(cl:in-package :claw.sffi.c)


(defclass c-enum (c-type)
  ((values :initarg :values :initform nil)))


(defmethod parse-c-type ((entity claw.spec:foreign-enum) spec)
  (let* ((id (entity-type->c entity))
         (name (unless (emptyp (claw.spec:foreign-entity-name entity))
                 (default-c-name-to-lisp (claw.spec:foreign-entity-name entity))))
         (values (loop for (key . value) in (claw.spec:foreign-enum-values entity)
                       collect (cons (default-c-name-to-lisp key) value))))
    (register-c-type (make-instance 'c-enum :id id
                                            :name name
                                            :values values))
    id))


(defmethod generate-binding ((type c-enum) &key name)
  (with-slots (values) type
    (if-let ((actual-name (or name (name-of type))))
      `((cffi:defcenum ,actual-name
          ,@(loop for (key . value) in values
                  collect (list (make-keyword key) value))))
      `(,@(loop for (key . value) in values
                collect `(define-constant ,(symbolicate '+ key '+) ,value))))))
