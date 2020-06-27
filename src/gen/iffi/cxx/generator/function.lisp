(cl:in-package :claw.iffi.cxx)


(defun recordp (entity)
  (let ((canonical (find-canonical-type entity)))
    (or (emulated-primitive-p canonical)
        (typep canonical 'claw.spec:foreign-record))))


(defun adaptablep (function)
  (some #'recordp (list* (claw.spec:foreign-function-return-type function)
                         (mapcar #'claw.spec:foreign-enveloped-entity
                                 (claw.spec:foreign-function-parameters function)))))


(defun returns-struct-p (function)
  (recordp (claw.spec:foreign-function-return-type function)))


(defun adapt-record-type (entity)
  (entity->cffi-type (if (and (adapter) (recordp entity))
                         (make-instance 'claw.spec:foreign-pointer :enveloped entity)
                         entity)))


(defun adapt-return-type (function)
  (adapt-record-type (claw.spec:foreign-function-return-type function)))


(defun generate-c-parameters (function)
  (loop for param in (claw.spec:foreign-function-parameters function)
        for param-idx from 0
        for enveloped = (claw.spec:foreign-enveloped-entity param)
        collect (let* ((id (adapt-record-type enveloped))
                       (name (c-name->lisp (or (claw.spec:foreign-entity-name param)
                                               (format nil "~A~A" 'arg param-idx))
                                           :parameter)))
                  `(,name ,id))
          into adapted-parameters
        finally (return (append (when (and (adapter) (returns-struct-p function))
                                  `(,(c-name->lisp "result" :parameter)
                                    ,(adapt-record-type function)))
                                adapted-parameters))))


(defun adapt-function-name (function-name)
  (if (adapter)
      (format nil "~A~A" +adapted-function-prefix+ function-name)
      function-name))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-method) &key)
  (let* ((id (entity->cffi-type entity))
         (adaptable-p (adaptablep entity))
         (c-name (if adaptable-p
                     (adapt-function-name (claw.spec:foreign-entity-name entity))
                     (claw.spec:foreign-entity-name entity)))
         (return-type (adapt-return-type entity))
         (params (generate-c-parameters entity)))
    (when (and (adapter) adaptable-p)
      (register-adapted-function entity))
    (export-symbol id)
    `((cffi:defcfun (,c-name ,id) ,return-type
        ,@params
        ,@(when (claw.spec:foreign-function-variadic-p entity)
            (list 'cl:&rest))))))
