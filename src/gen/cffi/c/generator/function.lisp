(cl:in-package :claw.cffi.c)


(defun generate-cffi-parameters (params)
  (loop for param in params
        for param-idx from 0
        for enveloped = (claw.spec:foreign-enveloped-entity param)
        for adapted = (entity->cffi-type enveloped)
        for name = (c-name->lisp (or (claw.spec:foreign-entity-name param)
                                     (format nil "~A~A" 'arg param-idx))
                                 :parameter)
        collect `(,name ,adapted)))


(defmethod generate-binding ((generator cffi-generator) (entity claw.spec:foreign-function) &key)
  (multiple-value-bind (adapted adapted-p)
      (adapt-function entity)
    (let* ((id (entity->cffi-type entity))
           (c-name (if (and (adapter)
                            (or adapted-p
                                (claw.spec:foreign-function-inlined-p entity)))
                       (register-adapted-function adapted)
                       (claw.spec:foreign-entity-name entity))))
      (export-symbol id)
      `(,@(when *inline-functions*
            `((declaim (inline ,id))))
        (cffi:defcfun (,c-name ,id) ,(entity->cffi-type (adapted-function-result-type adapted))
          ,@(generate-cffi-parameters (adapted-function-parameters adapted))
          ,@(when (claw.spec:foreign-function-variadic-p entity)
              (list 'cl:&rest)))))))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-function))
  (list* (claw.spec:foreign-function-result-type entity)
         (mapcar #'claw.spec:foreign-enveloped-entity (claw.spec:foreign-function-parameters entity))))


(defmethod generate-binding ((generator cffi-generator) (entity claw.spec:foreign-method) &key)
  nil)
