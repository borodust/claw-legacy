(cl:in-package :claw.cffi.c)


(defun recordp (type)
  (when-let ((entity (claw.spec:find-foreign-entity type *spec*)))
    (let ((base-type (if (typep entity 'claw.spec:foreign-alias)
                         (claw.spec:find-base-alias-type entity *spec*)
                         (claw.spec:foreign-entity-type entity))))
      (and (listp base-type) (member (first base-type) '(:struct :union))))))


(defun adaptablep (function)
  (some #'recordp (list* (claw.spec:foreign-function-return-type function)
                         (mapcar #'claw.spec:foreign-entity-type
                                 (claw.spec:foreign-function-parameters function)))))


(defun returns-struct-p (function)
  (recordp (claw.spec:foreign-function-return-type function)))


(defun generate-c-parameters (function)
  (loop for param in (claw.spec:foreign-function-parameters function)
        for param-idx from 0
        collect (let* ((id (adapt-record-type (claw.spec:foreign-entity-type param)))
                       (name (or (c-name->lisp
                                  (claw.spec:foreign-entity-name param))
                                 (c-name->lisp (format nil "~A~A" 'arg param-idx)))))
                  `(,name ,id))
          into adapted-parameters
        finally (return (if (returns-struct-p function)
                            (list* `(,(c-name->lisp "result")
                                     ,(adapt-return-type function))
                                   adapted-parameters)
                            adapted-parameters))))


(defun adapt-function-c-name (c-name)
  (if *adapter*
      (format nil "~A~A" +adapted-function-prefix+ c-name)
      c-name))


(defun adapt-record-type (typespec)
  (entity-typespec->cffi (if (and *adapter* (recordp typespec))
                             `(:pointer ,typespec)
                             typespec)))

(defun adapt-return-type (function)
  (adapt-record-type (claw.spec:foreign-function-return-type function)))


(defmethod adapted-function-name ((this claw.spec:foreign-function) &optional stream)
  (format stream "~A" (claw.spec:foreign-entity-name this)))


(defun format-function-declaration (control-string function &key name
                                                              stream
                                                              collect-names)
  (let* ((parameters (loop for param in (claw.spec:foreign-function-parameters function)
                           for i from 0
                           for type-name = (typespec->c
                                            (claw.spec:foreign-entity-type param))
                           if collect-names
                             collect (format nil "~A ~A"
                                             type-name
                                             (or (claw.spec:foreign-entity-name param)
                                                 (format nil "arg~A" i)))
                           else
                             collect type-name))
         (param-string (format nil "~{~A~^, ~}" parameters)))
    (format stream control-string
            (typespec->c (claw.spec:foreign-function-return-type function))
            (or name (claw.spec:foreign-entity-name function))
            param-string)))


(defmethod adapted-function-original-type ((this claw.spec:foreign-function)
                                           name &optional stream)
  (format-function-declaration "~A (*~A)(~A)" this :name name :stream stream))


(defmethod adapted-function-definition ((this claw.spec:foreign-function)
                                        adapted-name original-name &optional stream)
  (labels ((%adapted-typespec->c (typespec)
             (typespec->c (if (recordp typespec)
                              `(:pointer ,typespec)
                              typespec)))
           (%to-param-types ()
             (loop for param in (claw.spec:foreign-function-parameters this)
                   collect (%adapted-typespec->c (claw.spec:foreign-entity-type param))
                     into param-types
                   finally (return
                             (if (returns-struct-p this)
                                 (list* (%adapted-typespec->c
                                         (claw.spec:foreign-function-return-type this))
                                        param-types)
                                 param-types))))
           (%to-params ()
             (format nil "~{~A~^, ~}"
                     (loop for i from 0
                           for type in (%to-param-types)
                           collect (format nil "~A arg~A" type i))))
           (%to-args ()
             (format nil "~{~A~^, ~}"
                     (loop for i from (if (returns-struct-p this) 1 0)
                           for type in (claw.spec:foreign-function-parameters this)
                           collect (if (recordp (claw.spec:foreign-entity-type type))
                                       (format nil "(*arg~A)" i)
                                       (format nil "arg~A" i))))))
    (let* ((return-type (%adapted-typespec->c
                         (claw.spec:foreign-function-return-type this)))
           (void-p (equal return-type "void")))
      (format stream "~A ~A(~A) {~%  " return-type adapted-name (%to-params))
      (unless void-p
        (format stream "~A result = " (typespec->c
                                       (claw.spec:foreign-function-return-type this))))
      (format stream "~A(~A);" original-name (%to-args))
      (when (returns-struct-p this)
        (format stream "~& (*arg0) = result;"))
      (unless void-p
        (format stream "~&  return ~A;" (if (returns-struct-p this)
                                            "arg0"
                                            "result")))
      (format stream "~&}"))))


(defmethod generate-binding ((entity claw.spec:foreign-function) &key)
  (let* ((id (entity-typespec->cffi (claw.spec:foreign-entity-type entity)))
         (adaptable-p (adaptablep entity))
         (c-name (if adaptable-p
                     (adapt-function-c-name (claw.spec:foreign-entity-name entity))
                     (claw.spec:foreign-entity-name entity)))
         (return-type (adapt-return-type entity))
         (params (generate-c-parameters entity)))
    (when (and *adapter* adaptable-p)
      (register-adapted-function *adapter* entity))
    (export-symbol id)
    `((cffi:defcfun (,c-name ,id) ,return-type
        ,(format-function-declaration "~A ~A(~A);" entity :collect-names t)
        ,@params))))
