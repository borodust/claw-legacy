(cl:in-package :claw.generator.common)


(defun recordp (entity)
  (let ((canonical (find-canonical-type entity)))
    (or (emulated-primitive-p canonical)
        (typep canonical 'claw.spec:foreign-record))))


(defun adaptablep (function)
  (some #'recordp (list* (claw.spec:foreign-function-result-type function)
                         (mapcar #'claw.spec:foreign-enveloped-entity
                                 (claw.spec:foreign-function-parameters function)))))


(defun returns-struct-p (function)
  (recordp (claw.spec:foreign-function-result-type function)))


(defun adapt-record-type (entity)
  (entity->cffi-type (if (and (adapter) (recordp entity))
                         (make-instance 'claw.spec:foreign-pointer :enveloped entity)
                         entity)))


(defun adapt-result-type (function)
  (adapt-record-type (claw.spec:foreign-function-result-type function)))


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


#++(defmethod adapted-function-name ((this claw.spec:foreign-function) &optional stream)
  (format stream "~A" (claw.spec:foreign-entity-name this)))


#++(defun format-function-declaration (control-string function &key name
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
                               into params
                           else
                             collect type-name
                               into params
                           finally (return
                                     (if (claw.spec:foreign-function-variadic-p function)
                                         (append params (list "..."))
                                         params))))
         (param-string (format nil "~{~A~^, ~}" parameters)))
    (format stream control-string
            (typespec->c (claw.spec:foreign-function-result-type function))
            (or name (claw.spec:foreign-entity-name function))
            param-string)))


#++(defmethod adapted-function-original-type ((this claw.spec:foreign-function)
                                           name &optional stream)
  (format-function-declaration "~A (*~A)(~A)" this :name name :stream stream))


#++(defmethod adapted-function-definition ((this claw.spec:foreign-function)
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
                                         (claw.spec:foreign-function-result-type this))
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
    (let* ((result-type (%adapted-typespec->c
                         (claw.spec:foreign-function-result-type this)))
           (void-p (equal result-type "void")))
      (format stream "~A ~A(~A) {~%  " result-type adapted-name (%to-params))
      (unless void-p
        (format stream "~A result = " (typespec->c
                                       (claw.spec:foreign-function-result-type this))))
      (format stream "~A(~A);" original-name (%to-args))
      (when (returns-struct-p this)
        (format stream "~& (*arg0) = result;"))
      (unless void-p
        (format stream "~&  return ~A;" (if (returns-struct-p this)
                                            "arg0"
                                            "result")))
      (format stream "~&}"))))


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-function) &key)
  (let* ((id (entity->cffi-type entity))
         (adaptable-p (adaptablep entity))
         (c-name (if adaptable-p
                     (adapt-function-name (claw.spec:foreign-entity-name entity))
                     (claw.spec:foreign-entity-name entity)))
         (result-type (adapt-result-type entity))
         (params (generate-c-parameters entity)))
    (when (and (adapter) adaptable-p)
      (register-adapted-function entity))
    (export-symbol id)
    `((cffi:defcfun (,c-name ,id) ,result-type
        ,@params
        ,@(when (claw.spec:foreign-function-variadic-p entity)
            (list 'cl:&rest))))))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-function))
  (list* (claw.spec:foreign-function-result-type entity)
         (mapcar #'claw.spec:foreign-enveloped-entity (claw.spec:foreign-function-parameters entity))))
