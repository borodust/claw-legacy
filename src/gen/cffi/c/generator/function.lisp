(cl:in-package :claw.cffi.c)


(defgeneric recordp (entity)
  (:method (entity)
    (cond
      ((claw.spec:foreign-envelope-p entity) (recordp (claw.spec:foreign-enveloped-entity entity)))
      ((emulated-primitive-p entity) t)
      (t nil))))


(defmethod recordp ((this claw.spec:foreign-record))
  (declare (ignore this))
  t)


(defmethod recordp ((this claw.spec:foreign-pointer))
  (declare (ignore this))
  nil)


(defun adaptablep (function)
  (some #'recordp (list* (claw.spec:foreign-function-result-type function)
                         (mapcar #'claw.spec:foreign-enveloped-entity
                                 (claw.spec:foreign-function-parameters function)))))


(defun returns-struct-p (function)
  (recordp (claw.spec:foreign-function-result-type function)))


(defun adapt-record-type (entity)
  (if (and (adapter) (recordp entity))
      (make-instance 'claw.spec:foreign-pointer :enveloped entity)
      entity))


(defun adapt-result-type (function)
  (adapt-record-type (claw.spec:foreign-function-result-type function)))


(defun generate-cffi-parameters (params)
  (loop for param in params
        for param-idx from 0
        for enveloped = (claw.spec:foreign-enveloped-entity param)
        for adapted = (entity->cffi-type enveloped)
        for name = (c-name->lisp (or (claw.spec:foreign-entity-name param)
                                     (format nil "~A~A" 'arg param-idx))
                                 :parameter)
        collect `(,name ,adapted)))


(defclass adapted-function ()
  ((name :initarg :name :reader adapted-function-name)
   (params :initarg :parameters :reader adapted-function-parameters)
   (result-type :initarg :result-type :reader adapted-function-result-type)
   (body :initarg :body :reader adapted-function-body)
   (entity :initarg :entity :reader adapted-function-entity)))


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


(defun adapt-parameters (entity)
  (flet ((%make-parameter (name type)
           (make-instance 'claw.spec:foreign-parameter
                          :name name
                          :enveloped type)))
    (loop for param in (claw.spec:foreign-function-parameters entity)
          for enveloped = (claw.spec:foreign-enveloped-entity param)
          for adapted = (%make-parameter (claw.spec:foreign-entity-name param)
                                         (adapt-record-type enveloped))
          collect adapted into adapted-parameters
          finally (return (append (when (and (adapter) (returns-struct-p entity))
                                    (list (%make-parameter "__claw_result"
                                                           (adapt-result-type entity))))
                                  adapted-parameters)))))


(defun adapt-function (entity)
  (make-instance 'adapted-function
                 :name (claw.spec:foreign-entity-name entity)
                 :parameters (adapt-parameters entity)
                 :result-type (adapt-result-type entity)
                 :body ""
                 :entity entity))


(defmethod generate-binding ((generator cffi-generator) (entity claw.spec:foreign-function) &key)
  (let* ((id (entity->cffi-type entity))
         (adapted (adapt-function entity))
         (c-name (if (and (adapter) (adaptablep entity))
                     (register-adapted-function adapted)
                     (adapted-function-name adapted))))
    (export-symbol id)
    `((cffi:defcfun (,c-name ,id) ,(entity->cffi-type (adapted-function-result-type adapted))
        ,@(generate-cffi-parameters (adapted-function-parameters adapted))
        ,@(when (claw.spec:foreign-function-variadic-p entity)
            (list 'cl:&rest))))))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-function))
  (list* (claw.spec:foreign-function-result-type entity)
         (mapcar #'claw.spec:foreign-enveloped-entity (claw.spec:foreign-function-parameters entity))))
