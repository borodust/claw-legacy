(cl:in-package :claw.iffi.cxx)


(defgeneric adapt-type (entity)
  (:method (entity)
    (if (emulated-primitive-p entity)
        (values (make-instance 'claw.spec:foreign-pointer :enveloped entity) entity)
        (values entity nil))))


(defmethod adapt-type ((this claw.spec:foreign-record))
  (values (make-instance 'claw.spec:foreign-pointer :enveloped this) this))


(defmethod adapt-type ((this claw.spec:foreign-reference))
  (values (make-instance 'claw.spec:foreign-pointer
                         :enveloped (claw.spec:foreign-enveloped-entity this))
          this))

(defmethod adapt-type ((this claw.spec:foreign-const-qualifier))
  (multiple-value-bind (type adapted)
      (adapt-type (claw.spec:foreign-enveloped-entity this))
    (values (make-instance 'claw.spec:foreign-const-qualifier :enveloped type) adapted)))


(defgeneric alias-adaptable-p (entity)
  (:method (entity)
    (second (multiple-value-list (adapt-type entity)))))


(defmethod alias-adaptable-p ((this claw.spec:foreign-alias))
  (alias-adaptable-p (claw.spec:foreign-enveloped-entity this)))


(defmethod adapt-type ((this claw.spec:foreign-alias))
  (if (alias-adaptable-p this)
      (values (make-instance 'claw.spec:foreign-pointer :enveloped this) this)
      (values this nil)))


(defclass adapted-function ()
  ((name :initarg :name :reader adapted-function-name)
   (params :initarg :parameters :reader adapted-function-parameters)
   (result-type :initarg :result-type :reader adapted-function-result-type)
   (body :initarg :body :reader adapted-function-body)))


(defmethod print-object ((o adapted-function) s)
  (with-slots (name params result-type body) o
    (print-unreadable-object (o s :type t :identity nil)
      (let ((params (mapcar #'claw.spec:foreign-enveloped-entity params)))
        (format s "~A ~A ~A ~S" name result-type params body)))))


(defun adapt-parameters (entity)
  (loop for param in (claw.spec:foreign-function-parameters entity)
        for count from 0
        for param-name = (if (emptyp (claw.spec:foreign-entity-name param))
                             (format nil "arg~A" count)
                             (claw.spec:foreign-entity-name param))
        for param-location = (claw.spec:foreign-entity-location param)
        collect (multiple-value-bind (type adapted)
                    (adapt-type (claw.spec:foreign-enveloped-entity param))
                  (list :adapted (make-instance
                                  'claw.spec:foreign-parameter
                                  :name param-name
                                  :enveloped type
                                  :location param-location)
                        :name (c-name->lisp param-name :parameter)
                        :value (cond
                                 ((and (typep adapted 'claw.spec:foreign-reference)
                                       (claw.spec:foreign-reference-rvalue-p adapted))
                                  (format nil "std::move(*~A)" param-name))
                                 (adapted (format nil "*~A" param-name))
                                 (t param-name))))))


(defun unconst-adapted-result-type (adapted-result-type)
  (cond
    ((typep adapted-result-type 'claw.spec:foreign-const-qualifier)
     (unconst-adapted-result-type (claw.spec:foreign-enveloped-entity adapted-result-type)))
    ((typep adapted-result-type 'claw.spec:foreign-pointer)
     (make-instance 'claw.spec:foreign-pointer
                    :enveloped (unconst-adapted-result-type
                                (claw.spec:foreign-enveloped-entity adapted-result-type))))
    (t adapted-result-type)))


(defun decorate-if-instantiated-function (entity name)
  (format nil "~A~@[<~{~A~^,~}>~]"
          name
          (loop for arg in (claw.spec:foreign-entity-arguments entity)
                for param = (claw.spec:foreign-entity-parameter arg)
                for value = (claw.spec:foreign-entity-value arg)
                collect (cond
                          ((typep param 'claw.spec:foreign-entity-value-parameter)
                           (format nil "static_cast<~A>(~A)"
                                   (claw.spec:format-full-foreign-entity-name
                                    (claw.spec:foreign-entity-parameter-type param))
                                   value))
                          ((claw.spec:foreign-named-p value)
                           (claw.spec:format-full-foreign-entity-name value))
                          ((eq t value) "true")
                          ((eq nil value) "false")
                          (t value)))))


(defun format-adapted-body (entity adapted-params result-type result-type-adapted-from stream)
  (let* ((name (claw.spec:foreign-entity-name entity))
         (param-names (mapcar (lambda (param)
                                (getf param :value))
                              adapted-params))
         (invocation (if (typep entity 'claw.spec:foreign-method)
                         (cond
                           ((claw.spec:foreign-constructor-p entity)
                            (format nil "new (__claw_this_) ~A(~{~A~^, ~})"
                                    (claw.spec:format-full-foreign-entity-name entity)
                                    param-names))
                           ((claw.spec:foreign-method-static-p entity)
                            (format nil "~A(~{~A~^, ~})"
                                    (claw.spec:format-full-foreign-entity-name entity)
                                    param-names))
                           (t
                            (format nil "__claw_this_->~A(~{~A~^, ~})"
                                    name
                                    param-names)))
                         (format nil "~A(~{~A~^, ~})"
                                 (decorate-if-instantiated-function
                                  entity
                                  (claw.spec:format-full-foreign-entity-name entity))
                                 param-names))))
    (format stream "~&// ~A~%" (claw.spec:format-foreign-location
                                (claw.spec:foreign-entity-location entity)))
    (cond
      ((and
        (typep result-type 'claw.spec:foreign-primitive)
        (string= (claw.spec:foreign-entity-name result-type) "void"))
       (format stream "~A;" invocation))
      (result-type-adapted-from
       (let* ((unconsted-result-type (unconst-adapted-result-type result-type))
              (unconsted-result-type-c-name (claw.spec:format-foreign-entity-c-name
                                             unconsted-result-type)))
         (if (typep result-type-adapted-from 'claw.spec:foreign-reference)
             (format stream "return (~A) ~@[~A~](&~A);"
                     unconsted-result-type-c-name
                     (when (claw.spec:foreign-reference-rvalue-p result-type-adapted-from)
                       "std::move")
                     invocation)
             (format stream "new (__claw_result_) ~A(~A);~%return __claw_result_;"
                     (claw.spec:format-full-foreign-entity-name
                      (claw.spec:foreign-enveloped-entity unconsted-result-type))
                     invocation))))
      (t (format stream "return ~A;" invocation)))))


(defun adapt-function (entity)
  (let* ((adapted-params (adapt-parameters entity)))
    (multiple-value-bind (result-type result-type-adapted-from)
        (adapt-type (claw.spec:foreign-function-result-type entity))
      (let* ((body (with-output-to-string (body)
                     (format-adapted-body entity
                                          adapted-params
                                          result-type
                                          result-type-adapted-from
                                          body)))
             (params (mapcar (lambda (param)
                               (getf param :adapted))
                             adapted-params))
             (params (if (and (typep entity 'claw.spec:foreign-method)
                              (not (claw.spec:foreign-method-static-p entity)))
                         (list* (make-instance
                                 'claw.spec:foreign-parameter
                                 :name "__claw_this_"
                                 :enveloped (make-instance 'claw.spec:foreign-pointer
                                                           :enveloped (claw.spec:foreign-owner entity)))
                                params)
                         params))
             (params (if (and result-type-adapted-from
                              (not (typep result-type-adapted-from 'claw.spec:foreign-reference)))
                         (list* (make-instance
                                 'claw.spec:foreign-parameter
                                 :name "__claw_result_"
                                 :enveloped result-type)
                                params)
                         params)))
        (make-instance 'adapted-function
                       :name (claw.spec:foreign-entity-mangled-name entity)
                       :parameters params
                       :result-type result-type
                       :body body)))))


(defun adapt-pointer-extractor (entity)
  (let* ((full-name (claw.spec:format-full-foreign-entity-name entity :include-method-owner nil))
         (mangled-name (claw.spec:foreign-entity-mangled-name entity))
         (proto (make-instance 'claw.spec:foreign-function-prototype
                               :result-type (claw.spec:foreign-function-result-type entity)
                               :parameters (claw.spec:foreign-function-parameters entity)
                               :variadic (claw.spec:foreign-function-variadic-p entity)
                               :owner (claw.spec:foreign-owner entity)))
         (result (make-instance 'claw.spec:foreign-pointer :enveloped proto)))
    (make-instance 'adapted-function
                   :name (format nil "__claw_ptrextr_~A" mangled-name)
                   :parameters nil
                   :result-type result
                   :body (format nil "return &~A;" full-name))))


(defun generate-function-binding (entity)
  (check-entity-known (claw.spec:foreign-function-result-type entity))
  (loop for param in (claw.spec:foreign-function-parameters entity)
        for unqualified = (claw.spec:unwrap-foreign-entity param)
        when (claw.spec:foreign-entity-private-p unqualified)
          do (signal-unknown-entity unqualified)
        do (check-entity-known param))

  (let* ((adapted-function (adapt-function entity))
         (full-name (claw.spec:format-full-foreign-entity-name entity))
         (name (symbolicate-function-name entity))
         (result-type (entity->cffi-type
                       (adapted-function-result-type adapted-function)))
         (params (loop for param in (adapted-function-parameters adapted-function)
                       for name = (c-name->lisp (claw.spec:foreign-entity-name param)
                                                :parameter)
                       for enveloped = (claw.spec:foreign-enveloped-entity param)
                       collect `(,name ,(entity->cffi-type enveloped))))

         (adapted-cname (register-adapted-function adapted-function))
         (extractor-cname (when (function-pointer-extractor-required-p full-name)
                            (register-adapted-function (adapt-pointer-extractor entity)))))
    (export-symbol name)
    `((iffi:defifun (,adapted-cname ,name ,@(when extractor-cname
                                              `(:pointer-extractor ,extractor-cname)))
          ,result-type
        ,(claw.spec:format-foreign-location (claw.spec:foreign-entity-location entity))
        ,@params
        ,@(when (claw.spec:foreign-function-variadic-p entity)
            (list 'cl:&rest))))))

;;;
;;; FUNCTION
;;;
(defun function-parameterized-p (function)
  (let ((name (claw.spec:foreign-entity-name function)))
    (or (parameterizedp function)
        (parameterizedp (claw.spec:foreign-function-result-type function))
        (loop for param in (claw.spec:foreign-function-parameters function)
                thereis (parameterizedp (claw.spec:foreign-enveloped-entity param)))
        (starts-with-subseq "operator type-parameter" name)
        (starts-with-subseq "operator new" name)
        (starts-with-subseq "operator delete" name))))


(defmethod generate-binding ((generator iffi-generator)
                             (entity claw.spec:foreign-function) &key)
  (unless (function-parameterized-p entity)
    (generate-function-binding entity)))

;;;
;;; METHOD
;;;
(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-method) &key)
  (unless (or (function-parameterized-p entity)
              (parameterizedp (claw.spec:foreign-owner entity))
              (and (claw.spec:foreign-record-abstract-p (claw.spec:foreign-owner entity))
                   (claw.spec:foreign-constructor-p entity)))
    (generate-function-binding entity)))
