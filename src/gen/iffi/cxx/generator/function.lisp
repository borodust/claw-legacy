(cl:in-package :claw.iffi.cxx)


(defgeneric adapt-type (entity)
  (:method (entity)
    (values entity nil)))


(defmethod adapt-type ((this claw.spec:foreign-record))
  (values (make-instance 'claw.spec:foreign-pointer :enveloped this) t))


(defmethod adapt-type ((this claw.spec:foreign-reference))
  (values (make-instance 'claw.spec:foreign-pointer
                         :enveloped (claw.spec:foreign-enveloped-entity this))
          t))


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
      (values (make-instance 'claw.spec:foreign-pointer :enveloped this) t)
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
                        :value (format nil (if adapted
                                               "*~A"
                                               "~A")
                                       param-name)))))


(defun format-adapted-body (entity adapted-params result-type result-type-adapted stream)
  (let* ((name (claw.spec:foreign-entity-name entity))
         (param-names (mapcar (lambda (param)
                                (getf param :value))
                              adapted-params))
         (invocation (if (typep entity 'claw.spec:foreign-method)
                         (if (claw.spec:foreign-constructor-p entity)
                             (format nil "new (__claw_this_) ~A(~{~A~^, ~})"
                                     (claw.spec:format-full-foreign-entity-name entity)
                                     param-names)
                             (format nil "__claw_this_->~A(~{~A~^, ~})"
                                     name
                                     param-names))
                         (format nil "~A(~{~A~^, ~})"
                                 (claw.spec:format-full-foreign-entity-name entity)
                                 param-names))))
    (format stream "~&// ~A~%" (claw.spec:format-foreign-location
                                (claw.spec:foreign-entity-location entity)))
    (cond
      ((and
        (typep result-type 'claw.spec:foreign-primitive)
        (string= (claw.spec:foreign-entity-name result-type) "void"))
       (format stream "~A;" invocation))
      (result-type-adapted
       (format stream "*(__claw_result_) = ~A;~%return __claw_result_;" invocation))
      (t (format stream "return ~A;" invocation)))))



(defun unconst-adapted-result-type (adapted-result-type)
  (let ((result-type (claw.spec:foreign-enveloped-entity adapted-result-type)))
    (if (typep result-type 'claw.spec:foreign-const-qualifier)
        (make-instance 'claw.spec:foreign-pointer
                       :enveloped (claw.spec:foreign-enveloped-entity result-type))
        adapted-result-type)))


(defun adapt-function (entity)
  (let* ((adapted-params (adapt-parameters entity)))
    (multiple-value-bind (result-type result-type-adapted)
        (adapt-type (claw.spec:foreign-function-result-type entity))
      (let* ((body (with-output-to-string (body)
                     (format-adapted-body entity
                                          adapted-params
                                          result-type
                                          result-type-adapted
                                          body)))
             (params (mapcar (lambda (param)
                               (getf param :adapted))
                             adapted-params))
             (params (if (typep entity 'claw.spec:foreign-method)
                         (list* (make-instance
                                 'claw.spec:foreign-parameter
                                 :name "__claw_this_"
                                 :enveloped (make-instance 'claw.spec:foreign-pointer
                                                           :enveloped (claw.spec:foreign-owner entity)))
                                params)
                         params))
             (params (if result-type-adapted
                         (list* (make-instance
                                 'claw.spec:foreign-parameter
                                 :name "__claw_result_"
                                 :enveloped (unconst-adapted-result-type result-type))
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
                   :name (format nil "~A_claw_ptrextr" mangled-name)
                   :parameters nil
                   :result-type result
                   :body (format nil "return &~A;" full-name))))


(defun generate-function-binding (entity)
  (let* ((adapted-function (adapt-function entity))
         (full-name (claw.spec:format-full-foreign-entity-name entity :include-method-owner nil))
         (name (c-name->lisp (remove-template-argument-string full-name) :type))
         (result-type (entity->cffi-type
                       (check-entity-known
                        (adapted-function-result-type adapted-function))))
         (params (loop for param in (adapted-function-parameters adapted-function)
                       for name = (c-name->lisp (claw.spec:foreign-entity-name param)
                                                :parameter)
                       for enveloped = (check-entity-known
                                        (claw.spec:foreign-enveloped-entity param))
                       collect `(,name ,(entity->cffi-type enveloped))))

         (adapted-cname (register-adapted-function adapted-function))
         (extractor-cname (when (function-pointer-extractor-required-p full-name)
                            (register-adapted-function (adapt-pointer-extractor entity)))))
    (export-symbol name)
    `((claw.iffi:defifun (,adapted-cname ,name ,@(when extractor-cname
                                                   `(:pointer-extractor ,extractor-cname)))
          ,result-type
        ,(claw.spec:format-foreign-location (claw.spec:foreign-entity-location entity))
        ,@params
        ,@(when (claw.spec:foreign-function-variadic-p entity)
            (list 'cl:&rest))))))

;;;
;;; FUNCTION
;;;
(defmethod generate-binding ((generator iffi-generator)
                             (entity claw.spec:foreign-function) &key)
  (unless (claw.spec:foreign-entity-parameters entity)
    (generate-function-binding entity)))

;;;
;;; METHOD
;;;
(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-method) &key)
  (unless (or (claw.spec:foreign-entity-parameters entity)
              (claw.spec:foreign-entity-parameters (claw.spec:foreign-owner entity))
              (and (claw.spec:foreign-record-abstract-p (claw.spec:foreign-owner entity))
                   (claw.spec:foreign-constructor-p entity))
              (starts-with-subseq "operator type-parameter" (claw.spec:foreign-entity-name entity)))
    (generate-function-binding entity)))
