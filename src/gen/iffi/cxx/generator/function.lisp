(cl:in-package :claw.iffi.cxx)


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
                   :namespace (claw.spec:foreign-entity-namespace entity)
                   :parameters nil
                   :result-type result
                   :body (format nil "return &~A;" full-name))))


(defun check-function-entity-known (entity)
  (check-entity-known entity)
  (when (and (not (or (typep entity 'claw.spec:foreign-primitive)
                      (claw.spec:foreign-entity-forward-p entity)))
             (claw.spec:foreign-aligned-p entity)
             (= (claw.spec:foreign-entity-bit-size entity) 0))
    (signal-unknown-entity entity))
  (when (claw.spec:foreign-envelope-p entity)
    (check-function-entity-known (claw.spec:foreign-enveloped-entity entity))))


(defun generate-function-binding (entity)
  (check-function-entity-known (claw.spec:foreign-function-result-type entity))
  (loop for param in (claw.spec:foreign-function-parameters entity)
        for unqualified = (claw.spec:unwrap-foreign-entity param)
        when (claw.spec:foreign-entity-private-p unqualified)
          do (signal-unknown-entity unqualified)
        do (check-function-entity-known param))

  (let* ((adapted-function (adapt-function entity :mode :c++))
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
    `((iffi:defifun (,adapted-cname ,name
                                    ,@(when extractor-cname
                                        `(:pointer-extractor ,extractor-cname))
                                    ,@(when (and (typep entity 'claw.spec:foreign-method)
                                                 (claw.spec:foreign-method-const-p entity))
                                        `(:non-mutating t))
                                    ,@(unless *inline-functions*
                                        `(:inline nil)))
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


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-method))
  (append (list (claw.spec:foreign-owner entity)) (call-next-method)))
