(cl:in-package :claw.iffi.cxx)


(defun anonymous-branch-p (entity)
  (or (when (claw.spec:foreign-envelope-p entity)
        (anonymous-branch-p (claw.spec:foreign-enveloped-entity entity)))
      (and (claw.spec:foreign-named-p entity)
           (emptyp (claw.spec:foreign-entity-name entity)))
      (when-let ((owner (claw.spec:foreign-owner entity)))
        (anonymous-branch-p owner))))


(defun mangle-full-record-name (entity)
  (ppcre:regex-replace-all "::|\\W"
                           (claw.spec:format-full-foreign-entity-name entity)
                           "_"))

(defun adapt-reporter (entity reporter)
  (make-instance 'adapted-function
                 :name (format nil "~A_~A" reporter (mangle-full-record-name entity))
                 :parameters nil
                 :result-type (unsigned-long-long)
                 :body (format nil "return ~A(~A);"
                               reporter
                               (claw.spec:format-full-foreign-entity-name entity))))


(defun adapt-setter (record field)
  (let ((field-name (claw.spec:foreign-entity-name field)))
    (multiple-value-bind (field-type adapted-p)
        (adapt-type (claw.spec:foreign-enveloped-entity field))
      (unless (or (typep field-type 'claw.spec:foreign-array)
                  (typep field-type 'claw.spec:foreign-const-qualifier))
        (make-instance 'adapted-function
                       :name (format nil "set_~A_~A"
                                     (mangle-full-record-name record)
                                     field-name)
                       :parameters (list (parameter "__claw_this_" (pointer record))
                                         (parameter "__claw_value_" field-type))
                       :result-type (void)
                       :body (format nil "__claw_this_->~A = ~@[~A~]__claw_value_;"
                                     field-name
                                     (when adapted-p
                                       "*")))))))

(defun adapt-getter (record field)
  (let ((field-name (claw.spec:foreign-entity-name field)))
    (multiple-value-bind (field-type adapted-p)
        (adapt-type (claw.spec:foreign-enveloped-entity field))
      (let* ((array-p (typep field-type 'claw.spec:foreign-array))
             (result-type (if array-p
                              (pointer (claw.spec:foreign-enveloped-entity field-type))
                              field-type)))
        (make-instance 'adapted-function
                       :name (format nil "get_~A_~A"
                                     (mangle-full-record-name record)
                                     field-name)
                       :parameters (list (parameter "__claw_this_" (pointer record)))
                       :result-type result-type
                       :body (format nil "return ~@[(~A)~]~@[~A~]__claw_this_->~A;"
                                     (when array-p
                                       (entity->c-name result-type))
                                     (when adapted-p
                                       "&")
                                     field-name))))))


(defun symbolicate-record-name (entity)
  (let ((formatted (claw.spec:format-full-foreign-entity-name entity)))
    (c-name->lisp (if (emptyp formatted)
                      (claw.spec:foreign-entity-id entity)
                      formatted)
                  :type)))


(defun generate-record-binding (define entity)
  (unless (anonymous-branch-p entity)
    (let ((name (symbolicate-record-name entity))
          sizeof-cname
          alignof-cname)
      (when (and (> (claw.spec:foreign-entity-bit-size entity) 0)
                 (not (anonymous-branch-p entity)))
        (setf sizeof-cname (register-adapted-function (adapt-reporter entity "sizeof"))
              alignof-cname (register-adapted-function (adapt-reporter entity "alignof"))))
      `((,define (,name :size-reporter ,sizeof-cname
                        :alignment-reporter ,alignof-cname)
            ()
          ,(claw.spec:format-foreign-location (claw.spec:foreign-entity-location entity))
          ,@(unless (claw.spec:foreign-entity-parameters entity)
              (loop for field in (claw.spec:foreign-record-fields entity)
                    do (check-entity-known field)
                    unless (anonymous-branch-p (claw.spec:foreign-enveloped-entity field))
                      collect (let* ((adapted-setter (adapt-setter entity field))
                                     (setter-cname (when adapted-setter
                                                     (register-adapted-function adapted-setter)))
                                     (adapted-getter (adapt-getter entity field))
                                     (getter-cname (when adapted-getter
                                                     (register-adapted-function adapted-getter))))
                                `(,(c-name->lisp (claw.spec:foreign-entity-name field) :field)
                                  ,(entity->cffi-type (adapted-function-result-type adapted-getter))
                                  :setter ,setter-cname
                                  :getter ,getter-cname
                                  :documentation ,(claw.spec:format-foreign-location
                                                   (claw.spec:foreign-entity-location field)))))))))))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-class) &key)
  (generate-record-binding 'iffi:deficlass entity))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-struct) &key)
  (generate-record-binding 'iffi:defistruct entity))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-union) &key)
  (generate-record-binding 'iffi:defiunion entity))


(defmethod generate-forward-declaration ((generator iffi-generator)
                                         (entity claw.spec:foreign-record)
                                         &key)
  `((iffi:defirecord ,(symbolicate-record-name entity) ()
      "forward declaration")))
