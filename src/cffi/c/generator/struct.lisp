(cl:in-package :claw.cffi.c)

;;;
;;; RECORD
;;;
(defun generate-c-field (record-kind field)
  (let* ((name (c-name->lisp (claw.spec:foreign-entity-name field) :field))
         (byte-offset (/ (claw.spec:foreign-record-field-bit-offset field) 8))
         (offset-param `(:offset ,byte-offset)))
    (export-symbol name)
    (destructuring-bind (kind &optional actual-type count)
        (ensure-list (claw.spec:foreign-entity-type field))
      (append
       (if (and (eq kind :array) (numberp count))
           `(,name ,(entity-typespec->cffi actual-type) :count ,count)
           `(,name ,(entity-type->cffi field)))
       (when (eq record-kind :struct)
         offset-param)))))


(defun generate-c-fields (kind entity)
  (flet ((%generate-c-field (field)
           (generate-c-field kind field)))
    (mapcar #'%generate-c-field (claw.spec:foreign-record-fields entity))))


(defun generate-record-binding (kind entity name fields)
  (let* ((id (entity-type->cffi entity))
         (name (or name
                   (if (emptyp (claw.spec:foreign-entity-name entity))
                       (second id)
                       (c-name->lisp (claw.spec:foreign-entity-name entity) :type))))
         (byte-size (/ (claw.spec:foreign-entity-bit-size entity) 8)))
    (export-symbol name)
    `((,kind (,name :size ,byte-size) ,@fields))))


(defun %generate-forward-declaration (kind name)
  `(,kind ,(c-name->lisp name :type)))

;;;
;;; STRUCT
;;;
(defmethod generate-binding ((entity claw.spec:foreign-struct) &key name)
  (generate-record-binding 'cffi:defcstruct entity name
                           (generate-c-fields :struct entity)))


(defmethod generate-forward-declaration ((entity claw.spec:foreign-struct) &key name)
  (first (generate-record-binding 'cffi:defcstruct entity name nil)))


(defmethod generate-forward-declaration-from-typespec ((kind (eql :struct))
                                                       &optional name &rest opts)
  (declare (ignore opts))
  (%generate-forward-declaration 'cffi:defcstruct name))


;;;
;;; UNION
;;;
(defmethod generate-binding ((entity claw.spec:foreign-union) &key name)
  (generate-record-binding 'cffi:defcunion entity name
                           (generate-c-fields :union entity)))


(defmethod generate-forward-declaration ((entity claw.spec:foreign-union) &key name)
  (first (generate-record-binding 'cffi:defcunion entity name nil)))


(defmethod generate-forward-declaration-from-typespec ((kind (eql :union))
                                                       &optional name &rest opts)
  (declare (ignore opts))
  (%generate-forward-declaration 'cffi:defcunion name))
