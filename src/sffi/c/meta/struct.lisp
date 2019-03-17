(cl:in-package :claw.sffi.c)

;;;
;;; RECORD
;;;
(defclass c-record (c-type)
  ((fields :initarg :fields)))


(defclass c-field (c-type)
  ((bit-size :initarg :bit-size :reader bit-size-of)
   (bit-offset :initarg :bit-offset :reader bit-offset-of)
   (count :initform 1 :initarg :count :reader count-of)))


(defun entity-field->c (field)
  (let* ((id (entity-type->c field))
         (name (default-c-name-to-lisp (claw.spec:foreign-entity-name field)))
         (bit-size (claw.spec:foreign-entity-bit-size field))
         (bit-offset (claw.spec:foreign-record-field-bit-offset field)))
    (destructuring-bind (kind &optional type count) (ensure-list id)
      (if (and (eq kind :array) (numberp count))
          (make-instance 'c-field :id type
                                  :name name
                                  :bit-size bit-size
                                  :bit-offset bit-offset
                                  :count count)
          (make-instance 'c-field :id id
                                  :name name
                                  :bit-size bit-size
                                  :bit-offset bit-offset)))))


(defun parse-c-record (class entity)
  (let* ((id (entity-type->c entity))
         (name (if (emptyp (claw.spec:foreign-entity-name entity))
                   (second id)
                   (default-c-name-to-lisp (claw.spec:foreign-entity-name entity))))
         (fields (mapcar #'entity-field->c (claw.spec:foreign-record-fields entity))))
    (register-c-type (make-instance class :id id
                                          :name name
                                          :fields fields))
    id))


(defun generate-c-fields (fields)
  (loop for field in fields
        for field-id = (id-of field)
        ;; unless (and (listp field-id) (or (eq :struct (first field-id))))
        collect `(,(name-of field) ,(find-native-cffi-type field-id)
                  ,@(when (> (count-of field) 1)
                      `(:count ,(count-of field))))))


(defun generate-record-binding (kind type name)
  (with-slots ((this-name name) (this-fields fields)) type
    (let ((actual-name (or name (name-of type))))
      `((,kind ,actual-name
               ,@(generate-c-fields this-fields))))))

;;;
;;; STRUCT
;;;
(defclass c-struct (c-record) ())


(defmethod parse-c-type ((entity claw.spec:foreign-struct) spec)
  (parse-c-record 'c-struct entity))


(defmethod generate-binding ((type c-struct) &key name)
  (generate-record-binding 'cffi:defcstruct type name))


;;;
;;; UNION
;;;
(defclass c-union (c-record) ())


(defmethod parse-c-type ((entity claw.spec:foreign-union) spec)
  (parse-c-record 'c-union entity))


(defmethod generate-binding ((type c-union) &key name)
  (generate-record-binding 'cffi:defcunion type name))
