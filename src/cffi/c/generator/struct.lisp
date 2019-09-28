(cl:in-package :claw.cffi.c)


(declaim (special *anonymous-field-number*))


(uiop:define-package :%claw.anonymous
  (:use))


(defun next-anonymous-field-number ()
  (prog1 *anonymous-field-number*
    (incf *anonymous-field-number*)))

;;;
;;; RECORD
;;;
(defun field-c-name->lisp (field)
  (let ((name (claw.spec:foreign-entity-name field)))
    (if (emptyp name)
        (let ((lispified (format-symbol :%claw.anonymous "~A"
                                        (next-anonymous-field-number))))
          (setf (getf (symbol-plist lispified) :cffi-c-ref-anonymous-field-p) t)
          lispified)
        (c-name->lisp name :field))))


(defun %generate-c-field (field name kind type count)
  (if (and (eq kind :array) (numberp count))
      `(,name ,(entity-typespec->cffi type) :count ,count)
      `(,name ,(entity-type->cffi field))))


(defun generate-c-field (record-kind field)
  (let* ((name (field-c-name->lisp field))
         (type (claw.spec:foreign-entity-type field))
         (entity (claw.spec:find-foreign-entity type *spec*)))
    (export-symbol name)
    (append
     (destructuring-bind (kind &optional actual-type count)
         (ensure-list (if (and entity
                               (typep entity 'claw.spec:foreign-alias)
                               (not *recognize-arrays-p*))
                          (ensure-list (claw.spec:find-basic-type type *spec*))
                          type))
       (%generate-c-field field name kind actual-type count))
     (when (eq record-kind :struct)
       `(:offset ,(/ (claw.spec:foreign-record-field-bit-offset field) 8))))))


(defun generate-c-fields (kind entity)
  (flet ((%generate-c-field (field)
           (generate-c-field kind field)))
    (let ((*anonymous-field-number* 0))
      (mapcar #'%generate-c-field (claw.spec:foreign-record-fields entity)))))


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
  (generate-record-binding 'cffi:defcstruct entity name nil))


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
  (generate-record-binding 'cffi:defcunion entity name nil))


(defmethod generate-forward-declaration-from-typespec ((kind (eql :union))
                                                       &optional name &rest opts)
  (declare (ignore opts))
  (%generate-forward-declaration 'cffi:defcunion name))
