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


(defun generate-c-field (record-kind field)
  (let* ((name (field-c-name->lisp field))
         (entity (claw.spec:foreign-enveloped-entity field)))
    (export-symbol name)
    (append
     (if (and (typep entity 'claw.spec:foreign-array)
              (claw.spec:foreign-array-dimensions entity))
         (let ((enveloped (claw.spec:foreign-enveloped-entity entity))
               (count (apply #'* (claw.spec:foreign-array-dimensions entity))))
           `(,name ,(entity->cffi-type enveloped) :count ,count))
         `(,name ,(entity->cffi-type entity)))
     (when (eq record-kind :struct)
       `(:offset ,(/ (claw.spec:foreign-record-field-bit-offset field) 8))))))


(defun generate-c-fields (kind entity)
  (flet ((%generate-c-field (field)
           (generate-c-field kind field)))
    (let ((*anonymous-field-number* 0))
      (mapcar #'%generate-c-field (claw.spec:foreign-record-fields entity)))))


(defun generate-record-binding (kind entity name fields)
  (let* ((id (entity->cffi-type entity))
         (name (or name (second id)))
         (byte-size (/ (claw.spec:foreign-entity-bit-size entity) 8)))
    (export-symbol name)
    `((,kind (,name :size ,byte-size) ,@fields))))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-record))
  (mapcar #'claw.spec:foreign-enveloped-entity (claw.spec:foreign-record-fields entity)))


(defmethod dependablep ((entity claw.spec:foreign-record))
  (declare (ignore entity))
  t)


;;;
;;; STRUCT
;;;
(defmethod generate-binding ((generator cffi-generator) (entity claw.spec:foreign-struct) &key name)
  (generate-record-binding 'cffi:defcstruct entity name
                           (generate-c-fields :struct entity)))


(defmethod generate-forward-declaration ((generator cffi-generator)
                                         (entity claw.spec:foreign-struct) &key name)
  (generate-record-binding 'cffi:defcstruct entity name nil))


;;;
;;; UNION
;;;
(defmethod generate-binding ((generator cffi-generator) (entity claw.spec:foreign-union) &key name)
  (generate-record-binding 'cffi:defcunion entity name
                           (generate-c-fields :union entity)))


(defmethod generate-forward-declaration ((generator cffi-generator)
                                         (entity claw.spec:foreign-union) &key name)
  (generate-record-binding 'cffi:defcunion entity name nil))
