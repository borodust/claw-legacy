(cl:in-package :claw.iffi.cxx)


(defun generate-record-binding (define entity)
  (let* ((formatted (claw.spec:format-full-foreign-entity-name entity))
         (name (c-name->lisp (if (emptyp formatted)
                                 (claw.spec:foreign-entity-id entity)
                                 formatted)
                             :type)))
    `((,define (,name :size ,(* 8 (claw.spec:foreign-entity-bit-size entity))) ()
        ,(claw.spec:format-foreign-location (claw.spec:foreign-entity-location entity))))))

(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-class) &key)
  (generate-record-binding 'claw.iffi:deficlass entity))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-struct) &key)
  (generate-record-binding 'claw.iffi:defistruct entity))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-union) &key)
  (generate-record-binding 'claw.iffi:defiunion entity))
