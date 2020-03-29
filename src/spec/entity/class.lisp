(cl:in-package :claw.spec)


(defclass foreign-class (foreign-record) ())


(defun register-foreign-class (id name location type bit-size bit-alignment field-list)
  (let* ((class (find-foreign-entity type)))
    (flet ((%make-foreign-class ()
             (make-instance 'foreign-class
                            :id id
                            :name name
                            :location location
                            :type type
                            :bit-size bit-size
                            :bit-alignment bit-alignment
                            :fields field-list)))
      (if class
          class
          (register-foreign-entity type (%make-foreign-class))))))


(defmethod parse-form (form (tag (eql :class)))
  #++(warn "Skipping class ~A" form))


(defmethod compose-form ((this foreign-class))
  (compose-record-form this "class"))


(defmethod compose-type-reference ((group (eql :class)) name &rest args)
  (declare (ignore args))
  (compose-record-reference group name "class"))

;;;
;;; RESECT
;;;

(defmethod parse-declaration ((type (eql :class)) decl)
  (let (fields)
    #++(resect:docollection (field-decl field-decls)
         (let* ((field-type (%resect:declaration-type field-decl)))
           (push (make-instance 'foreign-record-field
                                :name (%resect:declaration-name field-decl)
                                :type (parse-type-by-category field-type)
                                :bit-size (* 8 (%resect:type-size field-type))
                                :bit-alignment (* 8 (%resect:type-alignment field-type))
                                :bit-offset (* 8 (%resect:field-offset field-decl))
                                :bitfield-p (%resect:field-bitfield-p field-decl)
                                :bit-width (%resect:field-width field-decl))
                 fields)))
    (foreign-entity-type
     (register-foreign-class (%resect:declaration-id decl)
                             (%resect:declaration-name decl)
                             (format-declaration-location decl)
                             (record-type :class
                                          (%resect:declaration-id decl)
                                          (%resect:declaration-name decl))
                             (* 8 (%resect:type-size (%resect:declaration-type decl)))
                             (* 8 (%resect:type-alignment (%resect:declaration-type decl)))
                             (nreverse fields)))))
