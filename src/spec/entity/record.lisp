(cl:in-package :claw.spec)


(defclass foreign-record (aligned foreign-entity)
  ((fields :initarg :fields :initform nil :reader foreign-record-fields)))


(defclass foreign-struct (foreign-record) ())


(defclass foreign-union (foreign-record) ())


(defclass foreign-record-field (aligned foreign-entity)
  ((bit-offset :initarg :bit-offset :initform (error ":bit-offset")
               :reader foreign-record-field-bit-offset)
   (bitfield-p :initarg :bitfield-p :initform nil
               :reader foreign-record-field-bitfield-p
               :type (or null t))
   ;; bit-width is the bitfield width if bitfield-p is t .. this differs
   ;; from bit-size, because bit-size is the full width of the field
   (bit-width :initarg :bit-width
              :initform nil :reader foreign-record-field-bit-width
              :type (or null integer))))


(defun record-type (kind id name)
  (if (and name (not (emptyp name)))
      (list kind name)
      (list kind id)))


(defun register-foreign-record (id name location type bit-size bit-alignment field-list)
  "Define a foreign record (struct or union) given `NAME`, a symbol,
`TYPE`, either :struct or :union, and a list of fields.  The actual
name for the type will be `(:struct NAME)` or `(:union NAME)`, as
appropriate."
  (let* ((record (find-foreign-entity type)))
    (flet ((%make-foreign-record ()
             (make-instance (ecase (first type)
                              (:struct 'foreign-struct)
                              (:union 'foreign-union))
                            :id id
                            :name name
                            :location location
                            :type type
                            :bit-size (unless (zerop bit-size) bit-size)
                            :bit-alignment (unless (zerop bit-alignment)
                                             bit-alignment)
                            :fields field-list)))
      (if record
          record
          (register-foreign-entity type (%make-foreign-record))))))


(defun parse-fields (fields)
  (loop for field in fields
        collect (alist-bind (name type
                             bit-size bit-offset bit-alignment
                             bitfield bit-width)
                            field
                  (make-instance 'foreign-record-field
                                 :name name
                                 :type (parse-form type (aval :tag type))
                                 :bit-size bit-size
                                 :bit-alignment bit-alignment
                                 :bit-offset bit-offset
                                 :bitfield-p bitfield
                                 :bit-width bit-width))))


(defun compose-fields (fields)
  (loop for field in fields
        collect (alist :tag "field"
                       :name (foreign-entity-name field)
                       :type (compose-reference (foreign-entity-type field))
                       :bit-size (foreign-entity-bit-size field)
                       :bit-alignment (foreign-entity-bit-alignment field)
                       :bit-offset (foreign-record-field-bit-offset field)
                       :bitfield (if (foreign-record-field-bitfield-p field)
                                     'true
                                     'false)
                       :bit-width (foreign-record-field-bit-width field))))


(defun compose-record-form (this tag)
  (alist :tag tag
         :id (foreign-entity-id this)
         :name (foreign-entity-name this)
         :location (foreign-entity-location this)
         :bit-size (foreign-entity-bit-size this)
         :bit-alignment (foreign-entity-bit-alignment this)
         :fields (compose-fields (foreign-record-fields this))))


(defun compose-record-reference (group name group-name)
  (let ((tag (format nil ":~A" group-name)))
    (if-let ((entity (find-foreign-entity (list group name))))
      (alist :tag tag
             :id (foreign-entity-id entity)
             :name (foreign-entity-name entity))
      ;; forward reference
      (alist :tag tag
             :name (unless (integerp name) name)
             :bit-size 0
             :bit-alignment 0
             :id (if (integerp name) name 0)))))


(defun parse-record-form (form tag)
  (alist-bind (id name location fields bit-size bit-alignment) form
    (let ((type (record-type tag id name)))
      (if (or (null bit-size) (= bit-size 0))
          ;; forward or recursive decl
          type
          (foreign-entity-type
           (register-foreign-record id name location type
                                    bit-size bit-alignment
                                    (parse-fields fields)))))))
;;;
;;; STRUCTS
;;;
(defmethod parse-form (form (tag (eql :struct)))
  (parse-record-form form tag))


(defmethod compose-form ((this foreign-struct))
  (compose-record-form this "struct"))


(defmethod compose-type-reference ((group (eql :struct)) name &rest args)
  (declare (ignore args))
  (compose-record-reference group name "struct"))


;;;
;;; UNIONS
;;;
(defmethod parse-form (form (tag (eql :union)))
  (parse-record-form form tag))


(defmethod compose-form ((this foreign-union))
  (compose-record-form this "union"))


(defmethod compose-type-reference ((group (eql :union)) name &rest args)
  (declare (ignore args))
  (compose-record-reference group name "union"))
