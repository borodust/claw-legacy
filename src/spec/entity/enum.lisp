(cl:in-package :claw.spec)


(defclass foreign-enum (foreign-entity)
  ((values :initarg :values :initform nil :reader foreign-enum-values)))


(defun register-foreign-enum (id name location value-list)
  "Define a foreign enum given `NAME`, a symbol, and a list of
symbol-to-integer mappings, `VALUE-LIST`.  ID should be 0 unless
anonymous-indexing is enabled and this will be later typedef'd
by ID."
  ;; Type is somewhat irrelevant here; enums are always :int-sized and
  ;; a foreign-enum is always an :enum.
  (loop for value in value-list
        do (assert (and (stringp (car value))
                        (integerp (cdr value)))))
  (let ((enum-type (list :enum (if (> id 0) id name))))
    (register-foreign-entity enum-type
                             (make-instance 'foreign-enum :id id
                                                          :name name
                                                          :type enum-type
                                                          :location location
                                                          :values value-list))))


(defun parse-enum-fields (fields)
  (loop for field in fields
        collect (cons (aval :name field)
                      (aval :value field))))


(defun compose-enum-fields (this)
  (loop for (name . value) in (foreign-enum-values this)
        collect (alist :name name
                       :value value
                       :tag "field")))


(defmethod parse-form (form (tag (eql :enum)))
  (alist-bind (name id location fields) form
    (foreign-entity-type
     (register-foreign-enum id name location (parse-enum-fields fields)))))


(defmethod compose-form ((this foreign-enum))
  (alist :tag "enum"
         :id (foreign-entity-id this)
         :name (foreign-entity-name this)
         :location (foreign-entity-location this)
         :fields (compose-enum-fields this)))


(defmethod compose-entity-reference ((this foreign-enum))
  (alist :tag ":enum"
         :id (foreign-entity-id this)
         :name (foreign-entity-name this)))
