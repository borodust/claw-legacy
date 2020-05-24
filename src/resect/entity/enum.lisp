(cl:in-package :claw.spec)


(defclass foreign-enum (foreign-entity)
  ((values :initarg :values :initform nil :reader foreign-enum-values)))


(defun register-foreign-enum (id name mangled location value-list)
  "Define a foreign enum given `NAME`, a symbol, and a list of
symbol-to-integer mappings, `VALUE-LIST`.  ID should be 0 unless
anonymous-indexing is enabled and this will be later typedef'd
by ID."
  ;; Type is somewhat irrelevant here; enums are always :int-sized and
  ;; a foreign-enum is always an :enum.
  (loop for value in value-list
        do (assert (and (stringp (car value))
                        (integerp (cdr value)))))
  (let ((enum-type (list :enum (if (emptyp name)
                                   id
                                   name))))
    (register-foreign-entity enum-type
                             (make-instance 'foreign-enum :id id
                                                          :name name
                                                          :mangled mangled
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
  (alist-bind (name id location fields mangled) form
    (foreign-entity-type
     (register-foreign-enum id name mangled location (parse-enum-fields fields)))))


(defmethod compose-form ((this foreign-enum))
  (alist :tag "enum"
         :id (foreign-entity-id this)
         :name (foreign-entity-name this)
         :mangled (foreign-entity-mangled-name this)
         :location (foreign-entity-location this)
         :fields (compose-enum-fields this)))


(defmethod compose-entity-reference ((this foreign-enum))
  (alist :tag ":enum"
         :id (foreign-entity-id this)
         :name (foreign-entity-name this)))


(defmethod try-including-entity ((entity foreign-enum))
  (if (call-next-method)
      (prog1 t
        (mark-included (foreign-entity-type entity)))
      (when (loop for (key . value) in (foreign-enum-values entity)
                    thereis (explicitly-included-p key (foreign-entity-location entity)))
        (mark-included (foreign-entity-type entity))
        t)))


;;;
;;; RESECT
;;;
(defmethod parse-declaration ((type (eql :enum)) decl)
  (let (value-list)
    (resect:docollection (decl (%resect:enum-constants decl))
      (push (cons (%resect:declaration-name decl) (%resect:enum-constant-value decl)) value-list))
    (foreign-entity-type
     (register-foreign-enum (%resect:declaration-id decl)
                            (%resect:declaration-name decl)
                            (%resect:declaration-mangled-name decl)
                            (format-declaration-location decl)
                            (nreverse value-list)))))


(defmethod parse-type (category (kind (eql :enum)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type)))
