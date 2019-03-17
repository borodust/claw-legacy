(cl:in-package :claw.spec)


(defvar *tag* nil)


(defgeneric compose-entity-reference (entity)
  (:documentation "Compose FORM referencing a type from entity"))


(defgeneric compose-type-reference (type-group type-name &rest type-args)
  (:documentation "Compose FORM referencing a type from a type descriptor"))


(defclass aligned ()
  ((bit-size :initarg :bit-size :initform nil :reader foreign-entity-bit-size)
   (bit-alignment :initarg :bit-alignment :initform nil :reader foreign-entity-bit-alignment)))


(defclass foreign-entity ()
  ((id :initarg :id :initform nil :reader foreign-entity-id
       :type (or string number))
   (name :initarg :name :initform (error ":name missing") :reader foreign-entity-name
         :type (or string number))
   (location :initarg :location :initform nil :reader foreign-entity-location
             :type (or null string))
   (type :initarg :type :initform (error ":type missing") :reader foreign-entity-type)))


(defmethod foreign-entity-name ((object symbol))
  (if (keywordp object)
      object
      (error "Invalid type: ~S" object)))


(defmethod print-object ((o foreign-entity) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A" (foreign-entity-name o))))


(defclass foreign-symbol (foreign-entity) ())


(defmethod parse-form (form tag)
  (if (stringp tag)
      (if-let ((entity (find-foreign-entity tag)))
        (foreign-entity-type entity)
        (parse-form form (keywordify tag)))
      (error "Unrecognized tag: ~A" tag)))


(defmethod compose-type-reference (type-group type-name &rest type-args)
  (compose-entity-reference (find-foreign-entity (if type-group
                                                     (list* type-group
                                                            type-name
                                                            type-args)
                                                     type-name))))

(defun compose-reference (type)
  (if (listp type)
      (apply #'compose-type-reference type)
      (compose-type-reference nil type)))


(defun type-reference-p (tag)
  (and tag (not (emptyp tag)) (equal #\: (aref tag 0))))


(defun extract-entity-type (typespec)
  (let ((typespec-list (ensure-list typespec)))
    (case (first typespec-list)
      ((or :pointer :array) (second typespec-list))
      (t typespec))))
