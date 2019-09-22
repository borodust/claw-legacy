(cl:in-package :claw.spec)


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


(defgeneric foreign-entity-basic-type (entity &optional spec)
  (:method ((this foreign-entity) &optional spec)
    (declare (ignore spec))
    (foreign-entity-type this)))


(defgeneric foreign-entity-dependencies (entity)
  (:method (entity) (declare (ignore entity))))


(defun cleanup-dependencies (list)
  (flet ((%null-or-void (element)
           (or (null element) (equal "void" element))))
    (remove-if #'%null-or-void list)))


(defmethod optimize-entity (entity)
  (when (inclusion-status-included-p (find-entity-inclusion-status entity))
    entity))


(defun find-basic-type (typespec &optional (spec *library-specification*))
  (let ((typespec-list (ensure-list typespec)))
    (case (first typespec-list)
      ((or :pointer :array) (list* (first typespec-list)
                                   (find-basic-type (second typespec-list) spec)
                                   (cddr typespec-list)))
      (t (if-let ((entity (find-foreign-entity typespec spec)))
           (foreign-entity-basic-type entity spec)
           "void")))))


(defun optimize-typespec (typespec &optional (spec *library-specification*))
  (let ((base-type (extract-base-type typespec)))
    (if (inclusion-status-included-p (find-inclusion-status base-type))
        typespec
        (let ((basic-type (find-basic-type typespec spec)))
          (if (equal typespec basic-type)
              (case (first (ensure-list basic-type))
                ((or :pointer :array) (list :pointer "void"))
                (t "void"))
              (optimize-typespec basic-type))))))


(defmethod try-including-entity ((entity foreign-entity))
  (if (and (entity-explicitly-included-p entity)
           (not (entity-explicitly-excluded-p entity)))
      (prog1 t
        (mark-included entity t))
      (when (marked-included-p entity)
        t)))


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
      (error "Unrecognized tag: ~S" tag)))


(defmethod compose-type-reference (type-group type-name &rest type-args)
  (if-let ((entity (find-foreign-entity (if type-group
                                            (list* type-group
                                                   type-name
                                                   type-args)
                                            type-name))))
    (compose-entity-reference entity)
    (error "Failed to compose type reference and find an enity of ~S kind with name ~S"
           type-group type-name)))


(defun compose-reference (type)
  (if (listp type)
      (apply #'compose-type-reference type)
      (compose-type-reference nil type)))


(defun type-reference-p (tag)
  (and tag (not (emptyp tag)) (equal #\: (aref tag 0))))


(defun extract-base-type (typespec)
  (if (null typespec)
      "void"
      (let ((typespec-list (ensure-list typespec)))
        (case (first typespec-list)
          ((or :pointer :array) (extract-base-type (second typespec-list)))
          (t typespec)))))


(defun %find-dependency (typespec)
  (extract-base-type typespec))


(defun %find-entity-dependency (entity)
  (%find-dependency (foreign-entity-type entity)))
