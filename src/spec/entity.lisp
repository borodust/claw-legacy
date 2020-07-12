(uiop:define-package :claw.spec
  (:use #:cl #:alexandria #:claw.util)
  (:export #:foreign-entity
           #:foreign-entity-id

           #:foreign-owner

           #:foreign-named-p
           #:foreign-entity-name
           #:foreign-entity-mangled-name

           #:foreign-entity-namespace

           #:foreign-envelope-p
           #:foreign-enveloped-entity

           #:foreign-aligned-p
           #:foreign-entity-bit-size
           #:foreign-entity-bit-alignment

           #:foreign-plain-old-data-type-p

           #:foreign-entity-location

           #:foreign-declared-p
           #:foreign-location
           #:foreign-location-path
           #:foreign-location-line
           #:foreign-location-column

           #:foreign-primitive

           #:foreign-array
           #:foreign-array-dimensions

           #:foreign-pointer
           #:foreign-reference

           #:foreign-constant
           #:foreign-constant-value

           #:foreign-alias

           #:foreign-enum
           #:foreign-enum-type
           #:foreign-enum-values

           #:foreign-record
           #:foreign-union
           #:foreign-struct
           #:foreign-class

           #:foreign-record-abstract-p
           #:foreign-record-parents
           #:foreign-record-field
           #:foreign-record-fields
           #:foreign-record-field-bit-offset
           #:foreign-record-field-bitfield-p
           #:foreign-record-field-bit-width

           #:foreign-function-prototype
           #:foreign-function
           #:foreign-function-variadic-p
           #:foreign-function-result-type
           #:foreign-function-storage-class

           #:foreign-parameter
           #:foreign-function-parameters

           #:foreign-method

           #:foreign-variable
           #:foreign-entity-value

           #:foreign-entity-parameter
           #:foreign-entity-type-parameter
           #:foreign-entity-value-parameter
           #:foreign-entity-parameters

           #:foreign-entity-specialization
           #:foreign-const-qualifier

           #:foreign-entity-unknown-p

           #:foreign-constructor-p
           #:format-foreign-location
           #:format-full-foreign-entity-name))
(cl:in-package :claw.spec)

;;;
;;; ALIGNED
;;;
(defclass aligned ()
  ((bit-size :initarg :bit-size :initform (error ":bit-size missing")
             :reader foreign-entity-bit-size)
   (bit-alignment :initarg :bit-alignment :initform (error ":bit-alignment missing")
                  :reader foreign-entity-bit-alignment)))


(defun foreign-aligned-p (entity)
  (typep entity 'aligned))


(defclass named ()
  ((name :initform nil
         :type (or null string)
         :reader foreign-entity-name)
   (namespace :initarg :namespace
              :initform nil
              :type (or null string)
              :reader foreign-entity-namespace)
   (mangled :initform nil
            :type (or null string)
            :reader foreign-entity-mangled-name)))


(defun foreign-named-p (entity)
  (typep entity 'named))


(defmethod initialize-instance :after ((this named) &key name mangled)
  (with-slots ((this-name name) (this-mangled mangled)) this
    (setf this-name (unless (emptyp name) name)
          this-mangled (unless (emptyp mangled) mangled))))


(defmethod print-object ((o named) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~A" (format-full-foreign-entity-name o))))


(defclass identified ()
  ((id :initarg :id
       :initform (error ":id missing")
       :reader foreign-entity-id)))


;;;
;;; OWNABLE
;;;
(defclass ownable ()
  ((owner :initarg :owner
          :initform nil
          :reader foreign-owner)))


(defmethod foreign-owner (entity)
  (declare (ignore entity))
  nil)


;;;
;;; LOCATION
;;;
(defclass foreign-location ()
  ((path :initarg :path
         :initform (error ":path missing")
         :reader foreign-location-path)
   (line :initarg :line
         :initform (error ":line missing")
         :reader foreign-location-line)
   (column :initarg :column
           :initform (error ":column missing")
           :reader foreign-location-column)))


(defmethod print-object ((o foreign-location) s)
  (print-unreadable-object (o s :type t :identity nil)
    (when (foreign-location-path o)
      (format s "\"~A:~A:~A\""
              (uiop:native-namestring (foreign-location-path o))
              (foreign-location-line o)
              (foreign-location-column o)))))


(defclass declared ()
  ((location :initarg :location
             :initform nil
             :type (or null foreign-location)
             :reader foreign-entity-location)))


(defun foreign-declared-p (entity)
  (typep entity 'declared))


;;;
;;; ENVELOPE
;;;
(defclass envelope ()
  ((type :initarg :enveloped
         :initform (error ":enveloped missing")
         :reader foreign-enveloped-entity)))


(defun foreign-envelope-p (entity)
  (typep entity 'envelope))


(defmethod print-object ((o envelope) s)
  (print-unreadable-object (o s :type t :identity nil)
    (print-object (foreign-enveloped-entity o) s)))


;;;
;;; ENTITY
;;;
(defclass foreign-entity () ())


;;;
;;; TYPE
;;;
(defclass foreign-type (aligned identified named foreign-entity)
  ((plain-old-data-type-p :initarg :plain-old-data-type
                          :initform nil
                          :reader foreign-plain-old-data-type-p)))


;;;
;;; PRIMITIVE
;;;
(defclass foreign-primitive (foreign-type) ())


;;;
;;; CONSTANT
;;;
(defclass foreign-constant (declared ownable named)
  ((value :initarg :value
          :initform (error ":value missing")
          :reader foreign-constant-value)))


;;;
;;; ENUM
;;;
(defclass foreign-enum (declared ownable foreign-type)
  ((values :initarg :values
           :initform nil
           :reader foreign-enum-values)
   (type :initarg :type
         :initform nil
         :reader foreign-enum-type)))


;;;
;;; PARAMETERIZED
;;;
(defclass parameterized ()
  ((entity-parameters :initarg :entity-parameters
                      :initform nil
                      :reader foreign-entity-parameters)))


(defclass foreign-entity-parameter (identified declared named foreign-entity) ())


(defclass foreign-entity-type-parameter (foreign-entity-parameter) ())


(defclass foreign-entity-value-parameter (foreign-entity-parameter)
  ((type :initarg :type
         :initform (error ":type missing")
         :reader foreign-entity-parameter-type)))


;;;
;;; RECORD
;;;
(defclass foreign-record-field (declared aligned named envelope)
  ((bit-offset :initarg :bit-offset
               :initform (error ":bit-offset missing")
               :type fixnum
               :reader foreign-record-field-bit-offset)
   (bitfield-p :initarg :bitfield-p :initform nil
               :reader foreign-record-field-bitfield-p)
   (bit-width :initarg :bit-width
              :initform 0 :reader foreign-record-field-bit-width
              :type fixnum)))


(defclass foreign-record (declared parameterized ownable foreign-type)
  ((fields :initarg :fields
           :initform nil
           :reader foreign-record-fields)
   (parents :initarg :parents
            :initform nil
            :reader foreign-record-parents)
   (abstract-p :initarg :abstract
               :initform nil
               :reader foreign-record-abstract-p)))


(defclass foreign-struct (foreign-record) ())


(defclass foreign-union (foreign-record) ())


(defclass foreign-class (foreign-record) ())


;;;
;;; FUNCTION
;;;
(defclass foreign-parameter (declared
                             parameterized
                             named
                             foreign-entity
                             envelope)
  ())


(defclass foreign-function-prototype (ownable foreign-entity)
  ((result-type :initarg :result-type
                :initform (error ":result-type missing")
                :type foreign-entity
                :reader foreign-function-result-type)
   (parameters :initarg :parameters
               :initform nil
               :reader foreign-function-parameters)
   (variadic-p :initarg :variadic
               :initform nil
               :reader foreign-function-variadic-p)))


(defclass foreign-function (declared
                            identified
                            named
                            parameterized
                            foreign-function-prototype)
  ((storage-class :initarg :storage-class
                  :initform nil
                  :reader foreign-function-storage-class)))


(defclass foreign-method (foreign-function) ())


;;;
;;; TYPEDEF
;;;
(defclass foreign-alias (declared identified named ownable foreign-entity envelope) ())


;;;
;;; ARRAY
;;;
(defclass foreign-array (foreign-entity envelope)
  ((dimensions :initarg :dimensions
               :initform nil
               :reader foreign-array-dimensions)))


(defmethod initialize-instance :after ((this foreign-array) &key size)
  (with-slots (dimensions) this
    (setf dimensions (ensure-list size))))


;;;
;;; POINTER
;;;
(defclass foreign-pointer (foreign-entity envelope) ())


;;;
;;; REFERENCE
;;;
(defclass foreign-reference (foreign-entity envelope) ())


;;;
;;; VARIABLE
;;;
(defclass foreign-variable (named foreign-entity)
  ((value :initarg :value
          :initform (error ":value missing")
          :reader foreign-entity-value)))


;;;
;;; SPECIALIZATION
;;;
(defclass foreign-entity-specialization (foreign-entity envelope)
  ((args :initarg :arguments
         :initform (error ":arguments missing")
         :reader foreign-specialization-arguments)))


;;;
;;; CONST QUALIFIED
;;;
(defclass foreign-const-qualifier (foreign-entity envelope) ())


;;;
;;; UNKNOWN
;;;
(defgeneric foreign-entity-unknown-p (entity)
  (:method (entity)
    (declare (ignore entity))
    nil))


;;;
;;; UTIL
;;;
(defun format-foreign-location (location &optional (print-line-and-column-p t))
  (if (and location
           (claw.spec:foreign-location-path location))
      (format nil "~A~@[~A~]"
              (claw.spec:foreign-location-path location)
              (when print-line-and-column-p
                (format nil ":~A:~A"
                        (claw.spec:foreign-location-line location)
                        (claw.spec:foreign-location-column location))))
      "::"))


(defun foreign-constructor-p (entity)
  (and (typep entity 'claw.spec:foreign-method)
       (equal (claw.spec:foreign-entity-name (claw.spec:foreign-owner entity))
              (claw.spec:foreign-entity-name entity))))


(defun format-full-foreign-entity-name (entity &key (include-method-owner t))
  (let ((current-owner (if (foreign-constructor-p entity)
                           (foreign-owner (foreign-owner entity))
                           (foreign-owner entity))))
    (format nil "~@[~A::~]~@[~A::~]~A"
            (foreign-entity-namespace entity)
            (when (and (or include-method-owner
                           (not (typep entity 'foreign-method)))
                       current-owner)
              (format nil "~{~A~^::~}"
                      (loop for owner = current-owner then (claw.spec:foreign-owner owner)
                            while owner
                            collect (or (claw.spec:foreign-entity-name owner) ""))))
            (or (foreign-entity-name entity) ""))))
