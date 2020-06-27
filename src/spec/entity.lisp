(uiop:define-package :claw.spec
  (:use #:cl #:alexandria #:claw.util)
  (:export #:foreign-entity
           #:foreign-entity-id

           #:named
           #:foreign-entity-name
           #:foreign-entity-mangled-name
           #:foreign-namespace

           #:foreign-enveloped-entity

           #:aligned
           #:foreign-entity-bit-size
           #:foreign-entity-bit-alignment

           #:foreign-entity-location

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

           #:foreign-record-field
           #:foreign-record-fields
           #:foreign-record-field-bit-offset
           #:foreign-record-field-bitfield-p
           #:foreign-record-field-bit-width

           #:foreign-function
           #:foreign-function-variadic-p
           #:foreign-function-return-type
           #:foreign-function-storage-class

           #:foreign-parameter
           #:foreign-function-parameters

           #:foreign-method
           #:foreign-method-owner))
(cl:in-package :claw.spec)

;;;
;;; ALIGNED
;;;
(defclass aligned ()
  ((bit-size :initarg :bit-size :initform (error ":bit-size missing")
             :reader foreign-entity-bit-size)
   (bit-alignment :initarg :bit-alignment :initform (error ":bit-alignment missing")
                  :reader foreign-entity-bit-alignment)))


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


(defmethod initialize-instance :after ((this named) &key name mangled)
  (with-slots ((this-name name) (this-mangled mangled)) this
    (setf this-name (unless (emptyp name) name)
          this-mangled (unless (emptyp mangled) mangled))))


(defmethod print-object ((o named) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~@[~A::~]~@[~A~]"
            (when-let ((namespace (foreign-entity-namespace o)))
              (unless (emptyp (foreign-entity-name namespace))
                (foreign-entity-name namespace)))
            (if (emptyp (foreign-entity-name o))
                "[anonymous]"
                (foreign-entity-name o)))))


(defclass identified ()
  ((id :initarg :id
       :initform (error ":id missing")
       :reader foreign-entity-id)))


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
             :type foreign-location
             :reader foreign-entity-location)))


;;;
;;; ENVELOPE
;;;
(defclass envelope ()
  ((type :initarg :enveloped
         :initform (error ":enveloped missing")
         :reader foreign-enveloped-entity)))

;;;
;;; NAMESPACE
;;;
(defclass foreign-namespace (named) ())

;;;
;;; ENTITY
;;;
(defclass foreign-entity () ())


;;;
;;; TYPE
;;;
(defclass foreign-type (aligned identified named foreign-entity) ())


;;;
;;; PRIMITIVE
;;;
(defclass foreign-primitive (foreign-type) ())


;;;
;;; CONSTANT
;;;
(defclass foreign-constant (declared named)
  ((value :initarg :value
          :initform (error ":value missing")
          :reader foreign-constant-value)))


;;;
;;; ENUM
;;;
(defclass foreign-enum (declared foreign-type)
  ((values :initarg :values
           :initform nil
           :reader foreign-enum-values)
   (type :initarg :type
         :initform nil
         :reader foreign-enum-type)))


;;;
;;; RECORD
;;;
(defclass foreign-record-field (declared aligned named envelope)
  ((bit-offset :initarg :bit-offset
               :initform (error ":bit-offset")
               :type fixnum
               :reader foreign-record-field-bit-offset)
   (bitfield-p :initarg :bitfield-p :initform nil
               :reader foreign-record-field-bitfield-p)
   (bit-width :initarg :bit-width
              :initform 0 :reader foreign-record-field-bit-width
              :type fixnum)))


(defclass foreign-record (declared foreign-type)
  ((fields :initarg :fields
           :initform nil
           :reader foreign-record-fields)))


(defclass foreign-struct (foreign-record) ())


(defclass foreign-union (foreign-record) ())


(defclass foreign-class (foreign-record) ())


;;;
;;; FUNCTION
;;;
(defclass foreign-parameter (declared named envelope) ())


(defclass foreign-function (declared identified named foreign-entity)
  ((return-type :initarg :return-type
                :initform (error ":return-type missing")
                :type foreign-entity
                :reader foreign-function-return-type)
   (parameters :initarg :parameters
               :initform nil
               :reader foreign-function-parameters)
   (variadic-p :initarg :variadic
               :initform nil
               :reader foreign-function-variadic-p)
   (storage-class :initarg :storage-class
                  :initform nil
                  :reader foreign-function-storage-class)))


(defclass foreign-method (foreign-function)
  ((owner :initarg :owner :reader foreign-method-owner)))


;;;
;;; TYPEDEF
;;;
(defclass foreign-alias (declared identified named foreign-entity envelope) ())


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
;;; TEMPLATE
;;;
(defclass foreign-template (foreign-entity) ())
