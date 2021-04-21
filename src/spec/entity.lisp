(uiop:define-package :claw.spec
  (:use #:cl #:alexandria #:claw.util)
  (:export #:foreign-entity
           #:foreign-entity-source
           #:foreign-entity-private-p
           #:foreign-entity-forward-p

           #:foreign-identified-p
           #:foreign-entity-id

           #:foreign-owner
           #:foreign-dependent

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
           #:foreign-reference-rvalue-p

           #:foreign-constant

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
           #:foreign-function-inlined-p
           #:foreign-function-result-type
           #:foreign-function-storage-class

           #:foreign-parameter
           #:foreign-function-parameters

           #:foreign-method
           #:foreign-method-static-p
           #:foreign-method-const-p

           #:foreign-variable
           #:foreing-variable-type
           #:foreign-entity-value
           #:foreing-variable-external-p

           #:foreign-parameterizable-p
           #:foreign-entity-parameter
           #:foreign-entity-type-parameter
           #:foreign-entity-value-parameter
           #:foreign-entity-parameter-type
           #:foreign-entity-parameters

           #:foreign-entity-argument
           #:foreign-entity-arguments
           #:foreign-specialized-p

           #:foreign-const-qualifier

           #:foreign-entity-unknown-p

           #:foreign-constructor-p
           #:format-foreign-location
           #:format-full-foreign-entity-name
           #:*tag-types*
           #:format-foreign-entity-c-name

           #:unwrap-foreign-entity
           #:unalias-foreign-entity
           #:unqualify-foreign-entity))
(cl:in-package :claw.spec)


(defvar *tag-types* t)

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


(defun foreign-identified-p (entity)
  (typep entity 'identified))


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
;;; DEPENDABLE
;;;
(defclass dependable ()
  ((dependents :initarg :owner
               :initform nil
               :reader foreign-depndent)))


(defmethod foreign-depndent (entity)
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
(defclass foreign-entity ()
  ((source :initarg :source
           :initform nil
           :reader foreign-entity-source)))


(defgeneric foreign-entity-private-p (entity)
  (:method (entity)
    (declare (ignore entity))
    nil))


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
(defclass foreign-constant (declared ownable named identified foreign-entity)
  ((value :initarg :value
          :initform (error ":value missing")
          :reader foreign-entity-value)))


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


(defun foreign-parameterizable-p (entity)
  (typep entity 'parameterized))


;;;
;;; SPECIALIZED
;;;
(defclass foreign-entity-argument ()
  ((parameter :initarg :parameter
              :initform (error ":parameter missing")
              :reader foreign-entity-parameter)
   (value :initarg :value
          :initform (error ":value missing")
          :reader foreign-entity-value)))


(defclass specializable ()
  ((entity-arguments :initarg :entity-arguments
                     :initform nil
                     :reader foreign-entity-arguments)))


(defun foreign-specialized-p (entity)
  (and (typep entity 'specializable)
       (foreign-entity-arguments entity)))


(defmethod print-object ((o foreign-entity-argument) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~A = ~A"
            (foreign-entity-name (foreign-entity-parameter o))
            (let ((value (foreign-entity-value o)))
              (if (foreign-named-p value)
                  (format-full-foreign-entity-name value)
                  value)))))

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


(defclass foreign-record (declared parameterized specializable ownable dependable foreign-type)
  ((fields :initarg :fields
           :initform nil
           :reader foreign-record-fields)
   (parents :initarg :parents
            :initform nil
            :reader foreign-record-parents)
   (abstract-p :initarg :abstract
               :initform nil
               :reader foreign-record-abstract-p)
   (private-p :initarg :private
              :initform nil
              :reader foreign-entity-private-p)
   (forward-p :initarg :forward
              :initform nil
              :reader foreign-entity-forward-p)))


(defmethod foreign-entity-forward-p (any)
  (declare (ignore any))
  nil)


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
                            specializable
                            foreign-function-prototype)
  ((storage-class :initarg :storage-class
                  :initform nil
                  :reader foreign-function-storage-class)
   (inlined-p :initarg :inlined
              :initform nil
              :reader foreign-function-inlined-p)))


(defclass foreign-method (foreign-function)
  ((static-p :initarg :static
             :initform nil
             :reader foreign-method-static-p)
   (const-p :initarg :const
             :initform nil
             :reader foreign-method-const-p)))


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
(defclass foreign-reference (foreign-entity envelope)
  ((rvalue-p :initarg :rvalue
             :initform nil
             :reader foreign-reference-rvalue-p)))


;;;
;;; VARIABLE
;;;
(defclass foreign-variable (identified named foreign-entity)
  ((value :initarg :value
          :initform (error ":value missing")
          :reader foreign-entity-value)
   (type :initarg :type
         :initform (error ":type missing")
         :reader foreing-variable-type)
   (external-p :initarg :external
               :initform nil
               :reader foreing-variable-external-p)))


;;;
;;; QUALIFIER
;;;
(defclass foreign-qualifier (foreign-entity envelope) ())

;;;
;;; CONST QUALIFIED
;;;
(defclass foreign-const-qualifier (foreign-qualifier) ())


;;;
;;; UNKNOWN
;;;
(defgeneric foreign-entity-unknown-p (entity)
  (:method (entity)
    (declare (ignore entity))
    nil))


(defmethod foreign-entity-unknown-p ((entity envelope))
  (foreign-entity-unknown-p (foreign-enveloped-entity entity)))

;;;
;;; UTIL
;;;
(defun format-foreign-location (location &optional (print-line-and-column-p t))
  (if (and location
           (foreign-location-path location))
      (format nil "~A~@[~A~]"
              (foreign-location-path location)
              (when print-line-and-column-p
                (format nil ":~A:~A"
                        (foreign-location-line location)
                        (foreign-location-column location))))
      "::"))


(defun foreign-constructor-p (entity)
  (and (typep entity 'foreign-method)
       (equal (foreign-entity-name (foreign-owner entity))
              (foreign-entity-name entity))))


(defun format-full-foreign-entity-name (entity &key (include-method-owner t) (include-name t))
  (let* ((current-owner (if (foreign-constructor-p entity)
                            (foreign-owner (foreign-owner entity))
                            (foreign-owner entity)))
         (no-anonymous-parent-p (loop for owner = current-owner then (foreign-owner owner)
                                      while owner
                                      always (foreign-entity-name owner)))
         (name (foreign-entity-name entity)))
    (when (and name no-anonymous-parent-p)
      (let ((prefix (cond
                      ((and current-owner
                            (typep entity 'foreign-method)
                            (not include-method-owner))
                       (format-full-foreign-entity-name current-owner
                                                        :include-method-owner nil
                                                        :include-name nil))
                      ((and current-owner include-method-owner)
                       (format-full-foreign-entity-name current-owner))
                      (t
                       (foreign-entity-namespace entity)))))
        (if include-name
            (format nil "~@[~A::~]~@[~A~]"
                    (unless (emptyp prefix) prefix)
                    name)
            prefix)))))

;;;
;;; C NAMES
;;;
(defgeneric format-foreign-entity-c-name (entity &key &allow-other-keys))


(defun format-default-c-name (tag const-qualified &optional name)
  (format nil "~@[~A ~]~A~@[ ~A~]" (when const-qualified "const") tag name))


(defmethod format-foreign-entity-c-name ((this foreign-primitive) &key const-qualified name)
  (let ((tag (foreign-entity-name this)))
    (format-default-c-name
     (switch (tag :test #'string=)
       ("int128" "__int128")
       ("uint128" "__uint128")
       ("float128" "__float128")
       (t tag))
     const-qualified name)))


(defmethod format-foreign-entity-c-name ((this foreign-pointer) &key const-qualified name)
  (let ((enveloped (foreign-enveloped-entity this)))
    (if (typep enveloped 'foreign-function-prototype)
        (format-foreign-entity-c-name enveloped :name (format nil "*~@[~A~]" name))
        (format nil "~A*~@[ ~A~]~@[ ~A~]"
                (format-foreign-entity-c-name enveloped)
                (when const-qualified "const")
                name))))


(defmethod format-foreign-entity-c-name ((this foreign-reference) &key const-qualified name)
  (let ((enveloped (foreign-enveloped-entity this)))
    (format nil "~@[~A ~]~A&~@[ ~A~]"
            (when const-qualified "const")
            (format-foreign-entity-c-name enveloped)
            name)))


(defmethod format-foreign-entity-c-name ((this foreign-function-prototype) &key name)
  (format nil "~A(~@[~A ~]~@[~A~])(~{~A~^, ~})"
          (format-foreign-entity-c-name (foreign-function-result-type this))
          (when-let ((owner (foreign-owner this)))
            (format-full-foreign-entity-name owner))
          name
          (mapcar #'format-foreign-entity-c-name (foreign-function-parameters this))))


(defmethod format-foreign-entity-c-name ((this foreign-struct)
                                         &key const-qualified name (tag-types *tag-types*))
  (format-default-c-name (format nil "~@[~A ~]~A"
                                 (when tag-types
                                   "struct")
                                 (format-full-foreign-entity-name this))
                         const-qualified name))


(defmethod format-foreign-entity-c-name ((this foreign-union)
                                         &key const-qualified name (tag-types *tag-types*))
  (format-default-c-name (format nil "~@[~A ~]~A"
                                 (when tag-types
                                   "union")
                                 (format-full-foreign-entity-name this))
                         const-qualified name))


(defmethod format-foreign-entity-c-name ((this foreign-enum)
                                         &key const-qualified name (tag-types *tag-types*))
  (format-default-c-name (format nil "~@[~A ~]~A"
                                 (when tag-types
                                   "enum")
                                 (format-full-foreign-entity-name this))
                         const-qualified name))


(defmethod format-foreign-entity-c-name ((this foreign-class) &key const-qualified name)
  (format-default-c-name (format nil "~A" (format-full-foreign-entity-name this)) const-qualified name))


(defmethod format-foreign-entity-c-name ((this foreign-alias) &key const-qualified name)
  (format-default-c-name (format-full-foreign-entity-name this) const-qualified name))


(defmethod format-foreign-entity-c-name ((this foreign-parameter) &key)
  (format-foreign-entity-c-name (foreign-enveloped-entity this) :name (foreign-entity-name this)))


(defmethod format-foreign-entity-c-name ((this foreign-const-qualifier) &key name)
  (format-foreign-entity-c-name (foreign-enveloped-entity this) :const-qualified t :name name))


(defmethod format-foreign-entity-c-name ((this foreign-array) &key const-qualified name)
  (format nil "~@[~A ~]~A~@[ ~A~][]"
          (when const-qualified
            "const")
          (format-foreign-entity-c-name (foreign-enveloped-entity this))
          name))


(defmethod format-foreign-entity-c-name ((this foreign-entity-parameter) &key const-qualified name)
  (format-default-c-name (format-full-foreign-entity-name this) const-qualified name))


(defun unwrap-foreign-entity (entity)
  (loop for current = entity then (foreign-enveloped-entity current)
        while (foreign-envelope-p current)
        finally (return current)))


(defun unqualify-foreign-entity (entity)
  (loop for current = entity then (foreign-enveloped-entity current)
        while (typep current 'foreign-qualifier)
        finally (return current)))


(defun unalias-foreign-entity (entity)
  (loop for current = entity then (foreign-enveloped-entity current)
        while (typep current 'foreign-alias)
        finally (return current)))
