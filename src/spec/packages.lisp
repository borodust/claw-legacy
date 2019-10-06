(uiop:define-package :claw.spec
  (:use #:cl #:alexandria #:claw.util)
  (:export #:describe-foreign-library
           #:map-platforms
           #:find-specification-for-platform
           #:find-specification-for-current-platform
           #:with-overriden-spec-path

           #:do-foreign-entities
           #:find-foreign-entity

           #:find-basic-type

           #:foreign-entity
           #:foreign-entity-id
           #:foreign-entity-name
           #:foreign-entity-type
           #:foreign-entity-basic-type
           #:foreign-entity-bit-size
           #:foreign-entity-bit-alignment
           #:foreign-entity-dependencies

           #:foreign-primitive

           #:foreign-constant
           #:foreign-constant-value

           #:foreign-alias
           #:foreign-alias-type
           #:find-base-alias-type
           #:find-alias-for-type
           #:aliases-type-p

           #:foreign-enum
           #:foreign-enum-values

           #:foreign-record
           #:foreign-union
           #:foreign-struct
           #:foreign-record-fields
           #:foreign-record-field-bit-offset
           #:foreign-record-field-bitfield-p
           #:foreign-record-field-bit-width

           #:foreign-function
           #:foreign-function-variadic-p
           #:foreign-function-return-type
           #:foreign-function-storage-class
           #:foreign-function-parameters

           #:foreign-extern))
