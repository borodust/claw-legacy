(uiop:define-package :claw.generator.common
  (:use :cl :alexandria :claw.util)
  (:export #:explode-library-definition

           #:generate-binding
           #:generate-forward-declaration
           #:signal-unknown-entity
           #:check-entity-known

           #:dependablep
           #:foreign-entity-dependencies

           #:find-foreign-entity
           #:find-alias-for-entity
           #:find-canonical-type

           #:build-adapter
           #:initialize-adapter

           #:register-adapted-function
           #:function-pointer-extractor-required-p

           #:adapted-function-name
           #:adapted-function-parameters
           #:adapted-function-result-type
           #:adapted-function-body

           #:get-overriden-type
           #:export-symbol
           #:expand-constant
           #:entity->cffi-type
           #:emulated-primitive-p
           #:anonymousp

           #:void
           #:pointer
           #:void-pointer
           #:unsigned-long-long
           #:parameter

           #:generator))
