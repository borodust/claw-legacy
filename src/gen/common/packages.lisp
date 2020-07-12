(uiop:define-package :claw.generator.common
  (:use :cl :alexandria :claw.util)
  (:export #:explode-library-definition

           #:generate-binding
           #:generate-forward-declaration
           #:generate-forward-declaration-from-typespec
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
           #:entity->c-name
           #:emulated-primitive-p
           #:anonymousp

           #:void-pointer

           #:generator))
