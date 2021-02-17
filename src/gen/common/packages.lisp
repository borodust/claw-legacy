(uiop:define-package :claw.generator.common
  (:use :cl :alexandria :claw.util)
  (:export #:explode-library-definition

           #:generate-binding
           #:generate-forward-declaration
           #:signal-unknown-entity
           #:check-entity-known

           #:dependablep
           #:foreign-entity-dependencies

           #:find-alias-for-entity

           #:build-adapter
           #:initialize-adapter
           #:adapter

           #:register-adapted-function
           #:function-pointer-extractor-required-p

           #:adapted-function-name
           #:adapted-function-namespace
           #:adapted-function-parameters
           #:adapted-function-result-type
           #:adapted-function-body
           #:adapted-function-entity

           #:adapt-type

           #:adapted-function
           #:format-location-comment
           #:adapt-function

           #:*qualify-records*
           #:*inline-functions*
           #:get-overriden-type
           #:export-symbol
           #:expand-constant
           #:entity->cffi-type
           #:emulated-primitive-p
           #:anonymousp
           #:call-shielded-from-unknown

           #:void
           #:pointer
           #:void-pointer
           #:unsigned-long-long
           #:parameter

           #:generator
           #:list-required-systems))
