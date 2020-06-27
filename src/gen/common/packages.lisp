(uiop:define-package :claw.generator.common
  (:use :cl :alexandria :claw.util)
  (:export #:explode-library-definition

           #:generate-binding
           #:generate-forward-declaration
           #:generate-forward-declaration-from-typespec

           #:find-foreign-entity
           #:find-alias-for-entity
           #:find-canonical-type

           #:build-adapter
           #:initialize-adapter

           #:+adapted-variable-prefix+
           #:+adapted-function-prefix+
           #:register-adapted-function
           #:adapter

           #:get-overriden-type
           #:export-symbol
           #:expand-constant
           #:entity->cffi-type
           #:emulated-primitive-p

           #:generator))
