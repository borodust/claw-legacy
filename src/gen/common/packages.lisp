(uiop:define-package :claw.generator.common
  (:use :cl :alexandria :claw.util)
  (:export #:explode-library-definition
           #:generate-binding
           #:generate-forward-declaration
           #:generate-forward-declaration-from-typespec

           #:build-adapter
           #:initialize-adapter))
