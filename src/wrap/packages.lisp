(uiop:define-package :claw.wrapper
  (:use #:cl #:alexandria #:claw.util)
  (:export #:defwrapper
           #:include

           #:wrapper-name
           #:wrapper-options
           #:wrapper-configuration
           #:wrapper-entities

           #:wrapper-options-headers
           #:wrapper-options-includes
           #:wrapper-options-standard
           #:wrapper-options-defines
           #:wrapper-options-intrinsics

           #:merge-wrapper-pathname

           #:generate-bindings

           #:describe-foreign-library
           #:foreign-library-entities
           #:foreign-library-language))


(uiop:define-package :%claw.wrapper.pristine
  (:use))
