(uiop:define-package :claw.wrapper
  (:use #:cl #:alexandria #:claw.util)
  (:export #:defwrapper
           #:wrapper-name
           #:wrapper-specification
           #:wrapper-headers
           #:wrapper-standard
           #:wrapper-includes
           #:wrapper-last-update-time
           #:merge-wrapper-pathname

           #:expand-library-definition))
