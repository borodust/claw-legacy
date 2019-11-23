(cl:defpackage :claw.resect
  (:use :cl)
  (:export #:parse

           #:cursor-location-line
           #:cursor-location-column
           #:cursor-location-name

           #:resect-type-kind
           #:resect-type-size
           #:resect-type-alignment
           #:resect-type-offset
           #:resect-type-declaration

           #:cursor-id
           #:cursor-kind
           #:cursor-name
           #:cursor-location
           #:cursor-comment
           #:cursor-type
           #:cursor-debug-info
           ;; typedef
           #:cursor-aliased-type
           ;; enum
           #:cursor-value))
