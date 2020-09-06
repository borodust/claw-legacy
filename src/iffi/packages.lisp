(cl:defpackage :iffi
  (:use :cl :alexandria)
  (:export #:initialize
           #:defifun

           #:defirecord
           #:defistruct
           #:defiunion
           #:deficlass

           #:intricate-function-pointer

           #:intricate-size
           #:intricate-alignment

           #:intricate-alloc
           #:intricate-free

           #:intricate-slot-value

           #:make-intricate-instance
           #:destroy-intricate-instance))
