(cl:defpackage :iffi
  (:use :cl :alexandria)
  (:export #:defifun

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
           #:destroy-intricate-instance
           #:with-intricate-instance
           #:with-intricate-instances))
