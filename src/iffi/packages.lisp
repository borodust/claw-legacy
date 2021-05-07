(cl:defpackage :iffi
  (:use :cl :alexandria)
  (:export #:defifun

           #:defitype
           #:defirecord
           #:defistruct
           #:defiunion
           #:deficlass

           #:intricate-function-pointer

           #:intricate-size
           #:intricate-alignment

           #:intricate-alloc
           #:intricate-free
           #:with-intricate-alloc
           #:with-intricate-allocs

           #:intricate-slot-value
           #:with-intricate-slots

           #:make-intricate-instance
           #:destroy-intricate-instance
           #:with-intricate-instance
           #:with-intricate-instances))
