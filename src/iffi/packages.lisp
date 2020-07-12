(cl:defpackage :claw.iffi
  (:use :cl :alexandria)
  (:export #:defifun
           #:defistruct
           #:defiunion
           #:deficlass

           #:intricate-function-pointer))
