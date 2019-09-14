(cl:defpackage :claw.tests
  (:use :cl :fiveam :claw :cffi-c-ref)
  (:export #:run))

(5am:def-suite :claw.tests)
