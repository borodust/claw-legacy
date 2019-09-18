(cl:defpackage :claw
  (:use :claw.util :claw.wrapper :claw.cffi.c)
  (:export #:defwrapper

           #:in-pipeline
           #:by-changing
           #:by-removing-prefixes
           #:by-removing-complex-prefix
           #:by-prepending
           #:if-none-matched

           #:build-adapter
           #:initialize-adapter))
