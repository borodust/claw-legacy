(cl:defpackage :claw
  (:use :claw.util :claw.wrapper :claw.cffi.c)
  (:export #:defwrapper

           #:build-adapter
           #:initialize-adapter))
