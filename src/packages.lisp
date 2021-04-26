(cl:defpackage :claw
  (:use :claw.util :claw.wrapper :claw.cffi.c)
  (:export #:defwrapper
           #:include
           #:load-wrapper

           #:build-adapter
           #:initialize-adapter))
