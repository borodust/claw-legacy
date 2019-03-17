(cl:in-package :claw.tests)
(5am:in-suite :claw.tests)


(defwrapper (libctest
             (:headers "c.h")
             (:base-path (asdf:system-relative-pathname :claw/tests "src/t/c/"))
             (:include-definitions "\\w*")
             (:spec-path "spec/"))
  :with-adapter :dynamic)
