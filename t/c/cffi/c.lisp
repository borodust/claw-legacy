(cl:in-package :claw.tests)

(uiop:define-package :%libctest
  (:use))


(defwrapper (libctest
             (:base-path (asdf:system-relative-pathname :claw/tests "t/c/cffi/"))
             (:headers "c.h")
             (:includes "../lib/")
             (:include-definitions "^(tst_|TST_)\\w*")
             (:spec-path "spec/"))
  :in-package :%libctest
  :trim-enum-prefix t
  :with-adapter :dynamic
  :symbolicate-names (:by-removing-prefixes "tst_" "TST_"))
