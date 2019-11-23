(cl:in-package :claw.tests)

(uiop:define-package :%libcxxtest
  (:use))


(defwrapper (libcxxtest
             (:base-path (asdf:system-relative-pathname :claw/tests "t/cxx/iffi/"))
             (:headers "cxx.hxx")
             (:includes "../lib/")
             (:include-definitions "^(tst_|TST_)\\w*")
             (:spec-path "spec/")
             (:language :c++)
             (:generator :claw/iffi))
  :in-package :%libcxxtest
  :trim-enum-prefix t
  :with-adapter :dynamic
  :recognize-strings t
  :override-types ((:pointer claw-utils:claw-pointer)
                   (:string claw-utils:claw-string))
  :symbolicate-names (:by-removing-prefixes "tst_" "TST_"))
