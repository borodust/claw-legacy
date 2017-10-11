(defpackage :bodge-autowrap-test.asdf
  (:use #:cl #:asdf))

(in-package :bodge-autowrap-test.asdf)

(defsystem :bodge-autowrap-test
  :description "Testing for BODGE-AUTOWRAP, may require manual work to run"
  :author "Ryan Pavlik"
  :license "LLGPL"
  :version "0.0"

  :depends-on (:bodge-autowrap)
  :pathname "t"
  :serial t

  :components
  ((:static-file "test.c")
   (:static-file "test.h")
   (:file "test")))
