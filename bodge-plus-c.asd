(defpackage :bodge-autowrap.asdf
  (:use #:cl #:asdf))

(in-package :bodge-autowrap.asdf)

(defsystem :bodge-plus-c
  :description "Convenience and alternative mechanic for C/autowrap"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "1.0"

  :depends-on (:bodge-autowrap)
  :pathname "plus-c"
  :serial t

  :components
  ((:file "package")
   (:file "conditions")
   (:file "plus-c")))
