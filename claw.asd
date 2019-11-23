(asdf:defsystem :claw/util
  :description "Various utilities used across CLAW subsystems"
  :author "Pavel Korolev"
  :license "BSD-2-Clause"
  :version "1.0"
  :depends-on (:uiop :alexandria :cl-ppcre :local-time)
  :pathname "src/"
  :serial t
  :components ((:file "sha1")
               (:file "util")))


(asdf:defsystem :claw/spec
  :description "Spec generation support and c2ffi interop for CLAW"
  :author "Pavel Korolev"
  :license "BSD-2-Clause"
  :version "1.0"
  :depends-on (:uiop :alexandria :claw-support :claw/resect :claw/util :cl-json)
  :pathname "src/spec/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "json")
               (:file "c2ffi")
               (:file "inclusion")
               (:file "specification")
               (:file "resect")
               (:module entity
                :serial t
                :components ((:file "entity")
                             (:file "primitive")
                             (:file "alias")
                             (:file "pointer")
                             (:file "array")
                             (:file "enum")
                             (:file "record")
                             (:file "function")
                             (:file "extern")
                             (:file "namespace")
                             (:file "reference")
                             (:file "class")
                             (:file "template")
                             (:file "thread")))))


(asdf:defsystem :claw/resect
  :description ""
  :author "Pavel Korolev"
  :license "BSD-2-Clause"
  :version "1.0"
  :depends-on (:uiop :alexandria :claw-support :claw/util :cffi)
  :pathname "src/resect/"
  :serial t
  :components ((:file "packages")
               (:file "resect")))


(asdf:defsystem :claw/wrapper
    :description "Wrapper definition interface for CLAW"
    :author "Pavel Korolev"
    :license "BSD-2-Clause"
    :version "1.0"
    :depends-on (:uiop :alexandria :cl-ppcre :sha1 :claw/util :claw/spec :claw/resect)
    :pathname "src/wrapper/"
    :serial t
    :components ((:file "packages")
                 (:file "library")
                 (:file "wrapper")))


(asdf:defsystem :claw/cffi
  :description "CFFI generator for CLAW"
  :author "Pavel Korolev"
  :license "BSD-2-Clause"
  :version "1.0"
  :depends-on (:uiop :alexandria
               :cffi :cl-json :cl-ppcre :trivial-features
                     :claw/util :claw/spec :claw/wrapper)
  :pathname "src/cffi/c/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:module generator
                :serial t
                :components ((:file "type")
                             (:file "primitive")
                             (:file "extern")
                             (:file "constant")
                             (:file "typedef")
                             (:file "enum")
                             (:file "struct")
                             (:file "function")))
               (:module adapter
                :serial t
                :components ((:file "adapter")
                             (:static-file "template/dynamic.c")
                             (:file "dynamic")
                             (:static-file "template/static.c")
                             (:file "static")))
               (:file "library")))


(asdf:defsystem :claw/iffi
  :description "IFFI generator for CLAW"
  :author "Pavel Korolev"
  :license "BSD-2-Clause"
  :version "1.0"
  :depends-on (:uiop :alexandria
               :cffi :cl-json :cl-ppcre :trivial-features
                     :claw/util :claw/spec :claw/wrapper)
  :pathname "src/iffi/cxx/"
  :serial t
  :components ())


(asdf:defsystem :claw
  :description "Generate clean & lean bindings to foreign libraries easily"
  :author "Pavel Korolev"
  :license "BSD-2-Clause"
  :version "1.0"
  :depends-on (:cffi :claw/wrapper :claw/cffi :claw/iffi)
  :pathname "src/"
  :serial t
  :components ((:file "packages")))


(asdf:defsystem :claw/tests
  :description "Tests for CLAW"
  :author "Pavel Korolev"
  :license "BSD-2-Clause"
  :version "1.0"
  :depends-on (:cffi :claw :fiveam :cffi-c-ref :claw-utils)
  :pathname "t/"
  :serial t
  :components ((:file "packages")
               (:module :resect
                :serial t
                :components ((:file "example")))
               (:module :c
                :components ((:static-file "lib/c.h")
                             (:static-file "lib/c.c")
                             (:static-file "lib/Makefile")
                             (:module :cffi
                              :serial t
                              :components ((:file "c")
                                           (:file "tests")))))
               #++(:module :cxx
                   :components ((:static-file "lib/cxx.hxx")
                                (:static-file "lib/cxx.cxx")
                                (:static-file "lib/Makefile")
                                (:module :iffi
                                 :serial t
                                 :components ((:file "cxx")
                                              (:file "tests")))))))
