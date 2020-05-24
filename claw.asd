(asdf:defsystem :claw/util
  :description "Various utilities used across CLAW subsystems"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cl-ppcre :local-time)
  :pathname "src/"
  :serial t
  :components ((:file "sha1")
               (:file "util")))


(asdf:defsystem :claw/spec
  :description "C/C++ spec API"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :claw-support :claw/util)
  :pathname "src/spec/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "inclusion")
               (:file "resect")
               (:file "specification")
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


(asdf:defsystem :claw/spec/resect
  :description "Spec generation support using libresect"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :claw-support :cl-resect :claw/util)
  :pathname "src/resect/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "inclusion")
               (:file "resect")
               (:file "specification")
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


(asdf:defsystem :claw/wrapper
  :description "Wrapper definition interface for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cl-ppcre :sha1 :claw/util :claw/spec)
  :pathname "src/wrap/"
  :serial t
  :components ((:file "packages")
               (:file "library")
               (:file "wrapper")))


(asdf:defsystem :claw/generator/common
  :description "Common code for included CLAW generators"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cffi :cl-ppcre
               :trivial-features :claw/util
               :claw/spec :claw/wrapper)
  :pathname "src/gen/common/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "library")
               (:module adapter
                :serial t
                :components ((:file "adapter")
                             (:static-file "template/dynamic.c")
                             (:file "dynamic")
                             (:static-file "template/static.c")
                             (:file "static")))))


(asdf:defsystem :claw/cffi
  :description "CFFI generator for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:claw/generator/common)
  :pathname "src/gen/cffi/c/"
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
               (:file "library")))


(asdf:defsystem :claw/iffi
  :description "Intricate foreign function interface generator for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:claw/generator/common)
  :pathname "src/gen/iffi/cxx/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "library")
               (:module "ffi"
                :serial t
                :components ((:file "class")
                             (:file "funcall")))
               (:module "generator"
                :serial t
                :components ((:file "function")
                             (:file "class")))))


(asdf:defsystem :claw
  :description "Generate clean & lean bindings to foreign libraries easily"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:cffi :claw/wrapper :claw/cffi :claw/iffi)
  :pathname "src/"
  :serial t
  :components ((:file "packages")))


(asdf:defsystem :claw/tests
  :description "Tests for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
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
