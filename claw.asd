(asdf:defsystem :claw/util
  :description "Various utilities used across CLAW subsystems"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cl-ppcre :local-time)
  :pathname "src/util/"
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
  :components ((:file "entity")))


(asdf:defsystem :claw/wrapper
  :description "Wrapper definition interface for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cl-ppcre :claw/util)
  :pathname "src/wrap/"
  :serial t
  :components ((:file "packages")
               (:file "library")
               (:file "wrapper")))


(asdf:defsystem :claw/resect
  :description "Spec generation support using libresect"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :claw-support :cl-resect :claw/util :claw/spec :claw/wrapper)
  :pathname "src/resect/"
  :serial t
  :components ((:file "packages")
               (:file "inclusion")
               (:file "filtering")
               #++(:file "optimization")
               (:file "inspect")
               (:file "prepare")
               (:file "resect")))


(asdf:defsystem :claw/generator/common
  :description "Common code for included CLAW generators"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:uiop :alexandria :cffi :cl-ppcre
               :trivial-features :claw/util
               :claw/spec)
  :pathname "src/gen/common/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "library")
               (:module "generator"
                :serial t
                :components ((:file "type")
                             (:file "primitive")
                             (:file "enum")
                             (:file "alias")
                             (:file "record")
                             (:file "function")))
               (:module "adapter"
                :serial t
                :components ((:file "adapter")
                             (:static-file "template/dynamic.c")
                             (:file "dynamic")
                             (:static-file "template/static.c")
                             (:file "static")))))


(asdf:defsystem :claw/generator/cffi
  :description "CFFI generator for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:claw/wrapper :claw/generator/common)
  :pathname "src/gen/cffi/c/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "library")
               (:module generator
                :serial t
                :components ((:file "type")
                             (:file "constant")
                             (:file "typedef")
                             (:file "enum")
                             (:file "struct")
                             (:file "function")))))


(asdf:defsystem :claw/iffi
  :description "Intricate foreign function interface"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:alexandria :cffi)
  :pathname "src/iffi/"
  :serial t
  :components ((:file "packages")
               (:file "iffi")))


(asdf:defsystem :claw/generator/iffi
  :description "Intricate foreign function interface generator for CLAW"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:claw/wrapper :claw/generator/common :claw/iffi)
  :pathname "src/gen/iffi/cxx/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "library")
               (:module "generator"
                :serial t
                :components ((:file "type")
                             (:file "function")
                             (:file "class")
                             (:file "template")))))


(asdf:defsystem :claw
  :description "Generate clean & lean bindings to foreign libraries easily"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:cffi :claw/wrapper :claw/resect :claw/generator/cffi :claw/generator/iffi)
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
