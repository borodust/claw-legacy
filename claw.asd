(asdf:defsystem :claw
  :description "Import c2ffi specs and generate CFFI wrappers"
  :author "Ryan Pavlik, Pavel Korolev"
  :license "BSD-2-Clause"
  :version "1.0"
  :depends-on (:uiop :alexandria :cffi :cl-json :cl-ppcre :trivial-features)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "conditions")
               (:file "c2ffi")
               (:file "sffi")
               (:file "alloc")
               (:file "errno")
               (:file "processing")
               (:file "parse")
               (:file "bitmask")
               (:file "cbv")
               (:file "plus-c")))
