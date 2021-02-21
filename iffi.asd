(asdf:defsystem :iffi
  :description "Intricate foreign function interface"
  :author "Pavel Korolev"
  :license "MIT"
  :version "1.0"
  :depends-on (:alexandria :cffi :trivial-features)
  :pathname "src/iffi/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "function-index")
               (:file "iffi")))
