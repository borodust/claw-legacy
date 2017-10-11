(cl:in-package :bodge-autowrap.libffi)

(autowrap:c-include '(#:bodge-autowrap/libffi #:bodge-autowrap-spec "libffi.h")
  :spec-path '(#:bodge-autowrap/libffi #:bodge-autowrap-spec)
  :sysincludes '("/usr/lib64/libffi-3.2.1/include")

  :exclude-definitions ("ffi_prep_cif_core")
  :symbol-exceptions (("FFI_TYPE" . "ffi-id-type"))

  :no-accessors cl:t
  :release-p cl:t)
