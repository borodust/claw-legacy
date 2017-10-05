(defpackage+-1:defpackage+ :bodge-autowrap
  (:use #:bodge-c)
  (:import-except-conflicts #:bodge-autowrap.libffi))

 ;; Variables

(in-package :bodge-autowrap)

(defvar *libffi-cif* (make-hash-table)
  "Cache function CIFs")

(defvar *libffi-type-map* (make-hash-table)
  "Map autowrap types to ffi_type")

(defvar *libffi-sigs* (make-hash-table :test 'equal)
  "Cached function signatures")
