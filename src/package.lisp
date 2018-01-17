(cl:defpackage :claw
  (:use #:cl #:alexandria)
  (:export

   #:by-removing-prefixes
   #:by-changing
   #:in-pipeline

   ;; Wrapper
   #:ptr #:valid-p #:invalidate
   #:wrap-pointer #:wrapper-null-p

   #:alloc #:calloc #:realloc #:free
   #:with-alloc #:with-many-alloc #:with-calloc
   #:memcpy

   #:define-enum-from-constants
   #:enum-key #:enum-value

   #:bitfield-mask

   ;; Bitmasks
   #:define-bitmask #:find-bitmask #:remove-bitmask #:mask-symbol-value
   #:mask #:mask-apply #:mask-keywords
   #:define-bitmask-from-constants #:define-bitmask-from-enum

   #:defcallback #:callback
   #:foreign-function-pointer

   #:get-errno-pointer #:errno

   ;; Parsing and input
   #:c-include #:*c2ffi-program*

   ;; Debug
   #:*trace-c2ffi*

   #:c-let #:c-with #:c-val
   #:c-fun #:c-ref))
