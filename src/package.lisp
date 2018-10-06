(uiop:define-package :claw
  (:use #:cl #:alexandria)
  (:export

   #:by-removing-prefixes
   #:by-removing-complex-prefix
   #:by-changing
   #:in-pipeline

   #:ptr

   #:alloc #:calloc #:realloc #:free
   #:with-alloc #:with-free #:with-many-alloc #:with-many-free #:with-calloc
   #:null-pointer-p
   #:memcpy #:sizeof #:offsetof #:alignof

   #:with-float-traps-masked

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

   #:c-let #:c-with #:c-val
   #:c-fun #:c-ref

   #:dump-c-wrapper))
