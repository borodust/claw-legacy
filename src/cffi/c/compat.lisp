(cl:in-package :claw.cffi.c)


(cffi:defcstruct double-double
  (first :double)
  (second :double))

(cffi:defctype double-double (:struct double-double))

(cffi:defcstruct quad-float
  (first :float)
  (second :float)
  (thrid :float)
  (fourth :float))

(cffi:defctype quad-float (:struct quad-float))

(cffi:defcunion long-double
  (double double-double)
  (float quad-float))

(cffi:defctype long-double (:union long-double))
