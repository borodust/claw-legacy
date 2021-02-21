(cl:in-package :iffi)

(defun find-quoted (value)
  (cond
    ((keywordp value) value)
    ((and (listp value) (eq 'quote (first value))) (second value))))


(defmacro initialize-iffi ()
  (let ((alloc-name
          (cond
            ((cffi:foreign-symbol-pointer "aligned_alloc") "aligned_alloc")
            ((cffi:foreign-symbol-pointer "_aligned_malloc") "_aligned_malloc")
            (t (error "Aligned memory allocation function not found. No C std library linked?")))))
    `(progn
       (declaim (inline iffi::aligned-alloc))
       (cffi:defcfun (,alloc-name iffi::aligned-alloc) :pointer
         (byte-alignment :size)
         (byte-size :size)))))


(defmacro meta-eval (&body body)
  `(eval-when (:compile-toplevel
               :execute
               ,@(unless (member :iffi-ephemeral-metadata *features*) '(:load-toplevel)))
     ,@body))
