(cl:in-package :claw.resect)


(declaim (special *prepared-entity-table*
                  *instantiated-classes*
                  *instantiated-functions*
                  *macros*
                  *instantiation-filter*))


(define-constant +instantiation-prefix+ "__claw_instantiated_"
  :test #'string=)

(define-constant +macro-prefix+ "__claw_macro_"
  :test #'string=)



(defun unless-empty (seq)
  (unless (emptyp seq)
    seq))


(defun mangle-id (id)
  (labels ((encode-non-word (match &rest registers)
             (declare (ignore registers))
             (format nil "E~X" (char-code (aref match 0)))))
    (ppcre:regex-replace-all "\\W"
                             id
                             #'encode-non-word
                             :simple-calls t)))


(defun ensure-mangled (decl)
  (let ((name (%resect:declaration-name decl))
        (mangled (unless-empty (%resect:declaration-mangled-name decl))))
    (if (or (string= name mangled) (not mangled))
        (let (found-mangled-name)
          ;; hack to extract mangled name from parameter: libclang doesn't mangle some
          ;; names properly (extern "C++"?)
          ;; it can also be empty due to decl being uninstantiated function, i guess
          (resect:docollection (param-decl (if (eq :function (%resect:declaration-kind decl))
                                               (%resect:function-parameters decl)
                                               (%resect:method-parameters decl)))
            (let ((param-name (%resect:declaration-name param-decl))
                  (param-name-mangled (%resect:declaration-mangled-name param-decl)))
              (unless (emptyp param-name-mangled)
                ;; found first mangled parameter
                ;; lets extract mangled function name
                ;; HAX
                (setf found-mangled-name (subseq param-name-mangled
                                                 0 (- (length param-name-mangled) (length param-name))))
                (return))))
          (if found-mangled-name
              found-mangled-name
              (mangle-id (%resect:declaration-id decl))))
        (or mangled (mangle-id (%resect:declaration-id decl))))))
