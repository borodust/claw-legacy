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

(defvar *class* nil)


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


;;;
;;; IGNORE FUNCTIONS
;;;
(defun match-function-signature (entity owner-name name &rest param-type-names)
  (let* ((owner (claw.spec:foreign-owner entity))
         (any-params (eq (first param-type-names) :any))
         (entity-name (if owner-name
                          (claw.spec:foreign-entity-name entity)
                          (claw.spec:format-full-foreign-entity-name entity)))
         (expected-name (cond
                          ((and owner (eq name :ctor))
                           (claw.spec:foreign-entity-name owner))
                          ((and owner (eq name :dtor))
                           (remove-template-argument-string
                            (string+ "~" (claw.spec:foreign-entity-name owner))))
                          (t name))))
    (and (typep entity 'claw.spec:foreign-function)
         (string= entity-name expected-name)
         (if owner-name
             (when owner
               (string= owner-name (claw.spec:format-full-foreign-entity-name owner)))
             t)
         (or any-params
             (= (length (claw.spec:foreign-function-parameters entity))
                (length param-type-names)))
         (or any-params
             (loop for expected-param in (claw.spec:foreign-function-parameters entity)
                   for provided-type-name in param-type-names
                   for expected-unqualified = (claw.spec:unqualify-foreign-entity
                                               (claw.spec:foreign-enveloped-entity
                                                expected-param))
                   for expected-param-type-name = (when (claw.spec:foreign-named-p expected-unqualified)
                                                    (claw.spec:format-full-foreign-entity-name
                                                     expected-unqualified))
                   always (string= expected-param-type-name provided-type-name))))))


(defmacro ignore-functions (&body funcs)
  (let ((entity (gensym (string 'entity))))
    `(lambda (,entity)
       (or ,@(loop for fu in funcs
                   collect (if (eq (first fu) :in-class)
                               `(let ((*class* ,(second fu)))
                                  (funcall (ignore-functions ,@(cddr fu)) ,entity))
                               (destructuring-bind (name &rest params) fu
                                 `(match-function-signature ,entity
                                                            *class*
                                                            ,name
                                                            ,@params))))))))
