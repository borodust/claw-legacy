(cl:in-package :claw.generator.common)


(declaim (special *spec*
                  *dependency-type-list*
                  *visit-table*
                  *forward-declaration-table*
                  *dependency-list*
                  *trim-enum-prefix-p*
                  *adapter*
                  *export-table*
                  *entity-table*
                  *pointer-type*
                  *recognize-strings-p*
                  *recognize-bitfields-p*
                  *recognize-arrays-p*
                  *override-table*))


(define-constant +adapted-variable-prefix+ "__v_claw_"
  :test #'string=)


(define-constant +adapted-function-prefix+ "__claw_"
  :test #'string=)


(defgeneric adapted-function-name (function))
(defgeneric adapted-function-parameters (function))
(defgeneric adapted-function-return-type (function))
(defgeneric adapted-function-body (function))


(defun export-symbol (symbol)
  (setf (gethash symbol *export-table*) symbol))


(defun get-overriden-type (type)
  (gethash type *override-table* type))


(defun adapter ()
  *adapter*)


(defun register-adapted-function (function))


(defun expand-constant (name value)
  (let ((name (format-symbol (or (package-name (symbol-package name))
                                 *package*) "+~A+" name)))
    (export-symbol name)
    `((defparameter ,name ,value))))


(defun name->cffi-type (name)
  (switch (name :test #'string=)
    ("char" :char)
    ("signed char" :char)
    ("unsigned char" :unsigned-char)
    ("short" :short)
    ("unsigned short" :unsigned-short)
    ("int" :int)
    ("unsigned int" :unsigned-int)
    ("long" :long)
    ("unsigned long" :unsigned-long)
    ("long long" :long-long)
    ("unsigned long long" :unsigned-long-long)
    ("float" :float)
    ("double" :double)
    ("long double" (c-name->lisp :long-double :type))
    ("void" :void)
    (t (c-name->lisp name :type))))


(defun entity->cffi-type (entity)
  (labels ((%enveloped-entity ()
             (claw.spec:foreign-enveloped-entity entity))
           (%enveloped-char-p ()
             (and (typep (%enveloped-entity) 'claw.spec:named)
                  (string= "char" (claw.spec:foreign-entity-name (%enveloped-entity)))))
           (%lisp-name ()
             (get-overriden-type
              (name->cffi-type (or (claw.spec:foreign-entity-name entity)
                                   (claw.spec:foreign-entity-id entity))))))
    (typecase entity
      (claw.spec:foreign-pointer (if (%enveloped-char-p)
                                     (get-overriden-type :string)
                                     (list :pointer (entity->cffi-type (%enveloped-entity)))))
      (claw.spec:foreign-reference (list :reference (entity->cffi-type (%enveloped-entity))))
      (claw.spec:foreign-array (let ((dimensions (claw.spec:foreign-array-dimensions entity)))
                                 (cond
                                   ((and (= (length dimensions) 1) (%enveloped-char-p))
                                    (get-overriden-type :string))
                                   (dimensions
                                    (list :array (entity->cffi-type (%enveloped-entity))
                                          (apply #'* dimensions)))
                                   (t
                                    (list :pointer (entity->cffi-type (%enveloped-entity)))))))
      (claw.spec:foreign-struct (list :struct (%lisp-name)))
      (claw.spec:foreign-union (list :union (%lisp-name)))
      (claw.spec:foreign-class (list :struct (%lisp-name)))
      (t (%lisp-name)))))
