(cl:in-package :claw.cffi.c)


(declaim (special *spec*
                  *dependency-type-list*
                  *visit-table*
                  *forward-declaration-table*
                  *dependency-list*
                  *trim-enum-prefix-p*
                  *adapter*
                  *export-table*
                  *pointer-type*
                  *recognize-strings-p*
                  *recognize-bitfields-p*
                  *override-table*))


(define-constant +adapted-variable-prefix+ "__v_claw_"
  :test #'string=)


(define-constant +adapted-function-prefix+ "__claw_"
  :test #'string=)


(defgeneric adapted-function-name (function &optional stream))
(defgeneric adapted-function-definition (function adapted-name original-name
                                         &optional stream))
(defgeneric adapted-function-original-type (function name &optional stream))


(defun export-symbol (symbol)
  (setf (gethash symbol *export-table*) symbol))


(defun get-overriden-type (type)
  (gethash type *override-table* type))


(defun primitive->c (name)
  (switch (name :test #'equal)
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


(defun entity-typespec->cffi (typespec)
  (if (listp typespec)
      (let ((kind (first typespec))
            (type (second typespec)))
        (case kind
          ((or :pointer :array)
           (if (and *recognize-strings-p* (equal type "char"))
               (get-overriden-type :string)
               (list* (get-overriden-type kind)
                      (entity-typespec->cffi type)
                      (cddr typespec))))
          (:enum (entity-typespec->cffi type))
          (t (list (get-overriden-type kind) (c-name->lisp type :type)))))
      (get-overriden-type (primitive->c typespec))))


(defun entity-type->cffi (entity)
  (entity-typespec->cffi (claw.spec:foreign-entity-type entity)))
