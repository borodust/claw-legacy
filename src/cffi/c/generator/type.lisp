(cl:in-package :claw.cffi.c)


(defun primitive->c (name)
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
    ("long double" :long-double)
    ("void" :void)
    (t (c-name->lisp name))))


(defun entity-typespec->cffi (typespec)
  (if (listp typespec)
      (let ((kind (first typespec))
            (type (second typespec)))
        (case kind
          ((or :pointer :array) (list :pointer (entity-typespec->cffi type)))
          (:enum (entity-typespec->cffi type))
          (t (list kind (c-name->lisp type)))))
      (primitive->c typespec)))


(defun entity-type->cffi (entity)
  (entity-typespec->cffi (claw.spec:foreign-entity-type entity)))


(defun anonymous-p (entity)
  (not (claw.spec:foreign-entity-name entity)))


(defun expand-constant (name value)
  (let ((name (format-symbol (or (package-name (symbol-package name))
                                 *package*) "+~A+" name)))
    (export-symbol name)
    `((defparameter ,name ,value))))


(defun typespec->c (typespec)
  (if (listp typespec)
      (destructuring-bind (kind type &optional count) typespec
        (ecase kind
          (:pointer (format nil "~A*" (typespec->c type)))
          (:array (if count
                      (format nil "~A[~A]" type count)))
          (:enum (format nil "enum ~A" type))
          (:struct (format nil "struct ~A" type))
          (:union (format nil "union ~A" type))))
      typespec))
