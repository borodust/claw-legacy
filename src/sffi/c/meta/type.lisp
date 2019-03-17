(cl:in-package :claw.sffi.c)

(declaim (special *type-table*))

(defgeneric parse-c-type (entity spec)
  (:method (entity spec)
    (declare (ignore spec))
    (warn "Skipping foreign entity ~A" (claw.spec:foreign-entity-type entity))))


(defgeneric generate-binding (type &key &allow-other-keys))


(defclass c-type ()
  ((id :initarg :id
       :initform (error ":id missing")
       :reader id-of)
   (name :initarg :name :initform nil :reader name-of)))


(defgeneric aliased-type-of (type)
  (:method ((type c-type))
    (id-of type)))


(defun anonymous-p (type)
  (not (name-of type)))


(defun find-c-type (id)
  (gethash id *type-table*))


(defun register-c-type (type)
  (setf (gethash (id-of type) *type-table*) type))


(defun ensure-c-type (entity spec)
  (if-let ((registered-type (find-c-type (entity-type->c entity))))
    (id-of registered-type)
    (parse-c-type entity spec)))


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
    (t (default-c-name-to-lisp name))))


(defun entity-typespec->c (typespec)
  (if (listp typespec)
      (let ((kind (first typespec))
            (type (second typespec)))
        (case kind
          ((or :pointer :array) (list* kind (entity-typespec->c type) (cddr typespec)))
          (t (list kind (if (numberp type)
                            (format-symbol *package* "~A" type)
                            (default-c-name-to-lisp type))))))
      (primitive->c typespec)))


(defun entity-type->c (entity)
  (entity-typespec->c (claw.spec:foreign-entity-type entity)))


(defun find-native-cffi-type (type)
  (cond
    ((keywordp type) type)
    ((symbolp type) (find-native-cffi-type (aliased-type-of (find-c-type type))))
    ((listp type) (case (first type)
                    ((or :pointer :array) (list :pointer
                                                (find-native-cffi-type (second type))))
                    (:enum (second type))
                    (t type)))
    (t (error "Unrecognized typespec: ~A" type))))
