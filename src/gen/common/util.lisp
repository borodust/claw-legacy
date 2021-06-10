(cl:in-package :claw.generator.common)


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
                  *recognize-arrays-p*
                  *use-float-features*
                  *float-features-requested*
                  *inline-functions*
                  *override-table*
                  *entities*
                  *adapted-function-table*))

(defvar *qualify-records* t)


(define-constant +adapted-variable-prefix+ "__v_claw_"
  :test #'string=)


(define-constant +adapted-function-prefix+ "__claw_"
  :test #'string=)


(defvar *void*
  (make-instance 'claw.spec:foreign-primitive
                 :id "void"
                 :name "void"
                 :bit-size 0
                 :bit-alignment 0))

(defvar *void-pointer*
  (make-instance 'claw.spec:foreign-pointer
                 :enveloped *void*))

(defvar *unsigned-long-long*
  (make-instance 'claw.spec:foreign-primitive
                 :id "unsigned long long"
                 :name "unsigned long long"
                 :bit-size (* (cffi:foreign-type-size :unsigned-long-long) 8)
                 :bit-alignment (* (cffi:foreign-type-alignment :unsigned-long-long) 8)))


(defgeneric adapted-function-name (function))
(defgeneric adapted-function-namespace (function))
(defgeneric adapted-function-parameters (function))
(defgeneric adapted-function-result-type (function))
(defgeneric adapted-function-body (function))
(defgeneric adapted-function-entity (function))


(defun export-symbol (symbol)
  (setf (gethash symbol *export-table*) symbol))


(defun unexport-symbol (symbol)
  (remhash symbol *export-table*))


(defun get-overriden-type (type)
  (gethash type *override-table* type))


(defun adapter ()
  *adapter*)


(defun expand-constant (name value)
  (let ((name (format-symbol (or (package-name (symbol-package name))
                                 *package*) "+~A+" name)))
    (export-symbol name)
    `((defparameter ,name ,value))))


(defun name->cffi-type (name)
  (switch (name :test #'string=)
    ("bool" :bool)
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



(defun entity->cffi-type (entity &key (qualify-records *qualify-records*))
  (labels ((%enveloped-entity ()
             (claw.spec:unqualify-foreign-entity (claw.spec:foreign-enveloped-entity entity)))
           (%enveloped-char-p ()
             (let ((unwrapped (%enveloped-entity)))
               (and (typep unwrapped 'claw.spec:foreign-primitive)
                    (string= "char" (claw.spec:foreign-entity-name unwrapped)))))
           (%lisp-name ()
             (get-overriden-type
              (name->cffi-type (let ((full-name (and (claw.spec:foreign-named-p entity)
                                                     (claw.spec:format-full-foreign-entity-name entity))))
                                 (if (emptyp full-name)
                                     (claw.spec:foreign-entity-id entity)
                                     full-name))))))
    (typecase entity
      (claw.spec:foreign-pointer (if (and *recognize-strings-p* (%enveloped-char-p))
                                     (get-overriden-type :string)
                                     (list :pointer (entity->cffi-type
                                                     (%enveloped-entity)
                                                     :qualify-records qualify-records))))
      (claw.spec:foreign-reference `(:pointer ,(entity->cffi-type
                                                (%enveloped-entity)
                                                :qualify-records qualify-records)))
      (claw.spec:foreign-array (let ((dimensions (claw.spec:foreign-array-dimensions entity)))
                                 (cond
                                   ((and (= (length dimensions) 1) (%enveloped-char-p))
                                    (get-overriden-type :string))
                                   (dimensions
                                    (list :array (entity->cffi-type
                                                  (%enveloped-entity)
                                                  :qualify-records qualify-records)
                                          (apply #'* dimensions)))
                                   (t
                                    (list :pointer (entity->cffi-type
                                                    (%enveloped-entity)
                                                    :qualify-records qualify-records))))))
      (claw.spec:foreign-struct (if qualify-records
                                    `(:struct ,(%lisp-name))
                                    (%lisp-name)))
      (claw.spec:foreign-union (if qualify-records
                                   `(:union ,(%lisp-name))
                                   (%lisp-name)))
      (claw.spec:foreign-class (%lisp-name))
      (claw.spec:foreign-const-qualifier (entity->cffi-type
                                          (%enveloped-entity)
                                          :qualify-records qualify-records))
      (claw.spec:foreign-function (%lisp-name))
      (claw.spec:foreign-function-prototype :void)
      (t (%lisp-name)))))


(defun void ()
  *void*)

(defun void-pointer ()
  *void-pointer*)

(defun unsigned-long-long ()
  *unsigned-long-long*)

(defun pointer (entity)
  (make-instance 'claw.spec:foreign-pointer :enveloped entity))

(defun parameter (name entity)
  (make-instance 'claw.spec:foreign-parameter :name name :enveloped entity))
