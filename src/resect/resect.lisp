(uiop:define-package :claw.resect
  (:use #:cl #:alexandria #:claw.util #:claw.spec)
  (:export))
(cl:in-package :claw.resect)


(declaim (special *declaration-table*))


(defvar *parsed-pointers* nil)


(defgeneric parse-declaration (kind declaration)
  (:method (kind declaration)
    (warn "Ignored declaration of ~A kind: ~A" kind (%resect:declaration-name declaration))))


(defmethod parse-declaration ((kind (eql :unknown)) declaration)
  (declare (ignore kind declaration)))


(defgeneric parse-type (category kind type)
  (:method (category kind type)
    (warn "Ignored type of ~A kind from ~A category: ~A" kind category (%resect:type-name type))))


(defun write-uber-header (headers path)
  (alexandria:with-output-to-file (out path :if-exists :supersede)
    (loop for header in headers
          do (format out "#include \"~A\"~%" header))))


(defclass foreign-library ()
  ((entities :initarg :entities
             :initform (error ":entities missing")
             :reader claw.wrapper:foreign-library-entities)
   (language :initarg :language
             :initform (error ":language missing")
             :reader claw.wrapper:foreign-library-language)))


(defmethod claw.wrapper:describe-foreign-library ((parser (eql :claw/resect))
                                                  headers &key
                                                            includes
                                                            frameworks
                                                            language
                                                            standard
                                                            target)
  (declare (ignore parser))
  (flet ((%stringify (value)
           (when value
             (if (stringp value)
                 value
                 (string-downcase value)))))
    (uiop:with-temporary-file (:pathname path :type "h")
      (write-uber-header headers path)
      (resect:with-translation-unit (unit (uiop:native-namestring path)
                                     :include-paths includes
                                     :framework-paths frameworks
                                     :language (%stringify language)
                                     :standard (%stringify standard)
                                     :target (%stringify target))
        (let ((*declaration-table* (make-hash-table :test 'equal)))
          (resect:docollection (decl (%resect:translation-unit-declarations unit))
            (parse-declaration (%resect:declaration-kind decl) decl))
          (make-instance 'foreign-library
                         :entities (loop for value being the hash-value of *declaration-table*
                                         collect value)
                         :language (case (%resect:translation-unit-language unit)
                                     (:c :c)
                                     (:c++ :c++)
                                     (:obj-c :objective-c))))))))


;;;
;;; UTIL
;;;
(defun register-entity (entity-class &rest args &key id &allow-other-keys)
  (if-let ((existing (gethash id *declaration-table*)))
    (values existing nil)
    (values (setf (gethash id *declaration-table*) (apply #'make-instance entity-class args))
            t)))


(defun make-declaration-location (declaration)
  (let ((location (%resect:declaration-location declaration)))
    (make-instance 'foreign-location
                   :path (uiop:ensure-pathname (%resect:location-name location))
                   :line (%resect:location-line location)
                   :column (%resect:location-column location))))


(defun parse-declaration-by-kind (decl)
  (parse-declaration (%resect:declaration-kind decl) decl))


(defun parse-type-by-category (type)
  (parse-type (%resect:type-category type) (%resect:type-kind type) type))

;;;
;;; PRIMITIVE
;;;
(defun register-void ()
  (register-entity 'foreign-primitive
                   :id "void"
                   :name "void"
                   :bit-size 0
                   :bit-alignment 0))


(defun register-primitive-resect-type (kind type)
  (flet ((register-primitive-type (name)
           (register-entity 'foreign-primitive
                            :id name
                            :name name
                            :bit-size (%resect:type-size type)
                            :bit-alignment (%resect:type-alignment type))))
    (ecase kind
      (:void (register-void))
      (:bool (register-primitive-type "bool"))
      (:unsigned-char (register-primitive-type "unsigned char"))
      (:char (register-primitive-type "char"))
      (:char16 (register-primitive-type "char16"))
      (:char32 (register-primitive-type "char32"))
      (:unsigned-short (register-primitive-type "unsigned short"))
      (:unsigned-int (register-primitive-type "unsigned int"))
      (:unsigned-long (register-primitive-type "unsigned long"))
      (:unsigned-long-long (register-primitive-type "unsigned long long"))
      (:unsigned-int128 (register-primitive-type "uint128"))
      (:char-s (register-primitive-type "char"))
      (:char-u (register-primitive-type "unsigned char"))
      (:wchar (register-primitive-type "wchar"))
      (:short (register-primitive-type "short"))
      (:int (register-primitive-type "int"))
      (:long (register-primitive-type "long"))
      (:long-long (register-primitive-type "long long"))
      (:int128 (register-primitive-type "int128"))
      (:float (register-primitive-type "float"))
      (:double (register-primitive-type "double"))
      (:long-double (register-primitive-type "long double"))
      (:nullptr (register-primitive-type "nullptr"))
      (:float128 (register-primitive-type "float128"))
      (:half (register-primitive-type "half"))
      (:float16 (register-primitive-type "float16")))))


(defmethod parse-type ((category (eql :arithmetic)) kind type)
  (declare (ignore category))
  (register-primitive-resect-type kind type))


(defmethod parse-type ((category (eql :aux)) kind type)
  (declare (ignore category))
  (register-primitive-resect-type kind type))

;;;
;;; ENUM
;;;
(defmethod parse-declaration ((type (eql :enum)) decl)
  (let ((id (%resect:declaration-id decl))
        (decl-type (%resect:declaration-type decl))
        (value-alist))
    (resect:docollection (decl (%resect:enum-constants decl))
      (push (cons (%resect:declaration-name decl) (%resect:enum-constant-value decl)) value-alist))
    (register-entity 'foreign-enum
                     :id id
                     :name (%resect:declaration-name decl)
                     :mangled (%resect:declaration-mangled-name decl)
                     :location (make-declaration-location decl)
                     :bit-size (%resect:type-alignment decl-type)
                     :bit-alignment (%resect:type-alignment decl-type)
                     :type (parse-type-by-category (%resect:enum-type decl))
                     :values (nreverse value-alist))))


(defmethod parse-type (category (kind (eql :enum)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type)))


;;;
;;; RECORD
;;;
(defun parse-record-declaration (decl record-type field-decls)
  (let ((id (%resect:declaration-id decl))
        fields)
    (resect:docollection (field-decl field-decls)
      (let* ((field-type (%resect:declaration-type field-decl)))
        (push (make-instance 'foreign-record-field
                             :name (%resect:declaration-name field-decl)
                             :location (make-declaration-location field-decl)
                             :enveloped (parse-type-by-category field-type)
                             :bit-size (%resect:type-size field-type)
                             :bit-alignment (%resect:type-alignment field-type)
                             :bit-offset (%resect:field-offset field-decl)
                             :bitfield-p (%resect:field-bitfield-p field-decl)
                             :bit-width (%resect:field-width field-decl))
              fields)))
    (register-entity (ecase record-type
                       (:struct 'foreign-struct)
                       (:union 'foreign-union)
                       (:class 'foreign-class))
                     :id id
                     :name (%resect:declaration-name decl)
                     :mangled (%resect:declaration-mangled-name decl)
                     :location (make-declaration-location decl)
                     :bit-size (%resect:type-size (%resect:declaration-type decl))
                     :bit-alignment (%resect:type-alignment (%resect:declaration-type decl))
                     :fields (nreverse fields))))


(defmethod parse-declaration ((type (eql :struct)) decl)
  (parse-record-declaration decl type (%resect:struct-fields decl)))


(defmethod parse-declaration ((type (eql :union)) decl)
  (parse-record-declaration decl type (%resect:union-fields decl)))


(defmethod parse-declaration ((type (eql :class)) decl)
  (multiple-value-bind (entity added)
      (parse-record-declaration decl type (%resect:class-fields decl))
    (when added
      (resect:docollection (method-decl (%resect:class-methods decl))
        (register-entity 'foreign-method
                         :id (%resect:declaration-id method-decl)
                         :name (%resect:declaration-name method-decl)
                         :mangled (%resect:declaration-mangled-name method-decl)
                         :location (make-declaration-location method-decl)

                         :return-type (parse-type-by-category (%resect:method-return-type method-decl))
                         :parameters nil
                         :variadic (%resect:method-variadic-p method-decl))))
    entity))


(defmethod parse-type (category (kind (eql :struct)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type)))


(defmethod parse-type (category (kind (eql :record)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type)))


(defmethod parse-type (category (kind (eql :class)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type)))


;;;
;;; FUNCTION
;;;
(defmethod parse-declaration ((type (eql :function)) decl)
  (let ((id (%resect:declaration-id decl))
        params)
    (resect:docollection (param (%resect:function-parameters decl))
      (let* ((name (%resect:declaration-name param))
             (param-type (%resect:declaration-type param)))
        (push (make-instance 'foreign-parameter
                             :name (unless (emptyp name)
                                     name)
                             :mangled (%resect:declaration-mangled-name param)
                             :location (make-declaration-location param)
                             :enveloped (parse-type-by-category param-type))
              params)))
    (register-entity 'foreign-function
                     :id id
                     :name (%resect:declaration-name decl)
                     :mangled (%resect:declaration-mangled-name decl)
                     :location (make-declaration-location decl)
                     :return-type (parse-type-by-category (%resect:function-return-type decl))
                     :parameters (nreverse params)
                     :variadic (%resect:function-variadic-p decl))))


(defmethod parse-type (category (kind (eql :function-prototype)) type)
  (declare (ignorable category kind type))
  (make-instance 'foreign-pointer :enveloped (register-void)))


;;;
;;; TYPEDEF
;;;
(defmethod parse-declaration ((kind (eql :typedef)) decl)
  (let ((id (%resect:declaration-id decl)))
    (register-entity 'foreign-alias
                     :id id
                     :name (%resect:declaration-name decl)
                     :mangled (%resect:declaration-mangled-name decl)
                     :location (make-declaration-location decl)
                     :enveloped (parse-type-by-category (%resect:typedef-aliased-type decl)))))


(defmethod parse-type (category (kind (eql :typedef)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type)))


;;;
;;; ARRAY
;;;
(defmethod parse-type ((category (eql :array)) kind type)
  (declare (ignore category kind))
  (make-instance 'foreign-array
                 :enveloped (parse-type-by-category (%resect:array-element-type type))
                 :size (let ((size (%resect:array-size type)))
                         (when (>= size 0)
                           (list size)))))

;;;
;;; POINTER
;;;
(defmethod parse-type ((category (eql :pointer)) kind type)
  (declare (ignorable category kind))
  (make-instance 'foreign-pointer
                 :enveloped (if (member (cffi:pointer-address type) *parsed-pointers* :test #'=)
                                (register-void)
                                (let ((*parsed-pointers* (cons (cffi:pointer-address type) *parsed-pointers*)))
                                  (parse-type-by-category (%resect:pointer-pointee-type type))))))


;;;
;;; REFERENCE
;;;
(defmethod parse-type ((category (eql :reference)) kind type)
  (declare (ignorable category kind))
  (make-instance 'foreign-reference
                 :enveloped (parse-type-by-category (%resect:reference-pointee-type type))))
