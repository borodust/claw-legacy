(cl:in-package :claw.resect)


(declaim (special *declaration-table*))

(defvar *template* nil)
(defvar *parsed-pointers* nil)
(defvar *template-argument-map-list* nil)


(defgeneric find-template-argument (record name)
  (:method (record name)
    (declare (ignore record name))
    nil))


(defun search-for-template-argument (name)
  (loop for map in *template-argument-map-list*
          thereis (find-template-argument map name)))


(defgeneric parse-declaration (kind declaration &key &allow-other-keys))


(defgeneric parse-type (category kind type))


(defmethod parse-type :around (category kind type)
  (declare (ignorable category kind type))
  (let ((result (call-next-method)))
    (when (%resect:type-const-qualified-p type)
      (setf result (make-instance 'claw.spec:foreign-const-qualifier
                                  :enveloped result)))
    result))


(defun write-uber-header (headers path defines)
  (alexandria:with-output-to-file (out path :if-exists :supersede)
    (loop for (name value) on defines by #'cddr
          do (format out "#define ~A~@[~A~]~%" name value))
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
                                                            target
                                                            defines

                                                            include-definitions
                                                            include-sources
                                                            exclude-definitions
                                                            exclude-sources)
  (declare (ignore parser))
  (flet ((%stringify (value)
           (when value
             (if (stringp value)
                 value
                 (string-downcase value)))))
    (uiop:with-temporary-file (:pathname path :type "h")
      (write-uber-header headers path defines)
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
                         :entities (filter-library-entities
                                    (loop for value being the hash-value of *declaration-table*
                                          collect value)
                                    include-definitions
                                    include-sources
                                    exclude-definitions
                                    exclude-sources)
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
    (values (setf (gethash id *declaration-table*) (apply #'make-instance entity-class args)) t)))


(defun make-declaration-location (declaration)
  (let ((location (%resect:declaration-location declaration)))
    (make-instance 'foreign-location
                   :path (uiop:ensure-pathname (%resect:location-name location))
                   :line (%resect:location-line location)
                   :column (%resect:location-column location))))


(defun parse-declaration-by-kind (decl &optional from-type)
  (parse-declaration (%resect:declaration-kind decl) decl :from-type from-type))


(defun parse-type-by-category (type)
  (parse-type (%resect:type-category type) (%resect:type-kind type) type))


(defun parse-owner (decl)
  (let ((owner (%resect:declaration-owner decl)))
    (unless (cffi:null-pointer-p owner)
      (parse-declaration-by-kind owner))))


(defun unless-empty (seq)
  (unless (emptyp seq)
    seq))


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
                            :plain-old-data-type t
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
      (:wchar (register-primitive-type "wchar_t"))
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
(defmethod parse-declaration ((type (eql :enum)) decl &key)
  (let ((id (%resect:declaration-id decl))
        (decl-type (%resect:declaration-type decl))
        (value-alist))
    (resect:docollection (decl (%resect:enum-constants decl))
      (push (cons (%resect:declaration-name decl) (%resect:enum-constant-value decl)) value-alist))
    (register-entity 'foreign-enum
                     :id id
                     :owner (parse-owner decl)
                     :name (%resect:declaration-name decl)
                     :namespace (unless-empty
                                 (%resect:declaration-namespace decl))
                     :mangled (%resect:declaration-mangled-name decl)
                     :location (make-declaration-location decl)
                     :bit-size (%resect:type-alignment decl-type)
                     :bit-alignment (%resect:type-alignment decl-type)
                     :plain-old-data-type (%resect:type-plain-old-data-p decl-type)
                     :type (parse-type-by-category (%resect:enum-type decl))
                     :values (nreverse value-alist))))


(defmethod parse-type (category (kind (eql :enum)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


;;;
;;; TEMPLATE PARAMETER
;;;
(defmethod parse-declaration ((kind (eql :template-parameter)) decl &key (inject-arguments t))
  (let* ((name (%resect:declaration-name decl))
         (arg (search-for-template-argument name)))
    (if (and inject-arguments arg)
        arg
        (make-instance 'foreign-entity-parameter
                       :id (%resect:declaration-id decl)
                       :name name
                       :namespace (unless-empty
                                   (%resect:declaration-namespace decl))
                       :mangled (%resect:declaration-mangled-name decl)
                       :location (make-declaration-location decl)))))


(defmethod parse-type (category (kind (eql :template-parameter)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))

;;;
;;; RECORD
;;;
(defclass resect-record ()
  ((fields :initform nil)
   (method-postfix :initarg :method-postfix :initform nil :reader method-postfix-of)))


(defun push-field (record field)
  (with-slots (fields) record
    (push field fields)))


(defmethod foreign-record-fields ((this resect-record))
  (with-slots (fields) this
    fields))

(defclass resect-struct (foreign-struct resect-record) ())

(defclass resect-union (foreign-union resect-record) ())

(defclass resect-class (foreign-class resect-record) ())


(defun collect-entity-parameters (decl)
  (let (params)
    (resect:docollection (param-decl (%resect:declaration-template-parameters decl))
      (push (parse-declaration (%resect:declaration-kind param-decl) param-decl :inject-arguments nil)
            params))
    (nreverse params)))


(defun publicp (decl)
  (let ((access-specifier (%resect:declaration-access-specifier decl)))
    (or (eq :unknown access-specifier)
        (eq :public access-specifier))))


(defgeneric id-of (decl))
(defgeneric name-of (decl))
(defgeneric bit-size-of (decl))
(defgeneric bit-alignment-of (decl))
(defgeneric plain-old-data-p (decl))
(defgeneric entity-parameters-of (decl))
(defgeneric method-postfix-of (decl))


(defclass record-info ()
  ((id :initform nil :reader id-of)
   (name :initform nil :reader name-of)
   (type :initarg :type :reader handle-of)
   (argument-map :initform (make-hash-table :test 'equal))))


(defmethod initialize-instance :after ((this record-info) &key decl template-arguments argument-literals)
  (with-slots (argument-map) this
    (when template-arguments
      (let (template-parameters)
        (resect:docollection (param (%resect:declaration-template-parameters decl))
          (push param template-parameters))
        (nreversef template-parameters)
        (loop for arg in template-arguments
              for param in template-parameters
              for literal in argument-literals
              for param-name = (%resect:declaration-name param)
              do (let ((value (if (eq (%resect:template-argument-kind arg) :type)
                                  (parse-type-by-category
                                   (%resect:template-argument-type arg))
                                  literal)))
                   (setf (gethash param-name argument-map) value)))))))


(defmethod find-template-argument ((this record-info) name)
  (with-slots (argument-map) this
    (gethash name argument-map)))

(defmethod bit-size-of ((this record-info))
  (%resect:type-size (handle-of this)))

(defmethod bit-alignment-of ((this record-info))
  (%resect:type-alignment (handle-of this)))

(defmethod plain-old-data-p ((this record-info))
  (%resect:type-plain-old-data-p (handle-of this)))


(defclass instantiated-record-info (record-info)
  ((method-postfix :initform nil :reader method-postfix-of)))


(defun format-template-argument-string (argument-literals)
  (format nil "<~{~A~^,~}>" argument-literals))

(defmethod initialize-instance :after ((this instantiated-record-info) &key decl argument-literals)
  (with-slots (id type name method-postfix) this
    (let ((postfix (format-template-argument-string argument-literals)))
      (setf id (format nil "~A~A" (%resect:declaration-id decl) postfix)
            method-postfix postfix
            name (format nil "~A~A" (%resect:declaration-name decl) postfix)))))


(defmethod entity-parameters-of ((this instantiated-record-info))
  nil)


(defclass regular-record-info (record-info)
  ((entity-parameters :initform nil :reader entity-parameters-of)))


(defmethod initialize-instance :after ((this regular-record-info) &key decl)
  (with-slots (id name type entity-parameters) this
    (setf id (%resect:declaration-id decl)
          entity-parameters (collect-entity-parameters decl)
          name (%resect:declaration-name decl))))


(defmethod method-postfix-of ((this regular-record-info))
  nil)


(defun make-record-info (decl from-type)
  (let (template-args template-literals)
    (when from-type
      (resect:docollection (template-arg (%resect:type-template-arguments from-type))
        (push template-arg template-args))
      (setf template-literals (remove-if #'emptyp
                                         (split-template-argument-string-into-literals
                                          (extract-template-argument-string
                                           (%resect:type-name from-type)))))
      (unless (= (length template-args) (length template-literals))
        (setf template-literals (make-sequence 'list (length template-args) :initial-element nil )))
      (nreversef template-args))
    (if (and template-args (> (%resect:type-size from-type) 0))
        (make-instance 'instantiated-record-info
                       :decl decl
                       :type from-type
                       :template-arguments template-args
                       :argument-literals template-literals)
        (make-instance 'regular-record-info
                       :type (%resect:declaration-type decl)
                       :decl decl
                       :template-arguments template-args
                       :argument-literals template-literals))))


(defun instantiated-id (record-info)
  (when-let ((params (entity-parameters-of record-info)))
    (let* ((literals (loop for param in params
                           for arg = (search-for-template-argument
                                      (foreign-entity-name param))
                           when arg
                             collect (if (stringp arg)
                                         arg
                                         (format-full-foreign-entity-name arg)))))
      (when (= (length literals) (length params))
        (format nil "~A~A"
                (id-of record-info)
                (format-template-argument-string literals))))))


(defun format-method-postfix (name entity)
  (format nil "~A~@[~A~]" name (method-postfix-of entity)))


(defun parse-record-declaration (record-kind decl from-type)
  (let* ((record-info (make-record-info decl from-type))
         (*template-argument-map-list* (list* record-info *template-argument-map-list*)))
    (labels ((collect-parents ()
               (let (parents)
                 (resect:docollection (parent-decl (%resect:record-parents decl))
                   (push (parse-declaration-by-kind parent-decl) parents))
                 (nreverse parents)))
             (collect-fields (entity)
               (let (fields)
                 (resect:docollection (field-decl (%resect:record-fields decl))
                   (when (publicp field-decl)
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
                             fields))))
                 (loop for field in fields
                       do (push-field entity field)))))
      (multiple-value-bind (entity registeredp)
          (register-entity (ecase record-kind
                             (:struct 'resect-struct)
                             (:union 'resect-union)
                             (:class 'resect-class))
                           :id (or (instantiated-id record-info) (id-of record-info))
                           :owner (parse-owner decl)
                           :name (name-of record-info)
                           :namespace (unless-empty
                                       (%resect:declaration-namespace decl))
                           :mangled (%resect:declaration-mangled-name decl)
                           :location (make-declaration-location decl)
                           :bit-size (bit-size-of record-info)
                           :bit-alignment (bit-alignment-of record-info)
                           :plain-old-data-type (plain-old-data-p record-info)
                           :parents (collect-parents)
                           :abstract (%resect:record-abstract-p decl)
                           :entity-parameters (entity-parameters-of record-info)

                           :method-postfix (method-postfix-of record-info))
        (when registeredp
          (collect-fields entity)
          (let (destructor-found
                constructor-found
                pure-virtual-found)
            (resect:docollection (method-decl (%resect:record-methods decl))
              (let ((name (%resect:declaration-name method-decl)))
                (when (and (not destructor-found)
                           (starts-with #\~ name :test #'equal))
                  (setf destructor-found t))
                (when (and (not constructor-found)
                           (string= name (foreign-entity-name entity)))
                  (setf constructor-found t))
                (when (and (not pure-virtual-found)
                           (%resect:method-pure-virtual-p method-decl))
                  (setf pure-virtual-found t))))

            (resect:docollection (method-decl (%resect:record-methods decl))
              (let* ((pure-name (remove-template-argument-string (%resect:declaration-name method-decl)))
                     (constructor-p (string= pure-name (remove-template-argument-string
                                                        (foreign-entity-name entity))))
                     (name (if constructor-p
                               (format-method-postfix pure-name entity)
                               pure-name))
                     (params (parse-parameters (%resect:method-parameters method-decl))))
                (when (and (publicp method-decl)
                           (not (and constructor-p pure-virtual-found)))
                  (register-entity 'foreign-method
                                   :id (format-method-postfix
                                        (%resect:declaration-id method-decl)
                                        entity)
                                   :name name
                                   :owner entity
                                   :namespace (unless-empty
                                               (%resect:declaration-namespace decl))
                                   :mangled (format-method-postfix
                                             (ensure-mangled method-decl params)
                                             entity)
                                   :location (make-declaration-location method-decl)

                                   :result-type (parse-type-by-category (%resect:method-result-type method-decl))
                                   :parameters params
                                   :variadic (%resect:method-variadic-p method-decl)))))
            (let ((entity-id (claw.spec:foreign-entity-id entity))
                  (entity-name (claw.spec:foreign-entity-name entity))
                  (entity-namespace (claw.spec:foreign-entity-namespace entity)))
              (unless (or (claw.spec:foreign-record-abstract-p entity)
                          pure-virtual-found
                          (not entity-name)
                          (claw.spec:foreign-plain-old-data-type-p entity)
                          (zerop (claw.spec:foreign-entity-bit-size entity)))
                (unless constructor-found
                  (register-entity 'foreign-method
                                   :id (format nil "~A_claw_ctor" entity-id)
                                   :name entity-name
                                   :owner entity
                                   :namespace entity-namespace
                                   :mangled (format nil "~A_claw_ctor" entity-id)
                                   :location nil
                                   :result-type (register-void)
                                   :parameters nil
                                   :variadic nil))
                (unless destructor-found
                  (register-entity 'foreign-method
                                   :id (format nil "~A_claw_dtor" entity-id)
                                   :name (format nil "~~~A" (remove-template-argument-string entity-name))
                                   :owner entity
                                   :namespace entity-namespace
                                   :mangled (format nil "~A_claw_dtor" entity-id)
                                   :location nil
                                   :result-type (register-void)
                                   :parameters nil
                                   :variadic nil))))))
        (values entity registeredp)))))


(defmethod parse-declaration ((type (eql :struct)) decl &key from-type)
  (parse-record-declaration type decl from-type))


(defmethod parse-declaration ((type (eql :union)) decl &key from-type)
  (parse-record-declaration type decl from-type))


(defmethod parse-declaration ((type (eql :class)) decl &key from-type)
  (parse-record-declaration type decl from-type))


(defmethod parse-type (category (kind (eql :struct)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


(defmethod parse-type (category (kind (eql :record)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


(defmethod parse-type (category (kind (eql :class)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


;;;
;;; FUNCTION
;;;
(defun mangle-id (id)
  (labels ((encode-non-word (match &rest registers)
             (declare (ignore registers))
             (format nil "E~X" (char-code (aref match 0)))))
    (ppcre:regex-replace-all "\\W"
                             id
                             #'encode-non-word
                             :simple-calls t)))

(defun ensure-mangled (decl params)
  (let ((name (%resect:declaration-name decl))
        (mangled (unless-empty (%resect:declaration-mangled-name decl))))
    ;; hack to extract mangled name from parameter: libclang doesn't mangle some
    ;; names properly (extern "C++"?)
    (if (and (string= name mangled) params)
        (if-let ((param (find-if (complement #'null) params
                                 :key #'claw.spec:foreign-entity-mangled-name)))
          (let ((param-name (claw.spec:foreign-entity-name param))
                (param-mangled (claw.spec:foreign-entity-mangled-name param)))
            (subseq param-mangled 0 (- (length param-mangled) (length param-name))))
          (mangle-id (%resect:declaration-id decl)))
        (or mangled (mangle-id (%resect:declaration-id decl))))))


(defun parse-parameters (parameters)
  (let (params)
    (resect:docollection (param parameters)
      (let* ((name (%resect:declaration-name param))
             (param-type (%resect:declaration-type param)))
        (push (make-instance 'foreign-parameter
                             :name (unless-empty name)
                             :mangled (%resect:declaration-mangled-name param)
                             :location (make-declaration-location param)
                             :enveloped (parse-type-by-category param-type))

              params)))
    (nreverse params)))


(defmethod parse-declaration ((type (eql :function)) decl &key)
  (let ((id (%resect:declaration-id decl))
        (params (parse-parameters (%resect:function-parameters decl))))
    (register-entity 'foreign-function
                     :id id
                     :name (%resect:declaration-name decl)
                     :namespace (unless-empty
                                 (%resect:declaration-namespace decl))
                     :mangled (ensure-mangled decl params)
                     :location (make-declaration-location decl)
                     :result-type (parse-type-by-category (%resect:function-result-type decl))
                     :parameters params
                     :variadic (%resect:function-variadic-p decl)
                     :entity-parameters (collect-entity-parameters decl))))


(defmethod parse-type (category (kind (eql :function-prototype)) type)
  (declare (ignorable category kind))
  (let (params)
    (resect:docollection (param-type (%resect:function-proto-parameters type))
      (push (make-instance 'foreign-parameter
                           :enveloped (parse-type-by-category param-type))

            params))
    (make-instance 'foreign-function-prototype
                   :result-type (parse-type-by-category
                                 (%resect:function-proto-result-type type))
                   :parameters (nreverse params)
                   :variadic (%resect:function-proto-variadic-p type))))


;;;
;;; TYPEDEF
;;;
(defmethod parse-declaration ((kind (eql :typedef)) decl &key)
  (let ((id (%resect:declaration-id decl)))
    (register-entity 'foreign-alias
                     :id id
                     :owner (parse-owner decl)
                     :name (%resect:declaration-name decl)
                     :namespace (unless-empty
                                 (%resect:declaration-namespace decl))
                     :mangled (%resect:declaration-mangled-name decl)
                     :location (make-declaration-location decl)
                     :enveloped (parse-type-by-category (%resect:typedef-aliased-type decl)))))


(defmethod parse-type (category (kind (eql :typedef)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


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
  (let ((pointee-type (%resect:pointer-pointee-type type)))
    (make-instance 'foreign-pointer
                   :enveloped (if (member (cffi:pointer-address type) *parsed-pointers*
                                          :test #'=)
                                  (register-void)
                                  (let ((*parsed-pointers* (cons (cffi:pointer-address type)
                                                                 *parsed-pointers*)))
                                    (parse-type-by-category pointee-type))))))


;;;
;;; REFERENCE
;;;
(defmethod parse-type ((category (eql :reference)) kind type)
  (declare (ignorable category kind))
  (let ((pointee-type (%resect:reference-pointee-type type)))
    (make-instance 'foreign-reference
                   :enveloped (parse-type-by-category pointee-type))))

;;;
;;; VARIABLE
;;;
(defmethod parse-declaration ((kind (eql :variable)) declaration &key)
  (declare (ignore kind declaration))
  (make-instance 'foreign-variable :value 0))


;;;
;;; UNRECOGNIZED
;;;
(defclass unrecognized-entity (foreign-entity)
  ((name :initarg :name :reader foreign-entity-name)
   (kind :initarg :kind)))


(defmethod foreign-entity-unknown-p ((this unrecognized-entity))
  (declare (ignore this))
  t)

;;;
;;; UNRECOGNIZED TYPE
;;;
(defclass unrecognized-type (unrecognized-entity)
  ((category :initarg :category)
   (declaration :initarg :declaration)))


(defmethod print-object ((o unrecognized-type) s)
  (with-slots (name kind category) o
    (print-unreadable-object (o s :type t :identity nil)
      (format s "~A ~A ~A" category kind name))))


(defun make-unrecognized-type (type category kind)
  (let ((decl (%resect:type-declaration type)))
    (make-instance 'unrecognized-type :name (%resect:type-name type)
                                      :category category
                                      :kind kind
                                      :declaration (unless (cffi:null-pointer-p decl)
                                                     (parse-declaration-by-kind decl)))))


(defun notice-unrecognized-type (category kind type)
  (warn "Failed to recognize type of ~A kind from ~A category: ~A"
        kind category (%resect:type-name type))
  (make-unrecognized-type type category kind))


(defmethod parse-type (category kind type)
  (let* ((decl (%resect:type-declaration type))
         (entity (unless (cffi:null-pointer-p decl)
                   (parse-declaration-by-kind decl))))
    (if (or (not entity) (foreign-entity-unknown-p entity))
        (notice-unrecognized-type category kind type)
        entity)))


(defmethod parse-type ((category (eql :aux)) (kind (eql :dependent)) type)
  (notice-unrecognized-type category kind type))


;;;
;;; UNRECOGNIZED DECL
;;;
(defclass unrecognized-declaration (unrecognized-entity)
  ((location :initarg :location)))


(defmethod print-object ((o unrecognized-declaration) s)
  (with-slots (name kind location) o
    (print-unreadable-object (o s :type t :identity nil)
      (format s "~A ~A ~A" kind name location))))


(defun make-unrecognized-declaration (decl kind)
  (make-instance 'unrecognized-declaration
                 :name (%resect:declaration-name decl)
                 :location (make-declaration-location decl)
                 :kind kind))


(defmethod parse-declaration (kind declaration &key)
  (warn "Failed to recognize declaration of ~A kind: ~A" kind (%resect:declaration-name declaration))
  (make-unrecognized-declaration declaration kind))
