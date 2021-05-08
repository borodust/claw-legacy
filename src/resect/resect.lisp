(cl:in-package :claw.resect)


(declaim (special *declaration-table*
                  *instantiated-table*
                  *mangled-table*
                  *post-parse-hooks*))

(defvar *parsed-pointers* nil)
(defvar *current-owner* nil)

(defgeneric parse-declaration (kind declaration &key &allow-other-keys))


(defgeneric parse-type (category kind type))


(defun find-instantiated-type-from-owner (type-entity)
  (let* ((type-owner (foreign-owner type-entity))
         (instantiated (when (and *current-owner*
                                  type-owner
                                  (foreign-entity-parameters type-owner)
                                  (string= (format-full-foreign-entity-name type-owner)
                                           (remove-template-argument-string
                                            (format-full-foreign-entity-name *current-owner*))))
                         (loop for dependent in (dependents-of *current-owner*)
                               when (string= (foreign-entity-name type-entity)
                                             (foreign-entity-name dependent))
                                 return dependent))))
    (or instantiated type-entity)))


(defun const (entity)
  (make-instance 'claw.spec:foreign-const-qualifier :enveloped entity))


(defmethod parse-type :around (category kind type)
  (declare (ignorable category kind type))
  (let ((result (call-next-method)))
    (when (%resect:type-const-qualified-p type)
      (setf result (const result)))
    result))


(defun write-uber-header (headers path defines &optional text)
  (alexandria:with-output-to-file (out path :if-exists :supersede)
    (format out "#ifndef  __CLAW_UBERHEADER~%#define __CLAW_UBERHEADER 1~%")
    (loop for (name value) on defines by #'cddr
          do (format out "~%#define ~A~@[ ~A~]" name value))
    (loop for header in headers
          do (format out "~%#include \"~A\"" header))
    (when text
      (format out "~%")
      (format out "~A" text))
    (format out "~%~%~%#endif")))


(defclass foreign-library ()
  ((entities :initarg :entities
             :initform (error ":entities missing")
             :reader claw.wrapper:foreign-library-entities)
   (language :initarg :language
             :initform (error ":language missing")
             :reader claw.wrapper:foreign-library-language)))


(defun parse-declaration-by-kind (decl &optional from-type)
  (parse-declaration (%resect:declaration-kind decl) decl :from-type from-type))


(defun parse-type-by-category (type)
  (parse-type (%resect:type-category type) (%resect:type-kind type) type))


(defclass describing-inspector ()
  ((language :initform nil :reader language-of)))


(defmethod inspect-foreign-library :before ((this describing-inspector)
                                            header-path
                                            includes frameworks
                                            language standard target
                                            intrinsics
                                            &key)
  (declare (ignore header-path includes frameworks language standard target intrinsics))
  (with-slots (language) this
    (setf language (case (%resect:translation-unit-language *translation-unit*)
                     (:c :c)
                     (:c++ :c++)
                     (:obj-c :objective-c)))))


(defmethod inspect-foreign-library :around ((this describing-inspector)
                                            header-path
                                            includes frameworks
                                            language standard target
                                            intrinsics
                                            &key)
  (declare (ignore header-path includes frameworks language standard target intrinsics))
  (let (*post-parse-hooks*)
    (call-next-method)))


(defmethod inspect-foreign-library :after ((this describing-inspector)
                                           header-path
                                           includes frameworks
                                           language standard target
                                           intrinsics
                                           &key)
  (declare (ignore header-path includes frameworks language standard target intrinsics))
  (loop for hook in (reverse *post-parse-hooks*)
        do (funcall hook)))


(defmethod inspect-declaration ((this describing-inspector) kind declaration)
  (parse-declaration kind declaration))


(defmethod claw.wrapper:describe-foreign-library ((parser (eql :claw/resect))
                                                  headers &key
                                                            includes
                                                            frameworks
                                                            language
                                                            standard
                                                            target
                                                            defines
                                                            intrinsics
                                                            instantiation-filter

                                                            include-definitions
                                                            include-sources
                                                            exclude-definitions
                                                            exclude-sources)
  (declare (ignore parser))
  (with-temporary-directory (:pathname prepared-dir)
    (uiop:with-temporary-file (:pathname uber-path :type "h")
      (write-uber-header headers uber-path defines)
      (multiple-value-bind (prepared-headers macros)
          (prepare-foreign-library uber-path
                                   prepared-dir
                                   includes
                                   frameworks
                                   language
                                   standard
                                   target
                                   intrinsics
                                   instantiation-filter)
        (let ((*declaration-table* (make-hash-table :test 'equal))
              (*instantiated-table* (make-hash-table :test 'equal))
              (*mangled-table* (make-hash-table :test 'equal))
              (inspector (make-instance 'describing-inspector)))
          (loop for header in prepared-headers
                do (inspect-foreign-library inspector
                                            header
                                            includes
                                            frameworks
                                            language
                                            standard
                                            target
                                            intrinsics))
          (loop for constant in (prepare-macros-as-constants uber-path
                                                             includes
                                                             frameworks
                                                             target
                                                             macros
                                                             intrinsics

                                                             include-definitions
                                                             include-sources
                                                             exclude-definitions
                                                             exclude-sources)
                do (register-entity-instance constant))
          (make-instance 'foreign-library
                         :entities (filter-library-entities
                                    (loop for value being the hash-value of *declaration-table*
                                          collect value)
                                    include-definitions
                                    include-sources
                                    exclude-definitions
                                    exclude-sources)
                         :language (language-of inspector)))))))


(defmacro on-post-parse (&body body)
  `(push (lambda () ,@body) *post-parse-hooks*))


(defun instantiated-p (decl)
  (and (not (cffi:null-pointer-p (%resect:declaration-template decl)))
       (or (> (%resect:type-size (%resect:declaration-type decl)) 0)
           (%resect:declaration-forward-p decl))))


(defun derive-instantiated-id-from-type (type)
  (flet ((args-for-known ()
           (loop for arg in (extract-type-arguments type)
                 for parsed = (if (cffi:pointerp arg)
                                  (%resect:type-name arg)
                                  (eval-template-argument arg))
                 if (null parsed)
                   do (return nil)
                 else
                   collect parsed))
         (args-for-unknown ()
           (loop for arg in (extract-template-literals type)
                 for evaluated = (eval-template-argument arg)
                 for parsed = (if (cffi:pointerp evaluated)
                                  (%resect:type-name evaluated)
                                  evaluated)
                 if (null parsed)
                   do (return nil)
                 else
                   collect parsed)))
    (let ((args (if (eq :unknown (%resect:type-kind type))
                    (args-for-unknown)
                    (args-for-known)))
          (decl (%resect:type-declaration type)))
      (when (and args (not (cffi:null-pointer-p decl)))
        (format nil "~A~A"
                (%resect:declaration-id (root-template (%resect:type-declaration type)))
                (format-template-argument-string args))))))


(defun eval-instantiated-id-from-decl (decl)
  (let ((args (loop for param in (extract-decl-parameters decl)
                    for arg = (eval-template-argument (%resect:declaration-name param))
                    unless arg
                      return nil
                    collect (if (cffi:pointerp arg)
                                (%resect:type-name arg)
                                arg))))
    (when args
      (format nil "~A~A"
              (%resect:declaration-id (root-template decl))
              (format-template-argument-string args)))))


(defun instantiated-id (decl)
  (if (instantiated-p decl)
      (format nil "~A~A"
              (%resect:declaration-id (%resect:declaration-template decl))
              (reformat-template-argument-string-from-type (%resect:declaration-type decl)))
      (%resect:declaration-id decl)))


(defun register-instantiated (entity decl)
  (when (instantiated-p decl)
    (setf (gethash (instantiated-id decl) *instantiated-table*) entity)))


(defun find-instantiated (name)
  (gethash name *instantiated-table*))


(defun decorate-id-if-instantiated-owner (decl owner)
  (format nil "~A~@[<~A>~]"
          (%resect:declaration-id decl)
          (when (and owner
                     (foreign-entity-arguments owner)
                     (not (foreign-entity-parameters owner))
                     (> (foreign-entity-bit-size owner) 0))
            (format-full-foreign-entity-name owner))))


;;;
;;; UTIL
;;;
(defun find-entity (id)
  (gethash id *declaration-table*))


(defun register-entity (entity-class &rest args &key id &allow-other-keys)
  (if-let ((existing (find-entity id)))
    (values existing nil)
    (values (setf (gethash id *declaration-table*) (apply #'make-instance entity-class args)) t)))


(defun register-entity-instance (entity)
  (let ((id (foreign-entity-id entity)))
    (if-let ((existing (find-entity id)))
      (values existing nil)
      (values (setf (gethash id *declaration-table*) entity) t))))


(defun make-declaration-location (declaration)
  (let ((location (%resect:declaration-location declaration)))
    (make-instance 'foreign-location
                   :path (uiop:ensure-pathname (%resect:location-name location))
                   :line (%resect:location-line location)
                   :column (%resect:location-column location))))


(defun parse-owner (decl)
  (let ((owner (%resect:declaration-owner decl)))
    (unless (cffi:null-pointer-p owner)
      (parse-declaration-by-kind owner))))


(defun specializationp (type)
  (resect:docollection (template-arg (%resect:type-template-arguments type))
    (declare (ignore template-arg))
    (return-from specializationp t)))


(defun reformat-template-argument-string-from-type (type)
  (let ((args (loop for arg in (extract-type-arguments type)
                    collect (if (cffi:pointerp arg)
                                (let ((*tag-types* nil))
                                  (format-foreign-entity-c-name (parse-type-by-category arg)))
                                arg))))
    (format-template-argument-string args)))

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
      (:char-u (register-primitive-type "char"))
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
      (:float16 (register-primitive-type "float16"))
      (:auto (register-primitive-type "auto"))
      (:atomic (register-primitive-type "atomic")))))


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
  (let* ((decl-type (%resect:declaration-type decl))
         (owner (parse-owner decl))
         (value-alist))
    (resect:docollection (decl (%resect:enum-constants decl))
      (push (cons (%resect:declaration-name decl) (%resect:enum-constant-value decl)) value-alist))
    (multiple-value-bind (entity registeredp)
        (register-entity 'foreign-enum
                         :id (decorate-id-if-instantiated-owner decl owner)
                         :source (%resect:declaration-source decl)
                         :owner owner
                         :name (%resect:declaration-name decl)
                         :namespace (unless-empty
                                     (%resect:declaration-namespace decl))
                         :mangled (%resect:declaration-mangled-name decl)
                         :location (make-declaration-location decl)
                         :bit-size (%resect:type-alignment decl-type)
                         :bit-alignment (%resect:type-alignment decl-type)
                         :plain-old-data-type (%resect:type-plain-old-data-p decl-type)
                         :type (parse-type-by-category (%resect:enum-type decl))
                         :values (nreverse value-alist))
      (when (and registeredp owner)
        (add-dependent owner entity))
      (find-instantiated-type-from-owner entity))))


(defmethod parse-type (category (kind (eql :enum)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


;;;
;;; TEMPLATE PARAMETER
;;;
(defmethod parse-declaration ((kind (eql :template-parameter)) decl &key (inject-arguments t))
  (let* ((name (%resect:declaration-name decl))
         (arg (eval-template-argument name)))
    (if (and inject-arguments arg (cffi:pointerp arg))
        (parse-type-by-category arg)
        (let ((description (list :id (%resect:declaration-id decl)
                                 :source (%resect:declaration-source decl)
                                 :name name
                                 :namespace (unless-empty
                                             (%resect:declaration-namespace decl))
                                 :mangled (%resect:declaration-mangled-name decl)
                                 :location (make-declaration-location decl))))
          (apply #'make-instance
                 (if (eq :non-type (%resect:template-parameter-kind decl))
                     (list* 'foreign-entity-value-parameter
                            :type (parse-type-by-category (%resect:declaration-type decl))
                            description)
                     (list* 'foreign-entity-type-parameter description)))))))


(defmethod parse-type (category (kind (eql :template-parameter)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))

;;;
;;; RECORD
;;;
(defclass resect-record ()
  ((fields :initform nil :accessor fields-of)
   (args :initform nil :accessor arguments-of)
   (params :initform nil :accessor parameters-of)
   (deps :initform nil :accessor dependents-of)
   (parents :initform nil :accessor parents-of)))

(defmethod foreign-record-fields ((this resect-record))
  (slot-value this 'fields))

(defmethod foreign-entity-arguments ((this resect-record))
  (slot-value this 'args))

(defmethod foreign-entity-parameters ((this resect-record))
  (slot-value this 'params))

(defmethod foreign-record-parents ((this resect-record))
  (slot-value this 'parents))

(defmethod foreign-dependent ((this resect-record))
  (slot-value this 'deps))

(defgeneric add-field (record field)
  (:method ((this resect-record) field)
    (with-slots (fields) this
      (unless (member (foreign-entity-name field) fields :key #'foreign-entity-name)
        (nconcf fields (list field))))))

(defgeneric add-dependent (record dependent)
  (:method ((this resect-record) dependent)
    (with-slots (deps) this
      (push dependent deps))))

(defclass resect-struct (resect-record foreign-struct) ())

(defclass resect-union (resect-record foreign-union) ())

(defclass resect-class (resect-record foreign-class) ())


(defun collect-entity-parameters (decl)
  (let (params)
    (resect:docollection (param-decl (%resect:declaration-template-parameters decl))
      (push (parse-declaration (%resect:declaration-kind param-decl) param-decl :inject-arguments nil)
            params))
    (nreverse params)))


(defun %collect-entity-arguments (args params)
  (loop for param in params
        for arg in args
        collect (make-instance 'claw.spec:foreign-entity-argument
                               :parameter param
                               :value (if (cffi:pointerp arg)
                                          (parse-type-by-category arg)
                                          (cond
                                            ((numberp arg) arg)
                                            ((string= "true" arg) t)
                                            ((string= "false" arg) nil)
                                            (t (handler-case
                                                   (parse-number:parse-number arg)
                                                 (t () arg))))))))


(defun collect-record-entity-arguments (decl)
  (when (instantiated-p decl)
    (let ((args (extract-type-arguments (%resect:declaration-type decl)))
          (params (foreign-entity-parameters
                   (parse-declaration-by-kind (root-template decl)))))
      (%collect-entity-arguments args params))))


(defun extract-function-template-arguments (decl)
  (let (template-arguments)
    ;; template-arguments
    (resect:docollection (arg (%resect:declaration-template-arguments decl))
      (push arg template-arguments))
    (nreversef template-arguments)

    (let ((values (loop for arg in template-arguments
                        for value = (ecase (%resect:template-argument-kind arg)
                                      ((:type :declaration :template :template-expansion)
                                       (%resect:type-name (%resect:template-argument-type arg)))
                                      ((:integral :expression :pack)
                                       (%resect:template-argument-value arg)))
                        collect value))
          (parent (%resect:declaration-template decl)))
      (if (cffi:null-pointer-p parent)
          values
          (append values (extract-function-template-arguments parent))))))


(defun collect-function-entity-arguments (decl)
  (unless (cffi:null-pointer-p (%resect:declaration-template decl))
    (let ((args (extract-function-template-arguments decl))
          (params (foreign-entity-parameters
                   (parse-declaration-by-kind (root-template decl)))))
      (%collect-entity-arguments args params))))


(defun collect-entity-arguments (decl)
  (if (eq :function (%resect:declaration-kind decl))
      (collect-function-entity-arguments decl)
      (collect-record-entity-arguments decl)))


(defun nested-pointer-p (type)
  (flet ((%any-pointer-p (type)
           (member (%resect:type-kind type) '(:pointer :rvalue-reference :lvalue-reference))))
    (or (and (eq (%resect:type-kind type) :pointer)
             (%any-pointer-p (%resect:pointer-pointee-type type)))
        (and (member (%resect:type-kind type) '(:rvalue-reference :lvalue-reference))
             (%any-pointer-p (%resect:reference-pointee-type type))))))

(defun ensure-const-type-if-needed (type entity &optional decl)
  (cond
    ((or (typep entity 'foreign-const-qualifier)
         (typep (unwrap-foreign-entity entity) 'foreign-function-prototype))
     entity)
    ((and (not (nested-pointer-p type))
          (or
           ;; crazy, maybe a bug in libclang
           (starts-with-subseq "const " (%resect:type-name type))
           ;; i don't like this at all
           ;; better to use lexer in resect
           (and decl (starts-with-subseq "const " (%resect:declaration-source decl)))))
     (if (foreign-envelope-p entity)
         (make-instance (class-of entity)
                        :enveloped (const (claw.spec:foreign-enveloped-entity entity)))
         (const entity)))
    (t entity)))


(defun decorate-if-instantiated-record (decl)
  (let ((name (%resect:declaration-name decl)))
    (format nil "~A~@[~A~]"
            name
            (when (and (not (emptyp name))
                       (instantiated-p decl))
              (reformat-template-argument-string-from-type (%resect:declaration-type decl))))))


(defun register-default-methods (entity
                                 constructor-required
                                 destructor-required)
  (let ((entity-id (claw.spec:foreign-entity-id entity))
        (entity-name (claw.spec:foreign-entity-name entity))
        (entity-namespace (claw.spec:foreign-entity-namespace entity)))
    (when constructor-required
      (register-entity 'foreign-method
                       :id (format nil "~A_claw_ctor" entity-id)
                       :name entity-name
                       :owner entity
                       :namespace entity-namespace
                       :mangled (format nil "~A_claw_ctor" entity-id)
                       :location (foreign-entity-location entity)
                       :result-type (register-void)
                       :parameters nil
                       :variadic nil))
    (when destructor-required
      (register-entity 'foreign-method
                       :id (format nil "~A_claw_dtor" entity-id)
                       :name (format nil "~~~A" (remove-template-argument-string entity-name))
                       :owner entity
                       :namespace entity-namespace
                       :mangled (format nil "~A_claw_dtor" entity-id)
                       :location (foreign-entity-location entity)
                       :result-type (register-void)
                       :parameters nil
                       :variadic nil))))


(defun postfix-decorate (name postfix)
  (format nil "~A~@[~A~]" name postfix))


(defun parse-methods (entity record-decl &optional postfix)
  (let ((*current-owner* entity)
        destructor-found
        constructor-found
        pure-virtual-found)
    (resect:docollection (method-decl (%resect:record-methods record-decl))
      (when (and (not pure-virtual-found)
                 (%resect:method-pure-virtual-p method-decl))
        (setf pure-virtual-found t)))
    (resect:docollection (method-decl (%resect:record-methods record-decl))
      (let* ((deleted-p (search "= delete" (%resect:declaration-source method-decl)))
             (pure-method-name (remove-template-argument-string
                                (%resect:declaration-name method-decl)))
             (pure-entity-name (remove-template-argument-string
                                (foreign-entity-name entity)))
             (constructor-p (string= pure-method-name pure-entity-name))
             (destructor-p (starts-with #\~ pure-method-name)))
        (when (and constructor-p
                   (not constructor-found))
          (setf constructor-found t))
        (when (and destructor-p
                   (not destructor-found))
          (setf destructor-found t))
        (when (and (publicp method-decl)
                   (not (and pure-virtual-found (or destructor-p constructor-p)))
                   (not deleted-p))
          (let* ((result-type (ensure-const-type-if-needed
                               (%resect:method-result-type method-decl)
                               (parse-type-by-category
                                (%resect:method-result-type method-decl))))
                 (name (cond
                         (constructor-p (foreign-entity-name entity))
                         ((and (starts-with-subseq "operator " pure-method-name)
                               (foreign-named-p result-type)
                               (string= (format nil "operator ~A"
                                                (remove-template-argument-string
                                                 (foreign-entity-name result-type)))
                                        pure-method-name))
                          (format nil "operator ~A" (foreign-entity-name result-type)))
                         (t (%resect:declaration-name method-decl))))
                 (params (parse-parameters (%resect:method-parameters method-decl)))
                 (mangled-name (postfix-decorate (ensure-mangled method-decl) postfix)))
            (multiple-value-bind (method newp)
                (register-entity 'foreign-method
                                 :id (postfix-decorate (%resect:declaration-id method-decl)
                                                       postfix)
                                 :source (%resect:declaration-source method-decl)
                                 :name name
                                 :owner entity
                                 :namespace (unless-empty
                                             (%resect:declaration-namespace method-decl))
                                 :mangled mangled-name
                                 :location (make-declaration-location method-decl)

                                 :result-type result-type
                                 :parameters params
                                 :variadic (%resect:method-variadic-p method-decl)
                                 :static (eq :static (%resect:method-storage-class method-decl))
                                 :const (%resect:method-const-p method-decl))
              (when newp
                (setf (gethash mangled-name *mangled-table*) method)))))))
    (unless (or (claw.spec:foreign-record-abstract-p entity)
                pure-virtual-found
                (not (claw.spec:foreign-entity-name entity))
                (zerop (claw.spec:foreign-entity-bit-size entity)))
      (register-default-methods entity (not constructor-found) (not destructor-found)))))


(defun method-exists-p (decl)
  (resect:docollection (method-decl (%resect:record-methods decl))
    (declare (ignore method-decl))
    (return-from method-exists-p t)))


(defun ensure-inherited-fields (entity)
  (let* ((owner (foreign-owner entity))
         (fields (fields-of entity))
         (owner-has-field (when owner
                            (loop for owner-field in (foreign-record-fields owner)
                                  for unwrapped-type = (unwrap-foreign-entity owner-field)
                                    thereis (and (foreign-identified-p unwrapped-type)
                                                 (string= (foreign-entity-id unwrapped-type)
                                                          (foreign-entity-id entity)))))))
    (when (and (not (foreign-entity-name entity))
               (not owner-has-field)
               fields
               (typep owner 'resect-record))
      (loop for field in fields
            do (add-field owner field))
      ;; propagate further
      (ensure-inherited-fields owner))))


(defun parse-fields (entity decl)
  (let ((*current-owner* entity)
        fields)
    (resect:docollection (field-decl (%resect:type-fields (%resect:declaration-type decl)))
      (when (publicp field-decl)
        (let ((field-type (%resect:declaration-type field-decl)))
          (push (make-instance 'foreign-record-field
                               :name (%resect:declaration-name field-decl)
                               :location (make-declaration-location field-decl)
                               :enveloped (ensure-const-type-if-needed
                                           field-type
                                           (parse-type-by-category field-type)
                                           field-decl)
                               :bit-size (%resect:type-size field-type)
                               :bit-alignment (%resect:type-alignment field-type)
                               :bit-offset (%resect:field-offset field-decl)
                               :bitfield-p (%resect:field-bitfield-p field-decl)
                               :bit-width (%resect:field-width field-decl))
                fields))))
    (setf (fields-of entity) (nreverse fields))
    (ensure-inherited-fields entity)))


(defun parse-new-record-declaration (record-kind decl)
  (labels ((collect-parents ()
             (let (parents)
               (resect:docollection (parent-type (%resect:record-parents decl))
                 (push (parse-type-by-category parent-type) parents))
               (nreverse parents))))
    (let ((owner (parse-owner decl)))
      (multiple-value-bind (entity registeredp)
          (let ((decl-type (%resect:declaration-type decl)))
            (register-entity (ecase record-kind
                               (:struct 'resect-struct)
                               (:union 'resect-union)
                               (:class 'resect-class))
                             :id (%resect:declaration-id decl)
                             :source (%resect:declaration-source decl)
                             :owner owner
                             :name (decorate-if-instantiated-record decl)
                             :namespace (unless-empty
                                         (%resect:declaration-namespace decl))
                             :mangled (%resect:declaration-mangled-name decl)
                             :location (make-declaration-location decl)
                             :bit-size (%resect:type-size decl-type)
                             :bit-alignment (%resect:type-alignment decl-type)
                             :plain-old-data-type (%resect:type-plain-old-data-p decl-type)
                             :abstract (%resect:record-abstract-p decl)
                             :private (or (foreign-entity-private-p owner)
                                          (not (publicp decl))
                                          (not (template-arguments-public-p decl)))
                             :forward (%resect:declaration-forward-p decl)))
        (when registeredp
          (when owner
            (add-dependent owner entity))
          (setf (parents-of entity) (collect-parents)
                (arguments-of entity) (collect-entity-arguments decl)
                (parameters-of entity) (collect-entity-parameters decl))
          (unless (foreign-entity-private-p entity)
            (unless (zerop (foreign-entity-bit-size entity))
              (on-post-parse
                (parse-fields entity decl)))
            (register-instantiated entity decl)
            (on-post-parse
              (parse-methods entity decl))))
        (find-instantiated-type-from-owner entity)))))


(defun find-instantiated-from-type (from-type)
  (and from-type (find-instantiated (derive-instantiated-id-from-type from-type))))


(defun find-instantiated-from-decl (decl)
  (find-instantiated (eval-instantiated-id-from-decl decl)))


(defun parse-record-declaration (record-kind decl from-type)
  (if-let ((instantiated (or (find-instantiated-from-type from-type)
                             (find-instantiated-from-decl decl))))
    instantiated
    (parse-new-record-declaration record-kind decl)))


(defmethod parse-declaration ((type (eql :struct)) decl &key from-type)
  (parse-record-declaration type decl from-type))


(defmethod parse-declaration ((type (eql :union)) decl &key from-type)
  (parse-record-declaration type decl from-type))


(defmethod parse-declaration ((type (eql :class)) decl &key from-type)
  (parse-record-declaration type decl from-type))


(defmethod parse-type (category (kind (eql :struct)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


(defmethod parse-type (category (kind (eql :union)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


(defmethod parse-type (category (kind (eql :class)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


;;;
;;; FUNCTION
;;;
(defun parse-parameters (parameters)
  (let (params
        parsed-param-names
        (repeat-idx 0))
    (resect:docollection (param parameters)
      (let ((name (unless-empty (%resect:declaration-name param)))
            (param-type (%resect:declaration-type param)))
        (when name
          (when (member name parsed-param-names :test #'string=)
            (setf name (format nil "~A~A" name (incf repeat-idx))))
          (push name parsed-param-names))
        (push (make-instance 'foreign-parameter
                             :name name
                             :mangled (%resect:declaration-mangled-name param)
                             :location (make-declaration-location param)
                             :enveloped (ensure-const-type-if-needed
                                         param-type
                                         (parse-type-by-category param-type)))

              params)))
    (nreverse params)))


(defun parse-result-type (decl)
  (let ((type (if (eq :function (%resect:declaration-kind decl))
                  (%resect:function-result-type decl)
                  (%resect:method-result-type decl))))
    (ensure-const-type-if-needed
     type
     (parse-type-by-category type))))


(defun register-function (decl)
  (let ((id (%resect:declaration-id decl))
        (name (%resect:declaration-name decl))
        (mangled-name (ensure-mangled decl))
        (params (parse-parameters (%resect:function-parameters decl))))
    (multiple-value-bind (entity newp)
        (register-entity 'foreign-function
                         :id id
                         :source (%resect:declaration-source decl)
                         :name name
                         :namespace (unless-empty
                                     (%resect:declaration-namespace decl))
                         :mangled mangled-name
                         :location (make-declaration-location decl)
                         :result-type (parse-result-type decl)
                         :parameters params
                         :variadic (%resect:function-variadic-p decl)
                         :inlined (%resect:function-inlined-p decl)
                         :entity-parameters (collect-entity-parameters decl)
                         :entity-arguments (collect-entity-arguments decl))
      (when newp
        (setf (gethash mangled-name *mangled-table*) entity))
      entity)))


(defun register-instantiated-function (template decl)
  (let ((params (parse-parameters (%resect:function-parameters decl))))
    (if (typep template 'foreign-method)
        (register-entity 'foreign-method
                         :id (%resect:declaration-id decl)
                         :name (foreign-entity-name template)
                         :namespace (foreign-entity-namespace template)
                         :source (foreign-entity-source template)
                         :mangled (ensure-mangled decl)
                         :location (foreign-entity-location template)
                         :result-type (parse-result-type decl)
                         :parameters (rest params)
                         :owner (foreign-enveloped-entity (first params))
                         :variadic (foreign-function-variadic-p template)
                         :entity-parameters nil
                         :entity-arguments nil)
        (register-entity 'foreign-function
                         :id (%resect:declaration-id decl)
                         :name (foreign-entity-name template)
                         :namespace (foreign-entity-namespace template)
                         :source (foreign-entity-source template)
                         :mangled (ensure-mangled decl)
                         :location (foreign-entity-location template)
                         :result-type (parse-result-type decl)
                         :parameters params
                         :variadic (foreign-function-variadic-p template)
                         :entity-parameters nil
                         :entity-arguments nil))))


(defmethod parse-declaration ((type (eql :function)) decl &key)
  (if (starts-with-subseq +instantiation-prefix+ (%resect:declaration-name decl))
      (on-post-parse
        (let ((template-mangled-name (subseq (%resect:declaration-name decl)
                                             (length +instantiation-prefix+))))
          (if-let ((template (gethash template-mangled-name *mangled-table*)))
            (register-instantiated-function template decl)
            (warn "Template with mangled name ~A not found" template-mangled-name))))
      (register-function decl)))


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
  (let* ((owner (parse-owner decl))
         (id (decorate-id-if-instantiated-owner decl owner))
         (enveloped (let ((aliased-type (%resect:typedef-aliased-type decl)))
                      (if-let ((instantiated (find-instantiated-from-type aliased-type)))
                        instantiated
                        (parse-type-by-category aliased-type)))))
    (multiple-value-bind (entity registeredp)
        (register-entity 'foreign-alias
                         :id id
                         :owner owner
                         :name (%resect:declaration-name decl)
                         :namespace (unless-empty
                                     (%resect:declaration-namespace decl))
                         :mangled (%resect:declaration-mangled-name decl)
                         :location (make-declaration-location decl)
                         :enveloped enveloped)
      (when (and registeredp owner)
        (add-dependent owner entity))
      (find-instantiated-type-from-owner entity))))


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
                   :enveloped (parse-type-by-category pointee-type)
                   :rvalue (not (%resect:reference-lvalue-p type)))))

;;;
;;; VARIABLE
;;;
(defmethod parse-declaration ((kind (eql :variable)) declaration &key)
  (let ((type (parse-type-by-category (%resect:variable-type declaration)))
        (name (%resect:declaration-name declaration)))
    (unless (starts-with-subseq +instantiation-prefix+ name)
      (let ((value (case (%resect:variable-kind declaration)
                     (:int (%resect:variable-to-int declaration))
                     (:float (%resect:variable-to-float declaration))
                     (:string (%resect:variable-to-string declaration))
                     (t nil))))
        (if (%resect:type-const-qualified-p (%resect:variable-type declaration))
            (register-entity 'foreign-constant
                             :id name
                             :name name
                             :namespace (unless-empty
                                         (%resect:declaration-namespace declaration))
                             :source (%resect:declaration-source declaration)
                             :value value)
            (register-entity 'foreign-variable
                             :id (%resect:declaration-id declaration)
                             :name name
                             :namespace (unless-empty
                                         (%resect:declaration-namespace declaration))
                             :source (%resect:declaration-source declaration)
                             :value value
                             :type type
                             :external (eq :external (%resect:declaration-linkage declaration))))))))


;;;
;;; MACRO
;;;
;;; Macros are parsed as auto variables
;;;
(defmethod parse-declaration ((kind (eql :macro)) declaration &key)
  (declare (ignore kind declaration)))


;;;
;;; UNRECOGNIZED
;;;
(defclass unrecognized-entity (foreign-entity)
  ((name :initarg :name :reader foreign-entity-name)
   (kind :initarg :kind)))


(defmethod foreign-entity-unknown-p ((this unrecognized-entity))
  (declare (ignore this))
  t)

(defmethod format-foreign-entity-c-name ((this unrecognized-entity) &key &allow-other-keys)
  (foreign-entity-name this))

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
                                      :source (unless (cffi:null-pointer-p decl)
                                                (%resect:declaration-source decl))
                                      :declaration (unless (cffi:null-pointer-p decl)
                                                     (parse-declaration-by-kind decl)))))


(defun notice-unrecognized-type (category kind type)
  (warn "Failed to recognize type of ~A kind from ~A category: ~A"
        kind category (%resect:type-name type))
  (make-unrecognized-type type category kind))


(defmethod parse-type (category kind type)
  (let* ((decl (%resect:type-declaration type))
         (entity (unless (cffi:null-pointer-p decl)
                   (parse-declaration-by-kind decl type))))
    (if (or (not entity) (foreign-entity-unknown-p entity))
        (if (and (eq category :unknown)
                 (eq kind :unknown))
            (make-unrecognized-type type category kind)
            (notice-unrecognized-type category kind type))
        entity)))


(defmethod parse-type ((category (eql :aux)) (kind (eql :dependent)) type)
  (make-unrecognized-type type category kind))


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
                 :source (%resect:declaration-source decl)
                 :kind kind))


(defmethod parse-declaration ((kind (eql :unknown)) declaration &key)
  (make-unrecognized-declaration declaration kind))


(defmethod parse-declaration (kind declaration &key)
  (warn "Failed to recognize declaration of ~A kind: ~A" kind (%resect:declaration-name declaration))
  (make-unrecognized-declaration declaration kind))
