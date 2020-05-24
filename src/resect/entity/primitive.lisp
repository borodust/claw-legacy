(cl:in-package :claw.spec)

(define-constant +signed-types+ '(:char :short :int :long :long-long)
  :test #'equal)


(defclass foreign-primitive (aligned foreign-entity) ())


(defmethod primitivep ((this foreign-primitive))
  (declare (ignore this))
  t)


(defun %to-primitive-name (name)
  (ppcre:regex-replace-all "\\s+" name "-"))


(defun register-primitive-type (tag location bit-size bit-alignment)
  (foreign-entity-type
   (if-let ((entity (find-foreign-entity tag)))
     entity
     (register-foreign-entity tag
                              (make-instance 'foreign-primitive
                                             :name tag
                                             :type tag
                                             :location location
                                             :bit-size bit-size
                                             :bit-alignment bit-alignment)))))

(defun %trim-primitive-tag (tag)
  (if (and (> (length tag) 0) (equal (aref tag 0) #\:))
      (subseq tag 1)
      tag))

(defun %%register-primitive-type (form)
  (alist-bind (tag location bit-size bit-alignment) form
    (register-primitive-type (%trim-primitive-tag tag) location bit-size bit-alignment)))


(defun %%register-primitive-type-renaming (form new-tag)
  (alist-bind (location bit-size bit-alignment) form
    (register-primitive-type (%trim-primitive-tag new-tag)
                             location
                             bit-size
                             bit-alignment)))


(defmethod compose-form ((this foreign-primitive))
  (alist :tag (%to-primitive-name (foreign-entity-name this))
         :bit-size (foreign-entity-bit-size this)
         :bit-alignment (foreign-entity-bit-alignment this)))


(defmethod compose-entity-reference ((this foreign-primitive))
  (alist :tag (format nil ":~A" (%to-primitive-name (foreign-entity-name this)))))


(defmethod parse-form (form (tag (eql :char)))
  (%%register-primitive-type form))


(defmethod parse-form (form (tag (eql :signed-char)))
  (%%register-primitive-type-renaming form ":char"))


(defmethod parse-form (form (tag (eql :_bool)))
  (alist-bind (bit-size) form
    (eswitch (bit-size :test #'=)
      (8 (%%register-primitive-type-renaming form ":char"))
      (32 (%%register-primitive-type-renaming form ":int")))))


(defmethod parse-form (form (tag (eql :void)))
  (declare (ignore form))
  (register-primitive-type "void" nil nil nil))


(defmethod parse-form (form (tag (eql :unsigned-char)))
  (%%register-primitive-type-renaming form ":unsigned char"))


(defmethod parse-form (form (tag (eql :short)))
  (%%register-primitive-type form))


(defmethod parse-form (form (tag (eql :unsigned-short)))
  (%%register-primitive-type-renaming form ":unsigned short"))


(defmethod parse-form (form (tag (eql :int)))
  (%%register-primitive-type form))


(defmethod parse-form (form (tag (eql :unsigned-int)))
  (%%register-primitive-type-renaming form ":unsigned int"))


(defmethod parse-form (form (tag (eql :long)))
  (%%register-primitive-type form))


(defmethod parse-form (form (tag (eql :unsigned-long)))
  (%%register-primitive-type-renaming form ":unsigned long"))


(defmethod parse-form (form (tag (eql :long-long)))
  (%%register-primitive-type-renaming form ":long long"))


(defmethod parse-form (form (tag (eql :unsigned-long-long)))
  (%%register-primitive-type-renaming form ":unsigned long long"))


(defmethod parse-form (form (tag (eql :float)))
  (%%register-primitive-type form))


(defmethod parse-form (form (tag (eql :double)))
  (%%register-primitive-type form))


(defmethod parse-form (form (tag (eql :int128)))
  (%%register-primitive-type form))


(defmethod parse-form (form (tag (eql :uint128)))
  (%%register-primitive-type form))


(defmethod parse-form (form (tag (eql :long-double)))
  (%%register-primitive-type-renaming form ":long double"))


(defmethod parse-form (form (tag (eql :__float128)))
  (%%register-primitive-type-renaming form ":long double"))


(defmethod parse-form (form (tag (eql :unsigned-__int128)))
  (%%register-primitive-type-renaming form ":uint128"))


(defmethod parse-form (form (tag (eql :__int128)))
  (%%register-primitive-type-renaming form ":int128"))


(defmethod parse-form (form (tag (eql :__builtin_va_list)))
  (%%register-primitive-type-renaming form ":va_list"))


(defmethod parse-form (form (tag (eql :va_list)))
  (%%register-primitive-type-renaming form ":va_list"))


(defmethod parse-form (form (tag (eql :wchar_t)))
  (%%register-primitive-type-renaming form ":wchar-t"))


(defmethod parse-form (form (tag (eql :char8_t)))
  (%%register-primitive-type-renaming form ":char8-t"))


(defmethod parse-form (form (tag (eql :char16_t)))
  (%%register-primitive-type-renaming form ":char16-t"))


(defmethod parse-form (form (tag (eql :char32_t)))
  (%%register-primitive-type-renaming form ":char32-t"))


(defmethod parse-form (form (tag (eql :__locale_t)))
  (declare (ignore tag form))
  ;; just skip it
  )


;;;
;;; CONSTANTS
;;;
(defclass foreign-constant (foreign-entity)
  ((value :initform (error ":value missing") :initarg :value
          :reader foreign-constant-value)))


(defmethod entity-constant-p ((this foreign-constant))
  (declare (ignore this))
  t)


(defmethod parse-form (form (tag (eql :const)))
  (alist-bind (name value location) form
    (register-foreign-entity name
                             (make-instance 'foreign-constant :name name
                                                              :value value
                                                              :type name
                                                              :location location))))


(defmethod compose-form ((this foreign-constant))
  (alist :tag ":const"
         :name (foreign-entity-name this)
         :location (foreign-entity-location this)
         :value (foreign-constant-value this)))


(defmethod optimize-entity ((entity foreign-primitive))
  (when (marked-included-p (foreign-entity-type entity))
    entity))

;;;
;;; RESECT
;;;
(defun register-primitive-resect-type (kind type)
  (flet ((register-primitive-type (tag)
           (register-primitive-type tag nil
                                    (* 8 (%resect:type-size type))
                                    (* 8 (%resect:type-alignment type)))))
    (ecase kind
      (:void (register-primitive-type "void"))
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
