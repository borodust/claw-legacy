(cl:in-package :claw.spec)

(define-constant +signed-types+ '(:char :short :int :long :long-long)
  :test #'equal)


(defclass foreign-primitive (aligned foreign-entity) ())


(defmethod primitivep ((this foreign-primitive))
  (declare (ignore this))
  t)


(defun %to-primitive-name (name)
  (ppcre:regex-replace-all "\\s+" name "-"))


(defun %register-primitive-type (tag location bit-size bit-alignment)
  (let ((tag (if (equal (aref tag 0) #\:)
                 (subseq tag 1)
                 tag)))
    (foreign-entity-type
     (if-let ((entity (find-foreign-entity tag)))
       entity
       (register-foreign-entity tag
                                (make-instance 'foreign-primitive
                                               :name tag
                                               :type tag
                                               :location location
                                               :bit-size bit-size
                                               :bit-alignment bit-alignment))))))

(defun register-primitive-type (form)
  (alist-bind (tag location bit-size bit-alignment) form
    (%register-primitive-type tag location bit-size bit-alignment)))


(defun register-primitive-type-renaming (form new-tag)
  (alist-bind (location bit-size bit-alignment) form
    (%register-primitive-type new-tag location bit-size bit-alignment)))


(defmethod compose-form ((this foreign-primitive))
  (alist :tag (%to-primitive-name (foreign-entity-name this))
         :bit-size (foreign-entity-bit-size this)
         :bit-alignment (foreign-entity-bit-alignment this)))


(defmethod compose-entity-reference ((this foreign-primitive))
  (alist :tag (format nil ":~A" (%to-primitive-name (foreign-entity-name this)))))


(defmethod parse-form (form (tag (eql :char)))
  (register-primitive-type form))


(defmethod parse-form (form (tag (eql :signed-char)))
  (register-primitive-type-renaming form ":char"))


(defmethod parse-form (form (tag (eql :_bool)))
  (register-primitive-type-renaming form ":bool"))


(defmethod parse-form (form (tag (eql :void)))
  (declare (ignore form))
  (%register-primitive-type ":void" nil nil nil))


(defmethod parse-form (form (tag (eql :unsigned-char)))
  (register-primitive-type-renaming form ":unsigned char"))


(defmethod parse-form (form (tag (eql :short)))
  (register-primitive-type form))


(defmethod parse-form (form (tag (eql :unsigned-short)))
  (register-primitive-type-renaming form ":unsigned short"))


(defmethod parse-form (form (tag (eql :int)))
  (register-primitive-type form))


(defmethod parse-form (form (tag (eql :unsigned-int)))
  (register-primitive-type-renaming form ":unsigned int"))


(defmethod parse-form (form (tag (eql :long)))
  (register-primitive-type form))


(defmethod parse-form (form (tag (eql :unsigned-long)))
  (register-primitive-type-renaming form ":unsigned long"))


(defmethod parse-form (form (tag (eql :long-long)))
  (register-primitive-type-renaming form ":long long"))


(defmethod parse-form (form (tag (eql :unsigned-long-long)))
  (register-primitive-type-renaming form ":unsigned long long"))


(defmethod parse-form (form (tag (eql :float)))
  (register-primitive-type form))


(defmethod parse-form (form (tag (eql :double)))
  (register-primitive-type form))


(defmethod parse-form (form (tag (eql :long-double)))
  (register-primitive-type-renaming form ":long double"))


(defmethod parse-form (form (tag (eql :__float128)))
  (register-primitive-type-renaming form ":long double"))


(defmethod parse-form (form (tag (eql :__builtin_va_list)))
  (register-primitive-type-renaming form ":va_list"))


(defmethod parse-form (form (tag (eql :va_list)))
  (register-primitive-type-renaming form ":va_list"))


(defmethod compose-type-reference (type-group (tag (eql :void)) &rest type-args)
  (declare (ignore type-group type-args))
  (alist :tag ":void"))

;;;
;;; CONSTANTS
;;;
(defclass foreign-constant (foreign-entity)
  ((value :initform (error ":value missing") :initarg :value
          :type fixnum :reader foreign-constant-value)))


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
  entity)
