(cl:in-package :claw.spec)


;;;
;;;
;;;
(defclass foreign-alias (foreign-entity)
  ((aliased-type :initarg :aliased :initform (error ":aliased missing")
                 :reader foreign-alias-type)))


(defun find-base-alias-type (alias &optional (spec *library-specification*))
  (labels ((unwrap-alias (aliased-type)
             (let* ((inner-type (extract-entity-type aliased-type))
                    (entity (find-foreign-entity inner-type spec)))
               ;; when entity is nil - probably a forward reference - no probs
               (if (and entity (typep entity 'foreign-alias))
                   (unwrap-alias (foreign-alias-type entity))
                   inner-type))))
    (unwrap-alias (foreign-alias-type alias))))


(defun aliases-type-p (alias typespec
                       &optional (library-specification *library-specification*))
  (and (typep alias 'foreign-alias)
       (equal (find-base-alias-type alias library-specification) typespec)))


(defmethod parse-form (form (tag (eql :typedef)))
  (alist-bind (name location type) form
    (foreign-entity-type
     (register-foreign-entity name
                              (make-instance 'foreign-alias
                                             :name name
                                             :type name
                                             :aliased (parse-form type (aval :tag type))
                                             :location location)))))


(defmethod compose-form ((this foreign-alias))
  (alist :tag "typedef"
         :name (foreign-entity-name this)
         :location (foreign-entity-location this)
         :type (compose-reference (foreign-alias-type this))))


(defmethod compose-entity-reference ((this foreign-alias))
  (alist :tag (foreign-entity-name this)))
