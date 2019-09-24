(cl:in-package :claw.spec)


;;;
;;;
;;;
(defclass foreign-alias (foreign-entity)
  ((aliased-type :initarg :aliased :initform (error ":aliased missing")
                 :reader foreign-alias-type)))


(defun find-base-alias-type (alias &optional (spec *library-specification*))
  (labels ((unwrap-alias (aliased-type)
             (let* ((inner-type (extract-base-type aliased-type))
                    (entity (find-foreign-entity inner-type spec)))
               ;; when entity is nil - probably a forward reference - no probs
               (if (and entity (typep entity 'foreign-alias))
                   (unwrap-alias (foreign-alias-type entity))
                   inner-type))))
    (unwrap-alias (foreign-alias-type alias))))


(defmethod foreign-entity-basic-type ((this foreign-alias)
                                      &optional (spec *library-specification*))
  (find-basic-type (foreign-alias-type this) spec))


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


(defmethod foreign-entity-dependencies ((type foreign-alias))
  (cleanup-dependencies
   (list (%find-dependency (foreign-alias-type type)))))


(defmethod try-including-entity ((entity foreign-alias))
  (prog1 (call-next-method)
    (let* ((dep-typespec (find-base-alias-type entity))
           (dep-inclusion-status (find-inclusion-status dep-typespec)))
      (unless (eq dep-inclusion-status
                  (transfer-inclusion-status (foreign-entity-type entity)
                                             dep-typespec))
        (when-let ((dep (find-foreign-entity dep-typespec)))
          (try-including-entity dep))))))


(defmethod optimize-entity ((entity foreign-alias))
  (when (marked-included-p (foreign-entity-type entity))
    (labels ((unwrap-alias (entity)
               (if (typep entity 'foreign-alias)
                   (let ((aliased-type (foreign-alias-type entity)))
                     (if (atom aliased-type)
                         (unwrap-alias (find-foreign-entity aliased-type))
                         aliased-type))
                   (foreign-entity-type entity))))
      (make-instance 'foreign-alias
                     :name (foreign-entity-name entity)
                     :type (foreign-entity-name entity)
                     :aliased (unwrap-alias entity)
                     :location (foreign-entity-location entity)))))
