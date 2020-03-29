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


(defun find-alias-for-type (type &optional (spec *library-specification*))
  (do-foreign-entities (entity spec)
    (when (aliases-type-p entity type spec)
      (return-from find-alias-for-type entity))))


(defun register-foreign-alias (name location aliased-type)
  (register-foreign-entity name
                           (make-instance 'foreign-alias
                                          :name name
                                          :type name
                                          :aliased aliased-type
                                          :location location)))


(defmethod parse-form (form (tag (eql :typedef)))
  (alist-bind (name location type) form
    (foreign-entity-type
     (register-foreign-alias name
                             location
                             (parse-form type (aval :tag type))))))


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
  (when (call-next-method)
    (let* ((aliased-type (find-basic-type (foreign-alias-type entity)))
           (dep-typespec (first (foreign-entity-dependencies entity)))
           (old-status (find-inclusion-status dep-typespec))
           (new-status (case (first (ensure-list aliased-type))
                         ((or :array :pointer) (mark-partially-included dep-typespec))
                         (t (transfer-inclusion-status (foreign-entity-type entity)
                                                       dep-typespec)))))
      (unless (eq old-status new-status)
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

;;;
;;; RESECT
;;;
(defmethod parse-declaration ((kind (eql :typedef)) decl)
  (foreign-entity-type
   (register-foreign-alias (%resect:declaration-name decl)
                           (format-declaration-location decl)
                           (parse-type-by-category (%resect:typedef-aliased-type decl)))))


(defmethod parse-type (category (kind (eql :typedef)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type)))
