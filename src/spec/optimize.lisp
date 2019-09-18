(cl:in-package :claw.spec)

(declaim (special *inclusion-table*))


;;;
;;; OPTIMIZATION
;;;
(defgeneric optimize-entity (entity)
  (:method (entity)
    (when (inclusion-status-included-p (find-inclusion-status entity))
      entity)))


(defmethod optimize-entity ((entity foreign-primitive))
  entity)


(defmethod optimize-entity ((entity foreign-alias))
  (when (inclusion-status-included-p (find-inclusion-status entity))
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


(defmethod optimize-entity ((entity foreign-record))
  (let ((inclusion-status (find-inclusion-status entity)))
    (when (inclusion-status-included-p inclusion-status)
      (if (inclusion-status-weakly-p inclusion-status)
          (make-instance (class-of entity)
                         :id (foreign-entity-id entity)
                         :name (foreign-entity-name entity)
                         :location (foreign-entity-location entity)
                         :type (foreign-entity-type entity)
                         :bit-size (foreign-entity-bit-size entity)
                         :bit-alignment (foreign-entity-bit-alignment entity))
          entity))))
;;;
;;; GRAPH
;;;
(defgeneric foreign-entity-dependencies (entity)
  (:method (entity) (declare (ignore entity))))


(defun %find-dependency (typespec)
  (extract-entity-type typespec))


(defun %find-entity-dependency (entity)
  (%find-dependency (foreign-entity-type entity)))


(defmethod foreign-entity-dependencies ((type foreign-alias))
  (list (%find-dependency (foreign-alias-type type))))


(defmethod foreign-entity-dependencies ((type foreign-function))
  (list* (%find-dependency (foreign-function-return-type type))
         (mapcar #'%find-entity-dependency
                 (foreign-function-parameters type))))


(defmethod foreign-entity-dependencies ((type foreign-record))
  (mapcar #'%find-entity-dependency (foreign-record-fields type)))

;;;
;;; FILTERING
;;;
(defun entity-explicitly-included-p (entity)
  (explicitly-included-p (foreign-entity-name entity)
                         (foreign-entity-location entity)))


(defun entity-explicitly-excluded-p (entity)
  (explicitly-excluded-p (foreign-entity-name entity)
                         (foreign-entity-location entity)))


(defstruct inclusion-status
  (included-p nil :read-only t)
  (excluded-p t :read-only t)
  (weakly-p t :read-only t))


(defun find-inclusion-status (entity)
  (gethash (foreign-entity-type entity) *inclusion-table* (make-inclusion-status)))


(defun update-inclusion-status (entity included-p excluded-p weakly-p)
  (let ((inclusion-status (make-inclusion-status :included-p included-p
                                                 :excluded-p excluded-p
                                                 :weakly-p weakly-p)))
    (setf (gethash (foreign-entity-type entity) *inclusion-table*) inclusion-status)))


(defun entity-included-p (entity)
  (inclusion-status-included-p (find-inclusion-status entity)))


(defun entity-weak-p (entity)
  (inclusion-status-weakly-p (find-inclusion-status entity)))


(defun entity-excluded-p (entity)
  (inclusion-status-excluded-p (find-inclusion-status entity)))


(defun mark-included (entity)
  (update-inclusion-status entity t nil nil))


(defun transfer-inclusion-status (from to)
  (let ((status (find-inclusion-status from)))
    (update-inclusion-status to
                             (inclusion-status-included-p status)
                             (inclusion-status-excluded-p status)
                             (inclusion-status-weakly-p status))))


(defun mark-weakly-included (entity)
  (unless (entity-included-p entity)
    (update-inclusion-status entity t nil t)))


(defun mark-excluded (entity)
  (let ((status (find-inclusion-status entity)))
    (unless (and (inclusion-status-weakly-p status)
                 (inclusion-status-included-p status))
      (update-inclusion-status entity nil t nil))))


(defgeneric try-including-entity (entity)
  (:method ((entity foreign-entity))
    (when (entity-explicitly-included-p entity)
      (mark-included entity)
      t)))


(defmethod try-including-entity ((entity foreign-alias))
  (prog1 (when (entity-explicitly-included-p entity)
           (mark-included entity)
           t)
    ;; when nil - probably a forward reference
    (when-let ((dep (find-foreign-entity (find-base-alias-type entity))))
      (transfer-inclusion-status entity dep))))


(defun weakly-include-dependencies (entity)
  (loop for dep-typespec in (foreign-entity-dependencies entity)
        for dep = (find-foreign-entity dep-typespec)
        when dep
          do (if (anonymous-p dep)
                 (mark-included dep)
                 (unless (entity-included-p dep)
                   (mark-weakly-included dep)
                   (try-including-entity dep)))))


(defmethod try-including-entity ((entity foreign-record))
  (when (call-next-method)
    (weakly-include-dependencies entity)
    t))


(defmethod try-including-entity ((entity foreign-function))
  (when (call-next-method)
    (weakly-include-dependencies entity)
    t))


(defmethod try-including-entity ((entity foreign-enum))
  (if (call-next-method)
      (prog1 t
        (mark-included entity))
      (when (loop for (key . value) in (foreign-enum-values entity)
                    thereis (explicitly-included-p key (foreign-entity-location entity)))
        (mark-included entity)
        t)))


(defun make-inclusion-table (spec
                             include-definitions
                             include-sources
                             exclude-definitions
                             exclude-sources)
  (let ((*library-specification* spec)
        (*inclusion-table* (make-hash-table :test 'equal))
        (*include-definitions* include-definitions)
        (*exclude-definitions* exclude-definitions)
        (*include-sources* include-sources)
        (*exclude-sources* exclude-sources))
    (do-foreign-entities (entity spec)
      (if (entity-explicitly-excluded-p entity)
          (mark-excluded entity)
          (try-including-entity entity)))
    *inclusion-table*))


(defun optimize-specification (spec
                               include-definitions
                               include-sources
                               exclude-definitions
                               exclude-sources)
  (let* ((*library-specification* spec)
         (*inclusion-table* (make-inclusion-table *library-specification*
                                                  include-definitions
                                                  include-sources
                                                  exclude-definitions
                                                  exclude-sources))
         (optimized-spec (make-instance 'library-specification)))
    (do-foreign-entities (entity *library-specification*)
      (when-let ((optimized-entity (optimize-entity entity)))
        (let ((entity-type (foreign-entity-type optimized-entity)))
          (register-foreign-entity entity-type optimized-entity optimized-spec))))
    optimized-spec))
