(cl:in-package :claw.generator.common)


(declaim (special *dependency-stack*))

(defvar *dependency-type-list* nil)


(defgeneric foreign-entity-dependencies (entity)
  (:method (entity)
    (declare (ignore entity))
    nil))


(defgeneric dependablep (entity)
  (:method (entity)
    (declare (ignore entity))
    nil))


(defun find-foreign-dependencies (entity)
  (labels ((%find-dependable (entity)
             (unless (member entity *dependency-stack*)
               (let ((*dependency-stack* (list* entity *dependency-stack*)))
                 (cond
                   ((dependablep entity)
                    entity)
                   ((claw.spec:foreign-envelope-p entity)
                    (%find-dependable (claw.spec:foreign-enveloped-entity entity))))))))
    (let (*dependency-stack*)
      (remove-duplicates
       (loop for dependency in (foreign-entity-dependencies entity)
             for dependable = (%find-dependable dependency)
             when dependable collect dependable)))))


(defun anonymousp (entity)
  (not (claw.spec:foreign-entity-name entity)))


(defun register-visit (entity)
  (setf (gethash (claw.spec:foreign-entity-id entity) *visit-table*) entity))


(defun notice-visit (id)
  (setf (gethash id *visit-table*) nil))


(defun remove-visit (id)
  (remhash id *visit-table*))


(defmethod generate-binding :around ((generator generator) entity &rest args &key)
  "Generates dependency bindings first"
  (labels ((%generate-bindings ()
             (loop for dep in (find-foreign-dependencies entity)
                   append (call-shielded-from-unknown
                           (lambda () (generate-binding generator dep)))))
           (%generate-depenencies ()
             (check-entity-known entity)
             (let* ((id (claw.spec:foreign-entity-id entity))
                    (*dependency-type-list* (list* id *dependency-type-list*)))
               (multiple-value-bind (existing-type present-p) (gethash id *visit-table*)
                 ;; to prevent redefinitions and stack overflow for recursive deps
                 (if present-p
                     ;; but if we get here through recursive definitions
                     ;; and type is still not defined we need to put forward decls
                     (when (and (not existing-type)
                                (not (gethash id *forward-declaration-table*))
                                (member id (rest *dependency-type-list*) :test #'equal))
                       (setf (gethash id *forward-declaration-table*) id)
                       (apply #'generate-forward-declaration generator entity args))
                     (progn
                       ;; register a visit
                       (notice-visit id)
                       (let ((deps (%generate-bindings)))
                         (prog1 (append deps (call-next-method))
                           (register-visit entity)))))))))
    (unless (claw.spec:foreign-entity-private-p entity)
      (%generate-depenencies))))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-pointer))
  (list (claw.spec:foreign-enveloped-entity entity)))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-array))
  (list (claw.spec:foreign-enveloped-entity entity)))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-reference))
  (list (claw.spec:foreign-enveloped-entity entity)))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-alias))
  (list (claw.spec:foreign-enveloped-entity entity)))


(defmethod dependablep ((entity claw.spec:foreign-alias))
  (declare (ignore entity))
  t)
