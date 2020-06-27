(cl:in-package :claw.cffi.c)


(declaim (special *dependency-stack*))

(defvar *dependency-type-list* nil)


(defgeneric foreign-entity-dependencies (entity)
  (:method (entity)
    (declare (ignore entity))
    nil))


(defgeneric dependable-p (entity)
  (:method (entity)
    (declare (ignore entity))
    nil))


(defun find-foreign-dependencies (entity)
  (labels ((%find-dependable (entity)
             (unless (member entity *dependency-stack*)
               (let ((*dependency-stack* (list* entity *dependency-stack*)))
                 (if (dependable-p entity)
                     entity
                     (%find-dependable (claw.spec:foreign-enveloped-entity entity)))))))
    (let (*dependency-stack*)
      (remove-duplicates
       (loop for dependency in (foreign-entity-dependencies entity)
             for dependable = (%find-dependable dependency)
             when dependable
               collect dependable)))))


(defun anonymous-p (entity)
  (not (claw.spec:foreign-entity-name entity)))


(defun entity->c (entity)
  (labels ((%name (entity)
             (if-let ((name (claw.spec:foreign-entity-name entity)))
               name
               "")))
    (typecase entity
      (claw.spec:foreign-pointer (format nil "~A*" (entity->c
                                                    (claw.spec:foreign-enveloped-entity entity))))
      (claw.spec:foreign-array (let ((enveloped-name (entity->c
                                                      (claw.spec:foreign-enveloped-entity entity))))
                                 (if-let ((dimensions (claw.spec:foreign-array-dimensions entity)))
                                   (format nil "~A~{[~A]~}" enveloped-name dimensions)
                                   (format nil "~A*" enveloped-name))))
      (claw.spec:foreign-enum (format nil "enum ~A" (%name entity)))
      (claw.spec:foreign-struct (format nil "struct ~A" (%name entity)))
      (claw.spec:foreign-union (format nil "union ~A" (%name entity)))
      (t (%name entity)))))


(defun register-visit (entity)
  (setf (gethash (claw.spec:foreign-entity-id entity) *visit-table*) entity))


(defmethod generate-binding :around ((generator cffi-generator) entity &rest args &key)
  "Generates dependency bindings first"
  (labels ((%generate-bindings ()
             (loop for dep in (find-foreign-dependencies entity)
                   append (generate-binding generator dep)))
           (%generate-depenencies ()
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
                       (apply #'generate-forward-declaration entity args))
                     (progn
                       ;; register a visit
                       (setf (gethash id *visit-table*) nil)
                       (let ((deps (%generate-bindings)))
                         (prog1 (append deps (call-next-method))
                           (register-visit entity)))))))))
    (if (typep entity 'claw.spec:foreign-constant)
        (call-next-method)
        (%generate-depenencies))))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-pointer))
  (list (claw.spec:foreign-enveloped-entity entity)))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-array))
  (list (claw.spec:foreign-enveloped-entity entity)))
