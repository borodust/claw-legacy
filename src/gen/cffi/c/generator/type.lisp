(cl:in-package :claw.cffi.c)


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
