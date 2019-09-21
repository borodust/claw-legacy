(cl:in-package :claw.spec)

(declaim (special *inclusion-table*))


(defgeneric primitivep (entity)
  (:method (entity) (declare (ignore entity)) nil))


(defgeneric try-including-entity (entity))


(defun entity-explicitly-included-p (entity)
  (or (primitivep entity)
      (explicitly-included-p (foreign-entity-name entity)
                             (foreign-entity-location entity))))


(defun entity-explicitly-excluded-p (entity)
  (and (not (primitivep entity))
       (explicitly-excluded-p (foreign-entity-name entity)
                              (foreign-entity-location entity))))


(defstruct inclusion-status
  (included-p nil :read-only t)
  (excluded-p t :read-only t)
  (weakly-p t :read-only t)
  (partially-p nil :read-only t))


(defun find-inclusion-status (typespec)
  (gethash typespec *inclusion-table* (make-inclusion-status)))


(defun find-entity-inclusion-status (entity)
  (gethash (foreign-entity-type entity) *inclusion-table* (make-inclusion-status)))


(defun update-inclusion-status (typespec included-p excluded-p weakly-p partially-p)
  (let ((inclusion-status (make-inclusion-status :included-p included-p
                                                 :excluded-p excluded-p
                                                 :weakly-p weakly-p
                                                 :partially-p partially-p)))
    (setf (gethash typespec *inclusion-table*) inclusion-status)))


(defun update-entity-inclusion-status (entity included-p excluded-p weakly-p partially-p)
  (update-inclusion-status (foreign-entity-type entity)
                           included-p excluded-p weakly-p partially-p))


(defun transfer-inclusion-status (from-typespec to-typespec)
  (let ((status (find-inclusion-status from-typespec)))
    (update-inclusion-status to-typespec
                             (inclusion-status-included-p status)
                             (inclusion-status-excluded-p status)
                             (inclusion-status-weakly-p status)
                             (inclusion-status-partially-p status))))


(defun marked-strongly-excluded-p (entity)
  (let ((status (find-entity-inclusion-status entity)))
    (and (inclusion-status-excluded-p status)
         (not (inclusion-status-weakly-p status))
         (not (inclusion-status-partially-p status)))))


(defun marked-included-p (entity)
  (let ((status (find-entity-inclusion-status entity)))
    (inclusion-status-included-p status)))


(defun marked-strongly-included-p (entity)
  (let ((status (find-entity-inclusion-status entity)))
    (and (inclusion-status-included-p status)
         (not (inclusion-status-weakly-p status)))))


(defun marked-strongly-partially-included-p (entity)
  (let ((status (find-entity-inclusion-status entity)))
    (and (inclusion-status-included-p status)
         (not (inclusion-status-weakly-p status))
         (not (inclusion-status-partially-p status)))))


(defun marked-weakly-p (entity)
  (let ((status (find-entity-inclusion-status entity)))
    (inclusion-status-weakly-p status)))


(defun marked-partially-included-p (entity)
  (let ((status (find-entity-inclusion-status entity)))
    (and (not (marked-strongly-excluded-p entity))
         (inclusion-status-partially-p status))))


(defun mark-included (entity &optional strongly)
  (unless (and (marked-strongly-excluded-p entity) (not strongly))
    (update-entity-inclusion-status entity t nil (not strongly) nil)))


(defun mark-partially-included (entity &optional strongly)
  (unless (or (and (marked-strongly-excluded-p entity) (not strongly))
              (marked-included-p entity))
    (update-entity-inclusion-status entity t nil (not strongly) t)))


(defun mark-excluded (entity)
  (when (marked-weakly-p entity)
    (update-entity-inclusion-status entity nil t nil nil)))
