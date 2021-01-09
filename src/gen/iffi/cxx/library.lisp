(cl:in-package :claw.iffi.cxx)


(defclass iffi-generator (generator) ())


(defmethod list-required-systems ((this iffi-generator))
  '(:cffi :iffi))


(defun fill-construction-table (entities)
  (loop for entity in entities
        when (typep entity 'claw.spec:foreign-method)
          do (let* ((owner (claw.spec:foreign-owner entity))
                    (owner-id (claw.spec:foreign-entity-id owner))
                    (owner-name (claw.spec:foreign-entity-name owner))
                    (entity-name (claw.spec:foreign-entity-name entity)))
               (when (starts-with #\~ entity-name)
                 (setf (gethash (cons owner-id :dtor) *construction-table*) entity))
               (when (string= owner-name entity-name)
                 (setf (gethash (cons owner-id :ctor) *construction-table*) entity)))))


(defun find-constructor (owner-id)
  (gethash (cons owner-id :ctor) *construction-table*))


(defun find-destructor (owner-id)
  (gethash (cons owner-id :dtor) *construction-table*))


(defmethod claw.wrapper:generate-bindings ((generator (eql :claw/iffi))
                                           (language (eql :c++))
                                           wrapper
                                           configuration)
  (let ((*qualify-records* nil)
        (*construction-table* (make-hash-table :test 'equal)))
    (fill-construction-table (claw.wrapper:wrapper-entities wrapper))
    (explode-library-definition (make-instance 'iffi-generator) language wrapper configuration)))
