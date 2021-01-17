(cl:in-package :claw.resect)


(defgeneric filter-entity (entity)
  (:method (entity)
    entity))


(defmethod filter-entity ((this foreign-record))
  (setf (fields-of this) (loop for field in (fields-of this)
                               unless (explicitly-excluded-p
                                       (format nil "~A::~A"
                                               (format-full-foreign-entity-name this)
                                               (format-full-foreign-entity-name field))
                                       (format-foreign-location (foreign-entity-location this)))
                                 collect field))
  this)


(defun filter-library-entities (entities
                                include-definitions
                                include-sources
                                exclude-definitions
                                exclude-sources)
  (with-scanners (include-definitions
                  include-sources
                  exclude-definitions
                  exclude-sources)
    (let ((inclusion-table (make-inclusion-table entities)))
      (loop for entity in entities
            for filtered = (filter-entity entity)
            when (and filtered
                      (marked-included-p (foreign-entity-id entity) inclusion-table))
              collect (filter-entity entity)))))
