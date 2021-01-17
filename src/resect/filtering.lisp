(cl:in-package :claw.resect)


(defgeneric filter-entity (entity)
  (:method (entity)
    entity))

(defun entity-name-string (entity)
  (if (foreign-named-p entity)
      (format-full-foreign-entity-name entity)
      ""))

(defun entity-location-string (entity)
  (if (foreign-declared-p entity)
      (format-foreign-location (foreign-entity-location entity) nil)
      ""))

(defmethod filter-entity ((this foreign-record))
  (setf (fields-of this) (loop for field in (fields-of this)
                               unless (explicitly-excluded-p
                                       (format nil "~A::~A"
                                               (entity-name-string this)
                                               (entity-name-string field))
                                       (entity-location-string this))
                                 collect field))
  this)


(defmethod filter-entity ((this foreign-function))
  (unless (loop for entity in (list* (foreign-function-result-type this)
                                     (foreign-function-parameters this))
                for unwrapped = (unwrap-foreign-entity entity)
                  thereis (and (not (foreign-entity-unknown-p unwrapped))
                               (explicitly-excluded-p
                                (entity-name-string unwrapped)
                                (entity-location-string unwrapped))))
    this))


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
