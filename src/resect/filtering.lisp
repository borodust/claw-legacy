(cl:in-package :claw.resect)

(declaim (special *inclusion-table*))

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
                               for field-type = (unwrap-foreign-entity field)
                               when (and (or (typep field-type 'foreign-primitive)
                                             (and (foreign-identified-p field-type)
                                                  (marked-included-p
                                                   (foreign-entity-id field-type)
                                                   *inclusion-table*)))
                                         (not (explicitly-excluded-p
                                               (format nil "~A::~A"
                                                       (entity-name-string this)
                                                       (entity-name-string field))
                                               (entity-location-string this))))
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


(defmethod filter-entity ((this foreign-alias))
  (let ((unwrapped (unwrap-foreign-entity this)))
    (unless (and (not (foreign-entity-unknown-p unwrapped))
                 (explicitly-excluded-p
                  (entity-name-string unwrapped)
                  (entity-location-string unwrapped)))
      this)))


(defun filter-library-entities (entities
                                include-definitions
                                include-sources
                                exclude-definitions
                                exclude-sources)
  (with-scanners (include-definitions
                  include-sources
                  exclude-definitions
                  exclude-sources)
    (let ((*inclusion-table* (make-inclusion-table entities)))
      (loop for entity in entities
            for filtered = (filter-entity entity)
            when (and filtered
                      (marked-included-p (foreign-entity-id entity) *inclusion-table*))
              collect filtered))))
