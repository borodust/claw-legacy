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
  (let* ((eid (foreign-entity-id this))
         (record-partially-included-p (or (not (marked-included-p eid *inclusion-table*))
                                          (marked-partially-included-p eid *inclusion-table*))))
    (setf (fields-of this) (loop for field in (fields-of this)
                                 for field-type = (unwrap-foreign-entity field)
                                 for name = (format nil "~A::~A"
                                                    (entity-name-string this)
                                                    (entity-name-string field))
                                 for location = (entity-location-string this)
                                 when (if record-partially-included-p
                                          (and (explicitly-included-p name location)
                                               (not (explicitly-excluded-p name location))
                                               (not (entity-explicitly-excluded-p field-type)))
                                          (or (not (foreign-identified-p field-type))
                                              (and (marked-included-p
                                                    (foreign-entity-id field-type)
                                                    *inclusion-table*)
                                                   (not (explicitly-excluded-p name location)))))
                                   collect field)))
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
      (filter-entity unwrapped) ; mostly to filter record fields
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
            for included-p = (marked-included-p (foreign-entity-id entity) *inclusion-table*)
            for filtered = (when included-p (filter-entity entity))
            when filtered
              collect filtered))))
