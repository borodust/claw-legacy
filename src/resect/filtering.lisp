(cl:in-package :claw.resect)


(defun filter-library-entities (entities
                                include-definitions
                                include-sources
                                exclude-definitions
                                exclude-sources)
  (let ((inclusion-table (make-inclusion-table entities
                                               include-definitions
                                               include-sources
                                               exclude-definitions
                                               exclude-sources)))
    (loop for entity in entities
          when (marked-included-p (foreign-entity-id entity) inclusion-table)
            collect entity)))
