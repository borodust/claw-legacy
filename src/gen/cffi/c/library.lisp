(cl:in-package :claw.cffi.c)


(defclass cffi-generator (generator) ())


(defmethod claw.wrapper:generate-bindings ((generator (eql :claw/cffi))
                                           (language (eql :c))
                                           wrapper
                                           configuration)
  (let ((*visit-table* (make-hash-table :test 'equal))
        (*forward-declaration-table* (make-hash-table :test 'equal))
        (*export-table* (make-hash-table))
        (*entity-table* (make-hash-table :test #'equal)))
    (explode-library-definition (make-instance 'cffi-generator) language wrapper configuration)))
