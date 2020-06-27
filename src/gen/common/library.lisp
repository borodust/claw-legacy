(cl:in-package :claw.generator.common)


(defgeneric generate-binding (generator entity &key &allow-other-keys))
(defgeneric generate-forward-declaration (generator entity &key &allow-other-keys))


(defun parse-overrides (configuration)
  (loop with override-table = (make-hash-table :test 'equal)
        for (cffi-type new-type) in configuration
        do (setf (gethash cffi-type override-table) new-type)
        finally (return override-table)))


(defun find-foreign-entity (id)
  (gethash id *entity-table*))


(defgeneric find-canonical-type (object)
  (:method (object) object))


(defmethod find-canonical-type ((this claw.spec:foreign-alias))
  (find-canonical-type (claw.spec:foreign-enveloped-entity this)))


(defun find-alias-for-entity (entity)
  (loop for value being the hash-value of *entity-table*
        when (and (typep value 'claw.spec:foreign-alias)
                  (and (equal (claw.spec:foreign-entity-id (find-canonical-type value))
                              (claw.spec:foreign-entity-id entity))))
          do (return value)))


(defun explode-library-definition (generator language wrapper configuration)
  (let ((entities (claw.wrapper:wrapper-entities wrapper)))
    (destructuring-bind (&key in-package
                           symbolicate-names
                           trim-enum-prefix
                           with-adapter
                           override-types
                           recognize-bitfields
                           recognize-arrays
                           recognize-strings)
        configuration
      (let ((in-package (eval in-package))
            (*trim-enum-prefix-p* (eval trim-enum-prefix))
            (*visit-table* (make-hash-table :test 'equal))
            (*forward-declaration-table* (make-hash-table :test 'equal))
            (*adapter* (when with-adapter
                         (destructuring-bind (adapter-kind &optional adapter-path)
                             (ensure-list with-adapter)
                           (let ((adapter-path (or (eval adapter-path) "adapter.c")))
                             (ecase (eval adapter-kind)
                               (:static (make-static-adapter wrapper
                                                             adapter-path))
                               (:dynamic (make-dynamic-adapter wrapper
                                                               adapter-path)))))))
            (*export-table* (make-hash-table))
            (*entity-table* (make-hash-table :test #'equal))
            (*override-table* (parse-overrides override-types))
            (*recognize-strings-p* recognize-strings)
            (*recognize-bitfields-p* recognize-bitfields)
            (*recognize-arrays-p* recognize-arrays)
            (rename-symbols (eval (parse-renaming-pipeline symbolicate-names)))
            (bindings (list)))
        (with-symbol-renaming (in-package rename-symbols)
          (loop for entity in (stable-sort entities #'string<
                                           :key #'claw.spec:foreign-entity-id)
                do (let ((*dependency-type-list* nil))
                     (loop for bing in (generate-binding generator entity)
                           do (push bing bindings))))
          (when *adapter*
            (generate-adapter-file *adapter*))
          `(,@(nreverse bindings)
            ,@(loop for symbol being the hash-key of *export-table*
                    do (unless (keywordp symbol)
                         (unexport symbol (symbol-package symbol)))
                    collect `(export ',symbol ,(package-name (symbol-package symbol))))
            ,@(when *adapter*
                (expand-adapter-routines *adapter* wrapper))))))))


(defclass generator () ())
