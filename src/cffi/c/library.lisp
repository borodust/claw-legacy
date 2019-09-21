(cl:in-package :claw.cffi.c)


(defgeneric generate-binding (entity &key &allow-other-keys))
(defgeneric generate-forward-declaration (entity &key &allow-other-keys))
(defgeneric generate-forward-declaration-from-typespec (kind &optional name &rest opts))

(defun check-duplicates (entity)
  (flet ((%aliased-types-p (this that)
           (or (claw.spec:aliases-type-p this
                                         (claw.spec:foreign-entity-type that)
                                         *spec*)
               (claw.spec:aliases-type-p that
                                         (claw.spec:foreign-entity-type this)
                                         *spec*)))
         (%constant-p (entity)
           (typep entity 'claw.spec:foreign-constant)))
    (let ((cffi-type (entity-type->cffi entity)))
      (when-let ((existing-entity (gethash cffi-type *visit-table*)))
        (when (and (not (equal (claw.spec:foreign-entity-name existing-entity)
                               (claw.spec:foreign-entity-name entity)))
                   (or (and (%constant-p entity) (%constant-p existing-entity))
                       (and (not (%aliased-types-p entity existing-entity))
                            (not (or (%constant-p entity)
                                     (%constant-p existing-entity))))))
          (error "Entity for type ~S already registered.~&Previous:~&~A~&New:~&~A"
                 cffi-type existing-entity entity))))))


(defun parse-overrides (configuration)
  (let ((override-table (make-hash-table :test 'equal)))
    (setf (gethash :long-double override-table) 'long-double)
    (loop for (cffi-type new-type) in configuration
          do (setf (gethash cffi-type override-table) new-type)
          finally (return override-table))))


(defmethod claw.wrapper:expand-library-definition ((generator (eql :claw/cffi))
                                                   (language (eql :c))
                                                   wrapper
                                                   configuration)
  (let ((*spec* (claw.spec:find-specification-for-current-platform
                 (claw.wrapper:wrapper-specification wrapper))))
    (unless *spec*
      (error "No specification defined for current paltform ~A" (local-platform)))
    (destructuring-bind (&key in-package
                           symbolicate-names
                           trim-enum-prefix
                           with-adapter
                           recognize-strings
                           override-types)
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
            (*override-table* (parse-overrides override-types))
            (*recognize-strings-p* recognize-strings)
            (rename-symbols (eval (parse-renaming-pipeline symbolicate-names)))
            (bindings (list)))
        (with-symbol-renaming (in-package rename-symbols)
          (claw.spec:do-foreign-entities (entity *spec*)
            (let ((*dependency-type-list* nil))
              (check-duplicates entity)
              (loop for bing in (generate-binding entity)
                    do (push bing bindings))))
          (when *adapter*
            (generate-adapter-file *adapter*))
          `(progn
             ,@(nreverse bindings)
             ,@(loop for symbol being the hash-key of *export-table*
                     collect `(export ',symbol ,(package-name (symbol-package symbol))))
             ,@(when *adapter*
                 (expand-adapter-routines *adapter* wrapper))))))))
