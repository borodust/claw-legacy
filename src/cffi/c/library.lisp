(cl:in-package :claw.cffi.c)


(defun c-name->lisp (name)
  (when name
    (loop with string = (format nil "~A" name)
          for r in *symbol-renaming-pipeline*
          when (ppcre:scan-to-strings (car r) string)
            do (setf string (funcall (cdr r) string))
          finally (return (default-c-name-to-lisp string
                                                  (or *symbol-package* *package*))))))


(defgeneric generate-binding (entity &key &allow-other-keys))
(defgeneric generate-forward-declaration (entity &key &allow-other-keys))


(defmethod generate-binding :around (entity &rest args &key)
  "Generates dependency bindings first"
  (let* ((type (claw.spec:foreign-entity-type entity))
         (*dependency-type-list* (list* type *dependency-type-list*)))
    (multiple-value-bind (existing-type present-p) (gethash type *visit-table*)
      ;; to prevent redefinitions and stack overflow for recursive deps
      (if present-p
          ;; but if we get here through recursive definitions
          ;; and type is still not defined we need to put forward decls
          (progn
            (when (and (not existing-type)
                       (not (gethash type *forward-declaration-table*))
                       (member type (rest *dependency-type-list*) :test #'equal))
              (setf (gethash type *forward-declaration-table*) type)
              (list (apply #'generate-forward-declaration entity args))))
          (progn
            ;; register a visit
            (setf (gethash type *visit-table*) nil)
            (let ((deps (loop
                          for dep in (claw.spec:find-foreign-entity-dependencies entity
                                                                                 *spec*)
                          append (generate-binding dep))))
              (prog1 (append deps (call-next-method))
                (setf (gethash type *visit-table*) entity))))))))


(defmethod claw.wrapper:expand-library-definition ((generator (eql :cffi))
                                                   (language (eql :c))
                                                   wrapper
                                                   configuration)
  (let ((*spec* (claw.spec:find-specification-for-current-platform
                 (claw.wrapper:wrapper-specification wrapper))))
    (unless *spec*
      (error "No specification defined for current paltform ~A" (local-platform)))
    (destructuring-bind (&key in-package rename-symbols trim-enum-prefix with-adapter)
        configuration
      (let ((*symbol-renaming-pipeline* (make-scanners (eval rename-symbols)))
            (*symbol-package* (eval in-package))
            (*trim-enum-prefix-p* (eval trim-enum-prefix))
            (*visit-table* (make-hash-table :test 'equal))
            (*forward-declaration-table* (make-hash-table :test 'equal))
            (*adapter* (when with-adapter
                         (destructuring-bind (adapter-kind &optional adapter-path)
                             (ensure-list with-adapter)
                           (let ((adapter-path (or (eval adapter-path) "adapter.c")))
                             (ecase (eval adapter-kind)
                               (:static (make-static-adapter wrapper adapter-path))
                               (:dynamic (make-dynamic-adapter wrapper adapter-path)))))))
            (*export-table* (make-hash-table))
            (bindings (list)))
        (claw.spec:do-foreign-entities (entity *spec*)
          (let ((*dependency-type-list* nil))
            (loop for bing in (generate-binding entity)
                  do (push bing bindings))))
        (when *adapter*
          (generate-adapter-file *adapter*))
        `(progn
           ,@(nreverse bindings)
           ,@(loop for symbol being the hash-key of *export-table*
                   collect `(export ',symbol ,(or *symbol-package* *package*)))
           ,@(when *adapter*
               (expand-adapter-routines *adapter*)))))))
